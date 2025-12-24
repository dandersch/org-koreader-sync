;;; org-koreader-sync.el --- Import KOReader annotations to Org-mode -*- lexical-binding: t; -*-

(require 'org)
(require 'org-capture)
(require 'cl-lib)

(defgroup org-koreader-sync nil
  "Settings for importing KOReader annotations to Org-mode."
  :group 'org)

;;; --- Custom Variables ---
(defcustom org-koreader-sync-library-folder "~/lib/books"
  "Path to library that is (recursively) searched for *.sdr folders."
  :type 'directory
  :group 'org-koreader-sync)

(defcustom org-koreader-sync-book-target-file "~/org/books.org"
  "The file where books and annotations will be stored."
  :type 'file
  :group 'org-koreader-sync)

(defcustom org-koreader-sync-book-target-headline "Books"
  "The top-level headline under which new Books are created.
If nil, books are created at the top level of the file."
  :type '(choice (string :tag "Headline Name") (const :tag "Top Level" nil))
  :group 'org-koreader-sync)

(defcustom org-koreader-sync-status-mapping
  '(("reading"   . "TODO")
    ("complete"  . "DONE")
    ("abandoned" . "KILLED"))
  "Alist mapping KOReader status strings to Org todo keywords."
  :type '(alist :key-type string :value-type string)
  :group 'org-koreader-sync)

(defcustom org-koreader-sync-filepath-replacement-string '("/mnt/us" . "/home/da")
  "Cons cell (MATCH . REPLACEMENT) to adjust file paths from Kindle/Kobo mount points."
  :type '(cons string string)
  :group 'org-koreader-sync)

;;; --- Template Variables ---
;; We use these global variables to pass data into the capture templates dynamically.

(defvar org-koreader-context-book nil
  "Holds the alist of the book currently being processed.")

(defvar org-koreader-context-annotation nil
  "Holds the alist of the annotation currently being processed.")

;;; --- Templates ---
(defcustom org-koreader-sync-book-template
  "* %(org-koreader--get-status) %(org-koreader--get-book-prop 'title)
:PROPERTIES:
:AUTHOR:    %(org-koreader--get-book-prop 'authors)
:KO_PATH:   %(org-koreader--get-book-prop 'filepath)
:END:
"
  "Template for creating a new Book entry."
  :type 'string
  :group 'org-koreader-sync)

(defcustom org-koreader-sync-annotation-template
  "* Annotation %(org-koreader--get-annot-prop 'number) (Chapter: %(org-koreader--get-annot-prop 'chapter))
:PROPERTIES:
:CREATED: %(org-koreader--get-annot-prop 'datetime)
:END:
%(org-koreader--get-annot-prop 'note)
#+begin_quote
%(org-koreader--get-annot-prop 'text)
#+end_quote"
  "Template for creating a new Annotation entry."
  :type 'string
  :group 'org-koreader-sync)

;;; --- Lua Parsing ---
(defun org-koreader--parse-lua-string (str)
  "Basic cleanup of Lua strings (removes surrounding quotes and unescapes)."
  (if (not str) ""
    (let ((clean (replace-regexp-in-string "^\"\\|\"$" "" str)))
      (replace-regexp-in-string "\\\\\"" "\"" clean))))

(defun org-koreader--extract-lua-field (content field-name)
  "Extract a simple string field from lua table CONTENT."
  (let ((regex (format "\\[\"%s\"\\]\\s-*=\\s-*\"\\(\\(?:[^\"\\\\]\\|\\\\.\\)*\\)\"" field-name)))
    (if (string-match regex content)
        (org-koreader--parse-lua-string (match-string 1 content))
      "")))

(defun org-koreader--parse-metadata-file (filepath)
  "Parse metadata.lua at FILEPATH. Returns an alist with keys:
:book-props (alist of title, authors, status, filepath)
:annotations (list of alists for each annotation)."
  (with-temp-buffer
    (insert-file-contents filepath)
    (let ((content (buffer-string))
          (book-props nil)
          (annotations nil))

      ;; 1. Parse Book Stats
      (let* ((orig-path (org-koreader--extract-lua-field content "doc_path"))
             (fixed-path (if (and org-koreader-sync-filepath-replacement-string
                                  (string-prefix-p (car org-koreader-sync-filepath-replacement-string) orig-path))
                             (replace-regexp-in-string
                              (regexp-quote (car org-koreader-sync-filepath-replacement-string))
                              (cdr org-koreader-sync-filepath-replacement-string)
                              orig-path)
                           orig-path)))
        (setq book-props
              `((title . ,(org-koreader--extract-lua-field content "title"))
                (authors . ,(org-koreader--extract-lua-field content "authors"))
                (status . ,(org-koreader--extract-lua-field content "status"))
                (filepath . ,fixed-path))))

      ;; 2. Parse Annotations
      (goto-char (point-min))
      (while (re-search-forward "\\[\\([0-9]+\\)\\]\\s-*=\\s-*{" nil t)
        (let ((num (match-string 1))
              (start (point))
              (end (save-excursion (search-forward "}," nil t)))) ; TODO needs to find balanced } that closes [num] = {
          (when end
            (let* ((block (buffer-substring-no-properties start end))
                   (text (org-koreader--extract-lua-field block "text"))
                   (note (org-koreader--extract-lua-field block "note"))
                   (chap (org-koreader--extract-lua-field block "chapter"))
                   (date (org-koreader--extract-lua-field block "datetime")))
              (push `((number . ,num)
                      (text . ,text)
                      (note . ,note)
                      (chapter . ,chap)
                      (datetime . ,date))
                    annotations))
            (goto-char end))))
      
      (list :book-props book-props
            :annotations (nreverse annotations)))))

;;; --- Template Accessors ---
(defun org-koreader--get-book-prop (prop)
  "Helper for capture templates to access current book properties."
  (or (cdr (assoc prop org-koreader-context-book)) ""))

(defun org-koreader--get-annot-prop (prop)
  "Helper for capture templates to access current annotation properties."
  (or (cdr (assoc prop org-koreader-context-annotation)) ""))

(defun org-koreader--get-status ()
  "Return the mapped TODO keyword for the current book."
  (let* ((status (org-koreader--get-book-prop 'status))
         (mapped (assoc status org-koreader-sync-status-mapping)))
    (if mapped (cdr mapped) "")))

;;; --- Capture Targeting ---
(defun org-koreader--find-book-entry ()
  "Target function for org-capture.
Finds the headline in `org-koreader-sync-book-target-file` matching
the current book title. Moves point there."
  (let ((title (org-koreader--get-book-prop 'title))
        (target-file (expand-file-name org-koreader-sync-book-target-file)))
    (unless title (error "No book title found in context"))
    (find-file target-file)
    (goto-char (point-min))
    (let ((found nil))
      (org-map-entries
       (lambda ()
         (when (string= (org-get-heading t t t t) title)
           (setq found (point))))
       nil 'file)
      (if found
          (goto-char found)
        (error "Book entry '%s' not found for annotation capture" title)))))

(defun org-koreader--exists-p (title &optional annotation-timestamp)
  "Check if an entry exists in the target file.
If ANNOTATION-TIMESTAMP is nil, looks for a Book Headline with TITLE.
If ANNOTATION-TIMESTAMP is set, looks for that Property *inside* the Book subtree."
  (let ((target-file (expand-file-name org-koreader-sync-book-target-file))
        (exists nil))
    (if (file-exists-p target-file)
        (with-current-buffer (find-file-noselect target-file)
          (save-excursion
            (org-map-entries
             (lambda ()
               (when (string= (org-get-heading t t t t) title)
                 (if (not annotation-timestamp)
                     (setq exists t)
                   (let ((limit (save-excursion (org-end-of-subtree) (point))))
                     (save-restriction
                       (narrow-to-region (point) limit)
                       (org-map-entries
                        (lambda ()
                          (let ((props (org-entry-properties)))
                            (when (string= (cdr (assoc "CREATED" props)) annotation-timestamp)
                              (message "annot with ts %s in %s already exists" annotation-timestamp target-file)
                              (setq exists t))))
                        nil 'file))))))
             nil 'file)))
      nil)
    exists))

;;; --- Main Logic ---
;;;###autoload
(defun org-koreader-sync ()
  "Sync KOReader metadata.lua files from library to Org-mode."
  (interactive)
  (let* ((files (directory-files-recursively org-koreader-sync-library-folder "metadata\\..+\\.lua$"))
         (new-books 0)
         (new-notes 0))
    
    (dolist (file files)
      (let* ((data (org-koreader--parse-metadata-file file))
             (book (plist-get data :book-props))
             (annots (plist-get data :annotations))
             (title (cdr (assoc 'title book))))

        (when (and title (not (string-empty-p title)))
          ;; 1. Set Context
          (setq org-koreader-context-book book)

          ;; 2. Sync Book
          (unless (org-koreader--exists-p title)
            (message "Creating book entry: %s" title)
            (let ((org-capture-templates
                   `(("k" "KOReader Book" entry
                      (file+headline ,org-koreader-sync-book-target-file ,org-koreader-sync-book-target-headline)
                      ,org-koreader-sync-book-template
                      :immediate-finish t))))
              (org-capture nil "k")
              ;; Force save to ensure `org-map-entries` sees the new book immediately
              (with-current-buffer (find-buffer-visiting (expand-file-name org-koreader-sync-book-target-file))
                (save-buffer)))
              (cl-incf new-books)))

          ;; 3. Sync Annotations
          (dolist (annot annots)
            (setq org-koreader-context-annotation annot)
            (let ((timestamp (cdr (assoc 'datetime annot))))
              (message "checkin for new annotation, datetime: %s" timestamp)
              (unless (org-koreader--exists-p title timestamp)
                (let ((org-capture-templates
                       `(("n" "KOReader Note" entry
                          (file+function ,org-koreader-sync-book-target-file org-koreader--find-book-entry)
                          ,org-koreader-sync-annotation-template
                          :immediate-finish t))))
                  (org-capture nil "n"))
                (cl-incf new-notes))))))
    (message "Sync complete. Imported %d new books and %d new annotations." new-books new-notes)))

(provide 'org-koreader-sync)
;;; org-koreader-sync.el ends here
