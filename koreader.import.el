;;; koreader-import.el --- Import KOReader annotations to Org-mode -*- lexical-binding: t; -*-

(require 'org)
(require 'subr-x)

(defgroup koreader-import nil
  "Settings for importing KOReader highlights to Org-mode."
  :group 'org)

(defcustom koreader-import-target-file "~/org/books.org"
  "The Org file where books and annotations will be saved."
  :type 'file
  :group 'koreader-import)

(defcustom koreader-import-root-heading "books"
  "The top-level heading text (without stars) under which books are filed."
  :type 'string
  :group 'koreader-import)

;;; --- Lua Parser ---

(defun koreader-import--clean-lua-string (str)
  "Unescape Lua string sequences (like \\n or \\\") for Org display."
  (let ((s (replace-regexp-in-string "\\\\n" "\n" str)))
    (replace-regexp-in-string "\\\\\"" "\"" s)))

(defun koreader-import--parse-annotation-block (block)
  "Extract fields from a single Lua annotation block string."
  (let (entry)
    ;; Extract Chapter
    (when (string-match "\\[\"chapter\"\\] = \"\\(.*?\\)\"," block)
      (push `(chapter . ,(match-string 1 block)) entry))
    ;; Extract Date
    (when (string-match "\\[\"datetime\"\\] = \"\\(.*?\\)\"," block)
      (push `(datetime . ,(match-string 1 block)) entry))
    ;; Extract Note
    ;; We use a non-greedy match that accounts for escaped quotes if possible,
    ;; but for KOReader metadata simple matching usually suffices.
    (when (string-match "\\[\"note\"\\] = \"\\(.*?\\)\",[ \t]*\n" block)
      (push `(note . ,(koreader-import--clean-lua-string (match-string 1 block))) entry))
    ;; Extract Text (Highlight)
    (when (string-match "\\[\"text\"\\] = \"\\(.*?\\)\",[ \t]*\n" block)
      (push `(text . ,(koreader-import--clean-lua-string (match-string 1 block))) entry))
    entry))

(defun koreader-import--parse-metadata-file (filepath)
  "Parse the KOReader metadata.lua file and return a list of annotation alists."
  (with-temp-buffer
    (insert-file-contents filepath)
    (goto-char (point-min))
    (let ((annotations '()))
      ;; We look for the start of the annotations table
      (when (search-forward "[\"annotations\"] = {" nil t)
        ;; Iterate over numbered indices [1], [2], etc.
        (while (search-forward-regexp "\\[[0-9]+\\] = {" nil t)
          (let ((start (point))
                (end (save-excursion
                       (search-forward-regexp "^[ \t]*}," nil t)
                       (point))))
            (when (and start end)
              (push (koreader-import--parse-annotation-block (buffer-substring start end)) annotations)))))
      (nreverse annotations))))

;;; --- Path & Info Extraction ---

(defun koreader-import--get-book-info (sdr-path)
  "Extract Author, Title, and Year from the folder structure.
SDR-PATH is the full path to the .sdr directory."
  (let* ((parent-dir (directory-file-name (file-name-directory (directory-file-name sdr-path))))
         (sdr-name (file-name-nondirectory (directory-file-name sdr-path)))
         ;; Remove .sdr extension
         (base-name (string-remove-suffix ".sdr" sdr-name))
         ;; 1. Extract Author (Parent Folder)
         (author-raw (file-name-nondirectory parent-dir))
         (author (string-replace "_" " " author-raw))
         ;; 2. Extract Year (from "_(YYYY)")
         (year (if (string-match "_(\\([0-9]\\{4\\}\\))" base-name)
                   (match-string 1 base-name)
                 ""))
         ;; 3. Extract Title (Filename minus year and underscores)
         (title-raw (if (string-match "_(\\([0-9]\\{4\\}\\))" base-name)
                        (substring base-name 0 (match-beginning 0))
                      base-name))
         (title (string-replace "_" " " title-raw)))
    (list :author author :title title :year year)))

(defun koreader-import--format-org-timestamp (date-str)
  "Convert KOReader 'YYYY-MM-DD HH:MM:SS' to Org 'YYYY-MM-DD Day HH:MM'."
  ;; Parse KOReader date
  (let* ((encoded (parse-time-string date-str))
         (time (encode-time encoded)))
    (format-time-string "[%Y-%m-%d %a %H:%M]" time)))

;;; --- Org Mode Interaction ---

(defun koreader-import--ensure-target-file ()
  "Ensure the target org file exists and is open."
  (find-file koreader-import-target-file)
  (widen)
  (goto-char (point-min)))

(defun koreader-import--find-or-create-heading (title &optional level)
  "Move point to heading TITLE. Create it if missing."
  (goto-char (point-min))
  ;; Search for * Title or ** Title based on level
  (let ((stars (make-string (or level 1) ?*)))
    (if (re-search-forward (format "^%s +%s" stars (regexp-quote title)) nil t)
        (progn (beginning-of-line) t)
      ;; Not found, create it
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert (format "%s %s\n" stars title))
      (beginning-of-line 0) ;; Go back to the line we just inserted
      nil)))

(defun koreader-import--insert-book-entry (info)
  "Find or create the book entry and return the position."
  (let ((title (plist-get info :title))
        (author (plist-get info :author))
        (year (plist-get info :year)))
    
    ;; 1. Find/Create Root Heading (* books)
    (koreader-import--find-or-create-heading koreader-import-root-heading 1)
    
    ;; 2. Find/Create Book Heading (** TODO Title) within the root
    ;; We restrict search to the "books" subtree
    (let ((root-end (save-excursion (org-end-of-subtree) (point))))
      (goto-char (point-min))
      (re-search-forward (format "^\\* +%s" (regexp-quote koreader-import-root-heading)) nil t)
      
      (if (re-search-forward (format "^\\*\\* +TODO +%s" (regexp-quote title)) root-end t)
          (goto-char (match-beginning 0))
        ;; If not found, append to end of "books" tree
        (goto-char root-end)
        (unless (bolp) (insert "\n"))
        (insert (format "** TODO %s\n" title))
        (let ((props (format
                      ":PROPERTIES:\n:YEAR:      %s\n:AUTHOR:    %s\n:SCORE:     \n:TYPE:      book\n:BOOKTYPE:  \n:READTIME:  \n:END:\n"
                      year author)))
          (insert props))
        (beginning-of-line 0) ;; Move back to heading
        (forward-line -1)))))

(defun koreader-import--insert-annotations (annotations)
  "Insert annotations as subheadings under the current book item."
  (let ((book-end (save-excursion (org-end-of-subtree) (point)))
        (cnt 1))
    (dolist (ann annotations)
      (let* ((chapter (alist-get 'chapter ann))
             (datetime (alist-get 'datetime ann))
             (note (alist-get 'note ann))
             (text (alist-get 'text ann))
             (org-ts (koreader-import--format-org-timestamp datetime))
             (heading-title (format "Annotation %d%s" 
                                    cnt 
                                    (if (and chapter (not (string-empty-p chapter)))
                                        (format " (Chapter: \"%s\")" chapter)
                                      ""))))
        
        ;; Check if this specific timestamp already exists in this subtree
        (goto-char (save-excursion (org-back-to-heading t) (point)))
        (if (save-excursion 
              (re-search-forward (regexp-quote (format "CLOSED: %s" org-ts)) book-end t))
            (message "Skipping existing annotation from %s" datetime)
          
          ;; Insert new annotation
          (goto-char book-end)
          (unless (bolp) (insert "\n"))
          (insert (format "*** %s\n" heading-title))
          (insert (format "CLOSED: %s\n" org-ts))
          (when (and note (not (string-empty-p note)))
            (insert (format "%s\n" note)))
          (when (and text (not (string-empty-p text)))
            (insert "#+begin_quote\n")
            (insert text)
            (insert "\n#+end_quote\n"))
          ;; Update book-end marker for next iteration
          (setq book-end (point)))
        
        (setq cnt (1+ cnt))))))

;;; --- Main Command ---

;;;###autoload
(defun koreader-import-from-dired ()
  "Import annotations from the selected .sdr directory in Dired."
  (interactive)
  (let ((files (dired-get-marked-files)))
    (dolist (file files)
      (when (file-directory-p file)
        ;; Check for metadata file (pdf or epub or other)
        (let ((metadata-files (directory-files file t "metadata\\..*\\.lua$")))
          (if (not metadata-files)
              (message "No metadata.lua found in %s" file)
            ;; Use the first match (usually only one active metadata file)
            (let* ((meta-file (car metadata-files))
                   (info (koreader-import--get-book-info file))
                   (annotations (koreader-import--parse-metadata-file meta-file)))
              
              (if (not annotations)
                  (message "No annotations found for %s" (plist-get info :title))
                
                (koreader-import--ensure-target-file)
                (save-excursion
                  (koreader-import--insert-book-entry info)
                  (koreader-import--insert-annotations annotations))
                (save-buffer)
                (message "Imported %d annotations for '%s'" 
                         (length annotations) 
                         (plist-get info :title))))))))))

(provide 'koreader.import)
;;; koreader.import.el ends here
