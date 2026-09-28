;;; org-koreader-sync.el --- Sync KOReader metadata to Org -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "27.1") (org "9.4"))

;;; Commentary:
;; Run `org-koreader-sync' to import adjacent EPUB/PDF sidecars.  KO_MD5
;; identifies books independently of device paths.  Only imported annotations
;; are updated; missing annotations and completed books are retained.
;; Lua files are parsed as data, never executed.
;;
;; Configure `org-koreader-sync-library-folder',
;; `org-koreader-sync-book-target-file', and
;; `org-koreader-sync-book-target-headline', then M-x org-koreader-sync.
;; A nil target heading means top-level entries.  The status mapping uses
;; TODO, DONE, and KILLED; configure these in Org or customize the mapping.
;;
;; Books use KOReader's partial_md5_checksum, not a title or device path.
;; KO_PATH is no longer stored and is removed from matched books on sync.
;; The local filepath remains available to custom templates.  The old
;; org-koreader-sync-filepath-replacement-string setting is no longer used.
;; Back up old imports before removing/reimporting them: manually written
;; notes cannot be reconstructed from KOReader.  There is no automatic
;; migration of entries without KO_MD5.  Missing checksums are reported;
;; reopening the book in KOReader normally creates one.
;;
;; Existing titles, AUTHOR, YEAR, TYPE, other properties, and Notes children
;; are preserved.  Missing AUTHOR/YEAR are filled from metadata; new books
;; default to TYPE book.  SCORE follows summary.rating when present, and is
;; retained when absent.  YEAR accepts explicit doc_props.year or
;; doc_props.publication_date (YYYY or YYYY-MM-DD).  Stock KOReader normally
;; supplies neither.  READTIME is not inferred from legacy sidecar stats.
;;
;; A complete status sets the mapped DONE keyword and a date-only CLOSED
;; from summary.modified.  Existing completion dates and states stay closed.
;; Missing completion dates are reported, never replaced by today's date.
;; KOReader timestamps have no timezone; their wall-clock values are kept.
;;
;; Annotation identity combines the creation second and source position.
;; Newer datetime_updated values replace imported heading/body content.
;; Unique creation times also match moved highlight boundaries.  Children,
;; tags, and extra properties survive updates.  Locally edited annotation
;; content and equal-timestamp source conflicts are preserved with warnings.
;; Keep KO_* properties intact: they record identity and update provenance.
;;
;; Only exact metadata.epub.lua/metadata.pdf.lua names in .sdr directories
;; are read; backups and Syncthing conflict files are ignored.  Malformed
;; files are reported and skipped atomically.  Let Syncthing settle first:
;; this importer does not resolve conflicts between device sidecars.  A
;; sampled MD5 identifies copies, not different editions or converted formats.
;;
;; Saving is immediate and includes existing unsaved edits in the destination
;; buffer.  No source files are written.  Templates support %(EXPRESSION)
;; only, not interactive capture prompts; identity does not depend on them.
;; Run regression tests with:
;; emacs -Q --batch -L . -l tests/org-koreader-sync-test.el \
;;   -f ert-run-tests-batch-and-exit

;;; Code:

(require 'org)
(require 'org-capture)
(require 'cl-lib)
(require 'subr-x)

(defgroup org-koreader-sync nil
  "Import KOReader metadata into Org."
  :group 'org)

(defcustom org-koreader-sync-library-folder "~/lib/books"
  "Library recursively searched for adjacent EPUB/PDF .sdr directories."
  :type 'directory
  :group 'org-koreader-sync)

(defcustom org-koreader-sync-book-target-file "~/org/books.org"
  "Org file to update and save immediately."
  :type 'file
  :group 'org-koreader-sync)

(defcustom org-koreader-sync-book-target-headline "Books"
  "Top-level heading for imported books, created if absent.
If nil, insert books at the top level.  Duplicate target headings are errors."
  :type '(choice string (const nil))
  :group 'org-koreader-sync)

(defcustom org-koreader-sync-status-mapping
  '(("reading" . "TODO") ("complete" . "DONE") ("abandoned" . "KILLED"))
  "KOReader status to Org keyword mapping.
Keywords used must be configured in the destination file.  Completed books
are never reopened.  Unknown statuses leave the Org state unchanged."
  :type '(alist :key-type string :value-type string)
  :group 'org-koreader-sync)

(defvar org-koreader-context-book nil
  "Book alist available to template expressions.")
(defvar org-koreader-context-annotation nil
  "Annotation alist available to template expressions.")
(defvar org-koreader--warnings nil
  "Warnings collected during the current sync.")

(defcustom org-koreader-sync-book-template
  "* %(org-koreader--get-status) %(org-koreader--get-book-prop 'title)\n"
  "Template used only when creating a book.
Supports Org capture %(EXPRESSION) expansion, not interactive placeholders.
It must produce exactly one heading.  Identity, status, and metadata
properties are managed independently of the template."
  :type 'string
  :group 'org-koreader-sync)

(defcustom org-koreader-sync-annotation-template
  "* Annotation %(org-koreader--get-annot-prop 'number) (Chapter: \"%(org-koreader--get-annot-prop 'chapter)\")
%(org-koreader--get-annot-prop 'note)
#+begin_quote
%(org-koreader--get-annot-prop 'text)
#+end_quote
"
  "Template for an imported annotation's heading and body.
Supports %(EXPRESSION), as in `org-koreader-sync-book-template'.
Must produce exactly one heading.  CREATED and identity properties are
always supplied by the importer.  Source text is escaped by the accessors
so that it cannot introduce headings or terminate the quote block."
  :type 'string
  :group 'org-koreader-sync)

;;; Data-only Lua reader

(defun org-koreader--lua-long-string ()
  "Read a Lua long string at point, returning its raw bytes."
  (unless (looking-at "\\[\\(=*\\)\\[") (error "Expected long string"))
  (let ((close (concat "]" (match-string 1) "]")))
    (goto-char (match-end 0))
    (when (looking-at "\r?\n\\|\r") (goto-char (match-end 0)))
    (let ((start (point)))
      (unless (search-forward close nil t) (error "Unterminated long string"))
      (replace-regexp-in-string
       "\r\n?" "\n" (buffer-substring-no-properties start (- (point) (length close))) t t))))

(defun org-koreader--lua-space ()
  "Skip Lua whitespace and comments."
  (skip-chars-forward " \t\r\n\f\v")
  (while (looking-at "--")
    (forward-char 2)
    (if (looking-at "\\[=*\\[")
        (org-koreader--lua-long-string)
      (forward-line 1))
    (skip-chars-forward " \t\r\n\f\v")))

(defun org-koreader--lua-expect (token)
  "Consume TOKEN after whitespace, or signal an error."
  (org-koreader--lua-space)
  (unless (looking-at (regexp-quote token))
    (error "Expected %s at byte %d" token (point)))
  (forward-char (length token)))

(defun org-koreader--lua-string ()
  "Read a quoted Lua string, decoding escapes before UTF-8."
  (let ((quote (char-after)) bytes done)
    (forward-char)
    (while (not done)
      (let ((c (char-after)))
        (unless c (error "Unterminated Lua string"))
        (forward-char)
        (cond
         ((= c quote) (setq done t))
         ((memq c '(?\n ?\r)) (error "Unescaped newline in Lua string"))
         ((/= c ?\\) (push c bytes))
         (t
          (let ((escape (char-after)))
            (unless escape (error "Unterminated Lua escape"))
            (forward-char)
            (cond
             ((assq escape '((?a . 7) (?b . 8) (?f . 12) (?n . 10)
                             (?r . 13) (?t . 9) (?v . 11)))
              (push (cdr (assq escape '((?a . 7) (?b . 8) (?f . 12) (?n . 10)
                                        (?r . 13) (?t . 9) (?v . 11)))) bytes))
             ((memq escape '(?\\ ?\" ?\')) (push escape bytes))
             ((memq escape '(?\n ?\r))
              (when (and (= escape ?\r) (eq (char-after) ?\n)) (forward-char))
              (push ?\n bytes))
             ((= escape ?z) (skip-chars-forward " \t\r\n\f\v"))
             ((= escape ?x)
              (unless (looking-at "[[:xdigit:]]\\{2\\}") (error "Invalid hex escape"))
              (push (string-to-number (match-string 0) 16) bytes)
              (goto-char (match-end 0)))
             ((and (>= escape ?0) (<= escape ?9))
              (backward-char)
              (looking-at "[0-9]\\{1,3\\}")
              (let ((byte (string-to-number (match-string 0))))
                (when (> byte 255) (error "Lua byte escape exceeds 255"))
                (push byte bytes))
              (goto-char (match-end 0)))
             (t (error "Unsupported Lua escape: %c" escape))))))))
    (decode-coding-string (apply #'unibyte-string (nreverse bytes)) 'utf-8)))

(defun org-koreader--lua-table ()
  "Read a Lua table as an equal-tested hash table."
  (org-koreader--lua-expect "{")
  (let ((table (make-hash-table :test #'equal)) (index 0))
    (org-koreader--lua-space)
    (while (not (eq (char-after) ?}))
      (let (key value)
        (cond
         ((and (eq (char-after) ?\[) (not (looking-at "\\[=*\\[")))
          (forward-char)
          (setq key (org-koreader--lua-value))
          (unless (or (stringp key) (numberp key)) (error "Invalid Lua table key"))
          (org-koreader--lua-expect "]")
          (org-koreader--lua-expect "=")
          (setq value (org-koreader--lua-value)))
         ((looking-at "\\([A-Za-z_][A-Za-z_0-9]*\\)[ \t\r\n]*=")
          (setq key (match-string 1))
          (goto-char (match-end 0))
          (setq value (org-koreader--lua-value)))
         (t (setq key (cl-incf index) value (org-koreader--lua-value))))
        (when (gethash key table) (error "Duplicate Lua table key: %s" key))
        (puthash key value table))
      (org-koreader--lua-space)
      (cond ((memq (char-after) '(?, ?\;)) (forward-char))
            ((not (eq (char-after) ?})) (error "Expected table separator at %d" (point))))
      (org-koreader--lua-space))
    (forward-char)
    table))

(defun org-koreader--lua-value ()
  "Read one Lua data value, rejecting executable expressions."
  (org-koreader--lua-space)
  (cond
   ((eq (char-after) ?{) (org-koreader--lua-table))
   ((memq (char-after) '(?\" ?\')) (org-koreader--lua-string))
   ((looking-at "\\[=*\\[")
    (decode-coding-string (org-koreader--lua-long-string) 'utf-8))
   ((looking-at "-?0[xX][[:xdigit:]]+")
    (let* ((s (match-string 0)) (negative (string-prefix-p "-" s)))
      (goto-char (match-end 0))
      (* (if negative -1 1) (string-to-number (substring s (if negative 3 2)) 16))))
   ((looking-at "[-+]?\\(?:[0-9]+\\(?:\\.[0-9]*\\)?\\|\\.[0-9]+\\)\\(?:[eE][-+]?[0-9]+\\)?")
    (prog1 (string-to-number (match-string 0)) (goto-char (match-end 0))))
   ((looking-at "\\(?:true\\|false\\|nil\\)\\_>")
    (let ((s (match-string 0)))
      (goto-char (match-end 0))
      (cond ((equal s "true") t) ((equal s "false") :false) (t nil))))
   (t (error "Unsupported Lua value at byte %d" (point)))))

(defun org-koreader--read-lua (file)
  "Read FILE containing only `return' and a Lua table."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (goto-char (point-min))
    (org-koreader--lua-expect "return")
    (let ((data (org-koreader--lua-table)))
      (org-koreader--lua-space)
      (when (eq (char-after) ?\;) (forward-char) (org-koreader--lua-space))
      (unless (eobp) (error "Unexpected data after Lua table"))
      data)))

;;; Metadata and templates

(defun org-koreader--field (table field)
  "Return FIELD from TABLE, or nil for an absent table."
  (and (hash-table-p table) (gethash field table)))

(defun org-koreader--text (value)
  "Return nonempty string VALUE, or nil."
  (and (stringp value) (not (string-empty-p (string-trim value))) value))

(defun org-koreader--one-line (value)
  "Format VALUE for an Org heading or property."
  (string-trim (replace-regexp-in-string "[\n\r\t]+" " " (format "%s" (or value "")))))

(defun org-koreader--timestamp (value)
  "Format a KOReader date or datetime VALUE as an inactive Org timestamp.
Preserve the source's wall-clock date and time; no timezone is supplied by
KOReader.  Reject invalid dates rather than substituting the import date."
  (unless (and (stringp value)
               (string-match
                "\\`\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)\\(?: \\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\)\\)?\\'"
                value))
    (error "Invalid KOReader date: %S" value))
  (let* ((parts (mapcar (lambda (n) (string-to-number (or (match-string n value) "0")))
                        '(1 2 3 4 5 6)))
         (time (encode-time (nth 5 parts) (nth 4 parts) (nth 3 parts)
                            (nth 2 parts) (nth 1 parts) (nth 0 parts) t))
         (with-time (> (length value) 10))
         (system-time-locale "C"))
    (unless (equal value (format-time-string
                          (if with-time "%Y-%m-%d %H:%M:%S" "%Y-%m-%d") time t))
      (error "Invalid calendar date: %s" value))
    (format-time-string (if with-time "[%Y-%m-%d %a %H:%M]" "[%Y-%m-%d %a]") time t)))

(defun org-koreader--canonical (value)
  "Return VALUE with Lua table keys in a stable order for identity hashing."
  (if (not (hash-table-p value)) value
    (let (pairs)
      (maphash (lambda (k v) (push (cons k (org-koreader--canonical v)) pairs)) value)
      (sort pairs (lambda (a b) (string< (prin1-to-string (car a)) (prin1-to-string (car b))))))))

(defun org-koreader--annotation (number table)
  "Normalize annotation TABLE at NUMBER.  Return nil for empty bookmarks."
  (let ((print-length nil)
        (print-level nil)
        (text (org-koreader--text (org-koreader--field table "text")))
        (note (org-koreader--text (org-koreader--field table "note"))))
    (when (or text note)
      (let* ((created (org-koreader--field table "datetime"))
             (updated (or (org-koreader--field table "datetime_updated") created))
             (stamp (org-koreader--timestamp created))
             (position (mapcar (lambda (k) (org-koreader--canonical (org-koreader--field table k)))
                               '("page" "pos0" "pos1")))
             (id (secure-hash 'sha256 (prin1-to-string (cons created position)))))
        (unless (and (= (length created) 19) (= (length updated) 19))
          (error "Annotation timestamps need second precision"))
        (org-koreader--timestamp updated)
        `((id . ,id) (number . ,number) (created . ,created) (updated . ,updated)
          (datetime . ,stamp) (text . ,text) (note . ,note)
          (source-hash . ,(secure-hash 'sha256
                                       (prin1-to-string (list text note (org-koreader--field table "chapter")))))
          (chapter . ,(org-koreader--field table "chapter")))))))

(defun org-koreader--parse-metadata-file (file)
  "Parse sidecar FILE into :book-props and :annotations."
  (let* ((data (org-koreader--read-lua file))
         (props (org-koreader--field data "doc_props"))
         (stats (org-koreader--field data "stats"))
         (summary (org-koreader--field data "summary"))
         (md5 (org-koreader--field data "partial_md5_checksum"))
         (extension (cadr (split-string (file-name-nondirectory file) "\\.")))
         (path (concat (file-name-sans-extension
                        (directory-file-name (file-name-directory (expand-file-name file))))
                       "." extension))
         (title (or (org-koreader--text (org-koreader--field props "title"))
                    (org-koreader--text (org-koreader--field stats "title"))
                    (file-name-base path)))
         (year (or (org-koreader--field props "year")
                   (org-koreader--field props "publication_date")))
         (rating (org-koreader--field summary "rating"))
         (status (org-koreader--field summary "status"))
         (modified (org-koreader--text (org-koreader--field summary "modified")))
         annotations)
    (unless (and (stringp md5) (string-match-p "\\`[[:xdigit:]]\\{32\\}\\'" md5))
      (error "Missing or invalid partial_md5_checksum; open the book in KOReader first"))
    (when (and (equal status "complete") modified) (org-koreader--timestamp modified))
    (setq year (and year (string-match "\\`\\([0-9]\\{4\\}\\)\\(?:\\'\\|[-/]\\)" (format "%s" year))
                    (match-string 1 (format "%s" year))))
    (let ((table (org-koreader--field data "annotations")))
      (when table
        (unless (hash-table-p table) (error "Annotations must be a table"))
        (maphash
         (lambda (n a)
           (unless (and (integerp n) (> n 0) (hash-table-p a))
             (error "Invalid annotation entry: %S" n))
           (when-let ((annot (org-koreader--annotation n a))) (push annot annotations))) table)))
    (list :book-props
          `((md5 . ,(downcase md5)) (title . ,title)
            (authors . ,(or (org-koreader--text (org-koreader--field props "authors"))
                            (org-koreader--text (org-koreader--field stats "authors"))))
            (status . ,status) (modified . ,modified) (filepath . ,path) (year . ,year)
            (score . ,(and (numberp rating) (<= 1 rating 5) rating)))
          :annotations (sort annotations (lambda (a b) (< (alist-get 'number a) (alist-get 'number b)))))))

(defun org-koreader--get-book-prop (prop)
  "Get PROP from the current book for a template."
  (org-koreader--one-line (alist-get prop org-koreader-context-book)))

(defun org-koreader--get-annot-prop (prop)
  "Get PROP from the current annotation, escaping Org structural lines."
  (let ((value (alist-get prop org-koreader-context-annotation)))
    (if (memq prop '(text note))
        (org-escape-code-in-string (or value ""))
      (org-koreader--one-line value))))

(defun org-koreader--get-status ()
  "Get the configured Org keyword for the current book."
  (or (cdr (assoc (alist-get 'status org-koreader-context-book) org-koreader-sync-status-mapping)) ""))

(defun org-koreader--render (template)
  "Expand TEMPLATE expressions once and validate a single Org entry."
  (with-temp-buffer
    (insert template)
    ;; Mark expressions before expansion: returned source text is never evaluated.
    (org-capture-expand-embedded-elisp t)
    (org-capture-expand-embedded-elisp)
    (goto-char (point-min))
    (unless (looking-at "\\*+ ") (error "Template must start with an Org heading"))
    (forward-line)
    (when (re-search-forward org-heading-regexp nil t)
      (error "Template must produce exactly one heading"))
    (concat (string-trim-right (buffer-string)) "\n")))

;;; Org updates

(defun org-koreader--warn (format-string &rest args)
  "Collect a warning using FORMAT-STRING and ARGS."
  (push (apply #'format format-string args) org-koreader--warnings))

(defun org-koreader--target ()
  "Find or create the target heading.  Return its position, or nil."
  (when org-koreader-sync-book-target-headline
    (let (matches)
      (org-map-entries
       (lambda ()
         (when (and (= (org-outline-level) 1)
                    (equal (org-get-heading t t t t) org-koreader-sync-book-target-headline))
           (push (point) matches))) nil nil)
      (when (cdr matches) (error "Multiple top-level target headings: %s" org-koreader-sync-book-target-headline))
      (or (car matches)
          (progn
            (goto-char (point-max))
            (unless (bolp) (insert "\n"))
            (let ((start (point)))
              (insert "* " (org-koreader--one-line org-koreader-sync-book-target-headline) "\n")
              start))))))

(defun org-koreader--find-property (property value)
  "Find unique PROPERTY VALUE within the current restriction."
  (let (matches)
    (org-map-entries
     (lambda () (when (equal (org-entry-get nil property) value) (push (point) matches))) nil nil)
    (when (cdr matches) (error "Ambiguous %s: %s" property value))
    (car matches)))

(defun org-koreader--insert-entry (parent rendered)
  "Insert RENDERED as a child of PARENT, or at top level if nil."
  (let ((level (if parent (progn (goto-char parent) (1+ (org-outline-level))) 1)))
    (if parent (org-end-of-subtree t t) (goto-char (point-max)))
    (unless (bolp) (insert "\n"))
    (let ((start (point)))
      (insert (replace-regexp-in-string "\\`\\*+" (make-string level ?*) rendered))
      (goto-char start)
      start)))

(defun org-koreader--put (property value &optional only-if-empty)
  "Store nonempty VALUE in PROPERTY, optionally ONLY-IF-EMPTY."
  (when (and value (not (equal value ""))
             (or (not only-if-empty) (not (org-entry-get nil property))))
    (let ((text (org-koreader--one-line value)))
      (unless (equal text (org-entry-get nil property)) (org-entry-put nil property text)))))

(defun org-koreader--update-book ()
  "Update metadata at the current book heading, preserving completion."
  (let* ((book org-koreader-context-book)
         (state (org-koreader--get-status))
         (complete (equal (alist-get 'status book) "complete"))
         (closed (org-entry-get nil "CLOSED"))
         (done (or closed (and (org-get-todo-state)
                               (equal (org-get-todo-state)
                                      (cdr (assoc "complete" org-koreader-sync-status-mapping)))))))
    (unless (or (string-empty-p state) done)
      (unless (member state org-todo-keywords-1)
        (error "Configure Org TODO keyword %s in the destination file" state))
      (unless (equal state (org-get-todo-state))
        (let ((org-inhibit-logging t) (org-inhibit-blocking t)) (org-todo state))))
    (when (and complete (not closed))
      (if-let ((date (alist-get 'modified book)))
          (let ((org-log-done-with-time nil) (system-time-locale "C"))
            (org-add-planning-info 'closed (substring (org-koreader--timestamp date) 1 -1)))
        (org-koreader--warn "No completion date for %s; left CLOSED unset" (alist-get 'title book))))
    (org-koreader--put "KO_MD5" (alist-get 'md5 book))
    (when (org-entry-get nil "KO_PATH") (org-entry-delete nil "KO_PATH"))
    (org-koreader--put "AUTHOR" (alist-get 'authors book) t)
    (org-koreader--put "TYPE" "book" t)
    (org-koreader--put "YEAR" (alist-get 'year book) t)
    (org-koreader--put "SCORE" (alist-get 'score book))))

(defun org-koreader--body-region ()
  "Return bounds of this entry's body, excluding properties and children."
  (save-excursion
    (org-end-of-meta-data)
    (cons (point) (progn (outline-next-heading) (point)))))

(defun org-koreader--content-hash ()
  "Hash this entry's heading and body, excluding properties, tags, and children."
  (let ((body (org-koreader--body-region)))
    (secure-hash 'sha256
                 (concat (org-get-heading t t t t) "\n"
                         (string-trim (buffer-substring-no-properties (car body) (cdr body)))))))

(defun org-koreader--replace-annotation (rendered)
  "Replace this annotation's heading/body with RENDERED, keeping its children."
  (let* ((parts (with-temp-buffer
                  (insert rendered)
                  (org-mode)
                  (goto-char (point-min))
                  (let ((heading (org-get-heading t t t t)) (body (org-koreader--body-region)))
                    (cons heading (buffer-substring-no-properties (car body) (cdr body))))))
         (body (org-koreader--body-region)))
    (save-excursion
      (goto-char (car body))
      (delete-region (car body) (cdr body))
      (insert (cdr parts)))
    (org-edit-headline (car parts))))

(defun org-koreader--sync-annotations (annotations)
  "Sync ANNOTATIONS below the current book.  Return (NEW UPDATED)."
  (let ((book (point-marker))
        (ids (make-hash-table :test #'equal))
        (dates (make-hash-table :test #'equal))
        (source-dates (make-hash-table :test #'equal))
        (new 0) (updated 0))
    (org-map-entries
     (lambda ()
       (when-let ((id (org-entry-get nil "KO_ID")))
         (when (gethash id ids) (error "Duplicate annotation KO_ID: %s" id))
         (let ((marker (copy-marker (point) t)) (date (org-entry-get nil "KO_CREATED")))
           (puthash id marker ids)
           (puthash date (cons marker (gethash date dates)) dates)))) nil 'tree)
    (dolist (a annotations)
      (let ((date (alist-get 'created a))) (puthash date (1+ (gethash date source-dates 0)) source-dates)))
    (dolist (org-koreader-context-annotation annotations)
      (let* ((a org-koreader-context-annotation)
             (id (alist-get 'id a)) (date (alist-get 'created a))
             (same-date (gethash date dates))
             ;; Moving highlight boundaries changes its position.  Creation time
             ;; can still identify it, but only if unique on both sides.
             (existing (or (gethash id ids)
                           (and (= (gethash date source-dates) 1)
                                (= (length same-date) 1) (car same-date)))))
        (if existing (goto-char existing)
          (goto-char (org-koreader--insert-entry book (org-koreader--render org-koreader-sync-annotation-template)))
          (cl-incf new))
        (let ((version (org-entry-get nil "KO_UPDATED")))
          (when (and existing (equal version (alist-get 'updated a))
                     (not (equal (org-entry-get nil "KO_SOURCE_HASH") (alist-get 'source-hash a))))
            (org-koreader--warn "Conflicting annotation at the same timestamp: %s / %s; kept Org copy"
                                (org-koreader--get-book-prop 'title) date))
          (when (or (not existing) (not version) (string< version (alist-get 'updated a)))
            (if (and existing
                     (not (equal (org-entry-get nil "KO_CONTENT_HASH") (org-koreader--content-hash))))
                (org-koreader--warn "Preserved locally edited annotation: %s / %s"
                                    (org-koreader--get-book-prop 'title) date)
              (when existing
                (org-koreader--replace-annotation (org-koreader--render org-koreader-sync-annotation-template))
                (remhash (org-entry-get nil "KO_ID") ids)
                (cl-incf updated))
              (org-koreader--put "KO_ID" id)
              (org-koreader--put "KO_CREATED" date)
              (org-koreader--put "KO_UPDATED" (alist-get 'updated a))
              (org-koreader--put "CREATED" (alist-get 'datetime a))
              (org-koreader--put "KO_SOURCE_HASH" (alist-get 'source-hash a))
              (org-koreader--put "KO_CONTENT_HASH" (org-koreader--content-hash))
              (let ((marker (or existing (copy-marker (point) t))))
                (puthash id marker ids)
                (unless existing (puthash date (cons marker same-date) dates))))))))
    (set-marker book nil)
    (maphash (lambda (_ marker) (set-marker marker nil)) ids)
    (list new updated)))

(defun org-koreader--sync-book (data)
  "Apply DATA to the current Org buffer.  Return (NEW-BOOKS NEW-NOTES UPDATED)."
  (let* ((org-koreader-context-book (plist-get data :book-props))
         (target (org-koreader--target))
         (new 0))
    (save-restriction
      (when target (goto-char target) (org-narrow-to-subtree))
      (let ((existing (org-koreader--find-property "KO_MD5" (alist-get 'md5 org-koreader-context-book))))
        (if existing (goto-char existing)
          (let ((state (org-koreader--get-status)))
            (unless (or (string-empty-p state) (member state org-todo-keywords-1))
              (error "Configure Org TODO keyword %s in the destination file" state)))
          (goto-char (org-koreader--insert-entry target (org-koreader--render org-koreader-sync-book-template)))
          (setq new 1))
        (org-koreader--update-book)
        (org-back-to-heading t)
        (cons new (org-koreader--sync-annotations (plist-get data :annotations)))))))

;;;###autoload
(defun org-koreader-sync ()
  "Sync EPUB/PDF sidecars to Org and save the destination immediately.
Retain completed books, missing annotations, and locally edited annotation
content.  Report malformed files without importing partial data.  Return a
plist of counts and warnings.  No KOReader files are modified."
  (interactive)
  (let* ((case-fold-search nil)
         (files (cl-remove-if-not
                 (lambda (f) (string-suffix-p ".sdr" (directory-file-name (file-name-directory f))))
                 (directory-files-recursively
                  (expand-file-name org-koreader-sync-library-folder) "\\`metadata\\.\\(?:epub\\|pdf\\)\\.lua\\'")))
         (org-koreader--warnings nil)
         (new-books 0) (new-notes 0) (updated 0) (skipped 0)
         (target (find-file-noselect (expand-file-name org-koreader-sync-book-target-file))))
    (with-current-buffer target
      (unless (derived-mode-p 'org-mode) (error "Destination must use Org mode"))
      (save-excursion
        (save-restriction
          (widen)
          (dolist (file (sort files #'string<))
            (condition-case err
                (let* ((data (org-koreader--parse-metadata-file file))
                       (counts (atomic-change-group (org-koreader--sync-book data))))
                  (cl-incf new-books (nth 0 counts))
                  (cl-incf new-notes (nth 1 counts))
                  (cl-incf updated (nth 2 counts)))
              (error
               (cl-incf skipped)
               (org-koreader--warn "%s: %s" file (error-message-string err)))))
          (when (buffer-modified-p) (save-buffer)))))
    (dolist (warning (reverse org-koreader--warnings))
      (display-warning 'org-koreader-sync warning :warning))
    (message "KOReader: %d new books, %d new annotations, %d updated annotations, %d skipped files"
             new-books new-notes updated skipped)
    (list :new-books new-books :new-annotations new-notes :updated-annotations updated
          :skipped-files skipped :warnings (nreverse org-koreader--warnings))))

(provide 'org-koreader-sync)
;;; org-koreader-sync.el ends here
