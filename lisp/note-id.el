(defun note-id--first-non-zero (id)
  "Get the first non-zero id position, i.e. the one to be incrimented. Zero-based index."
    (cond
    ((not id) nil)
    ((= (length id) 1) 0)
    ((> (car id) 0) 0)
    (t (+ 1 (note-id--first-non-zero (cdr id))))))

;(note-id--first-non-zero (list 1 0 0 0))
;(note-id--first-non-zero (list 0 1 0 0))
;(note-id--first-non-zero (list 0 0 1 0))
;(note-id--first-non-zero (list 0 0 0 1))
;(note-id--first-non-zero (list 0 0 0 0))

(defun note-id--last-non-zero (id)
    "Find the last non-zero position of a note ID"
    ;; This could be much easier implemetied using seq-positions
    ;; It is easeir to find first non-zero element, so I work on reverse
    (- (length id) (+ (note-id--first-non-zero (reverse id))) 1))

;(note-id--last-non-zero (list 1 1 1 1))

(defun note-id--increment-position (id pos)
    "Increment the note ID at a given position"
    (let* ((before (- (length id) pos))
	    (after (+ pos 1)))
	(append (butlast id before) (list (+ 1 (nth pos id))) (nthcdr after id))))

; (print
;     (list
; 	(note-id--increment-position (list 0 0 0 1) 0)
; 	(note-id--increment-position (list 1 0 0 0) 1)
; 	(note-id--increment-position (list 1 1 0 0) 2)
; 	(note-id--increment-position (list 0 0 1 0) 3)
; 	(note-id--increment-position (list 0 0 0 1) 3)))

(defun note-id--extract-id-string-from-filename (filename)
"Extracts the substring of filename that contains the ID.
    Returns nil if no valid ID is found"
  (if (string-match "^\\([0-9]+\\(-[0-9]+\\)*\\)" filename)
      (match-string 1 filename)))
; (note-id--extract-id-string-from-filename "01-02-03_who-is-it.org")
; (note-id--extract-id-string-from-filename "01-02-03-who-is-it.org")
; (note-id--extract-id-string-from-filename "01-02-03-w04-is-it.org")
; (note-id--extract-id-string-from-filename "01-02-03 04-05-06.org")

(defun note-id--from-filename (filename)
    "Extract ID from the prefix of filename"
    (let* ((prefix (note-id--extract-id-string-from-filename (file-name-nondirectory filename))) ;; Get the filename prefix
	(parts (split-string prefix "-"))          ;; Split the prefix into parts
	(id (mapcar #'string-to-number parts)))    ;; Convert each part to a number
    id))   ;; Otherwise, raise an error

; (note-id--from-filename "01-03-01-0200-0003_pdf.org")
; (note-id--from-filename "01-02-03_who-is-it.org") ; Should be (1 2 3)
; (note-id--from-filename (note-id--current-file))

(defun note-id--to-string (id)
  "Convert ID list to string format 000-000-000-000"
  (if (null id)
        ""
        (concat
  (     format "%03d" (car id))
        (if (cdr id) "-" "")
        (note-id--to-string (cdr id)))))

; (note-id--to-string (list 1 0 0 1))

(defun note-id--current-file () ;
    "Get current file for note-id. If in dired takes the file under currsor."
    (if (derived-mode-p 'dired-mode) (dired-get-file-for-visit) buffer-file-name))

; (note-id--current-file)

(defun note-id--current-file-directory () ;
    "Get the directory of the current buffer's file."
    (file-name-directory (note-id--current-file)))
; (note-id--current-file-directory)

(defun note-id--get-filename (directory id)
    "Get the file with the given ID. Assumes that ID length has to equal to that of ~id~"
    (let* ((id-string (note-id--to-string id))
	    (files (directory-files directory nil (concat "^" id-string ".*\\.org$") t)))
    (if (> (length files) 1)
	(error  "Multipe files with same ID")
	(car files))))

; (note-id--get-filename (note-id--current-file-directory) (list 0 1 0 0 ))
; (note-id--get-filename (note-id--current-file-directory) (note-id--from-filename (note-id--current-file)))


(defun note-id--increment-recursive (directory id pos)
    (let* ((new-id (note-id--increment-position id pos))
	    (new-id-filename (note-id--get-filename directory new-id)))
	(if new-id-filename
	    (note-id--increment-recursive directory new-id pos)
	    new-id)))

(defun note-id--increment (directory id max-depth)
"Increment ID util a free ID is found (or max-depth reached)"
(let* ((pos (note-id--last-non-zero id))
	(new-id (note-id--increment-position id pos))
	(new-id-filename (note-id--get-filename directory new-id)))
    ; max-depth is in length units, i.e., max-depth=4 means 4-length id
    (if (>= pos max-depth)
	(error "Maximum note depth reached.")
    (if new-id-filename
	(note-id--increment-recursive directory id (+ 1 pos))
	new-id))))

(defun note-id--create-next-note (current-note)
  "Create a new note with an incremented ID based on CURRENT-NOTE."
  (let* ((current-id (note-id--from-filename current-note))
         (new-id (note-id--increment (file-name-directory current-note) current-id 4)) ; Adjust max depth as needed
         (name-suffix (read-string "Enter filename suffix (empty for none): "))
         (new-filename (concat (note-id--to-string new-id)
                               (if (string-empty-p name-suffix) "" (concat "_" name-suffix))
                               ".org"))
         (full-path (concat (note-id--current-file-directory) new-filename)))
    (write-region "" nil full-path)
    (message "Created next note: %s" new-filename)
    full-path))

;(note-id--increment (note-id--current-file-directory) (list 3 1 0 0))
;(note-id--increment-position (list 0 0 0 0) 3)
;(note-id--increment (note-id--current-file-directory) (list 10 0 0 0) 4)
(defun note-id-create-next-note ()
  "Create the next note, using the file under the cursor as the starting point."
  (interactive)
  (let* ((new-file (note-id--create-next-note (note-id--current-file))))
    ;; Open the new file if not in Dired mode
    (unless (derived-mode-p 'dired-mode)
      (find-file new-file))))

(defun note-id-create-and-link ()
"Create a new note with ID
    Generate a new ID, prompt for a filename, create the
    file, and insert org-mode links."
(interactive)
(let* ((directory (note-id--current-file-directory))
	(current-filename (file-name-nondirectory buffer-file-name))
	(new-filename (file-name-nondirectory (note-id--create-next-note (note-id--current-file))))
	(current-file-link (format "[[file:%s][%s]]" new-filename new-filename))
	(new-file-link (format "[[file:%s][%s]]" current-filename current-filename)))
    ;; Create new file and insert links
    (find-file current-filename)
    (insert current-file-link "\n")
    (find-file new-filename)
    (insert new-file-link "\n")))
