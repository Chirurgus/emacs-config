(defun note-id-first-non-zero (id)
    (cond
    ((not id) nil)
    ((and (= (length id) 1) (> (nth 0 id) )) 0)
    ((> (car id) 0) 0)
    (t (+ 1 (note-id-first-non-zero (cdr id))))))

;(note-id-first-non-zero (list 0 0 0 0))

(defun note-id-last-non-zero (id)
    "Find the last non-zero position of a note ID"
    ;; This could be much easier implemetied using seq-positions
    ;; It is easeir to find first non-zero element, so I work on reverse
    (- (length id) (+ (note-id-first-non-zero (reverse id))) 1))

;(note-id-last-non-zero (list 1 1 1 1))

(defun note-id-increment-position (id pos)
    "Increment the note ID at a given position"
    (let* ((before (- (length id) pos))
	    (after (+ pos 1)))
	(append (butlast id before) (list (+ 1 (nth pos id))) (nthcdr after id))))

; (print
;     (list
; 	(note-id-increment-position (list 0 0 0 1) 0)
; 	(note-id-increment-position (list 1 0 0 0) 1)
; 	(note-id-increment-position (list 1 1 0 0) 2)
; 	(note-id-increment-position (list 0 0 1 0) 3)
; 	(note-id-increment-position (list 0 0 0 1) 3)))

(defun note-id-from-filename (filename)
    "Extract ID from the prefix of filename"
    (let* ((prefix (file-name-nondirectory filename)) ;; Get the filename prefix
	(parts (split-string prefix "-"))          ;; Split the prefix into parts
	(id (mapcar #'string-to-number parts)))    ;; Convert each part to a number
    id))   ;; Otherwise, raise an error

; (note-id-from-filename "01-03-01-0200-0003_pdf.org")

(defun note-id-to-string (id)
	    "Convert ID list to string format 000-000-000-000"
	    (if (null id)
		""
		(concat
		(format "%03d" (car id))
		(if (cdr id)
		    "-"
		"")
		(note-id-to-string (cdr id)))))

; (note-id-to-string (list 1 0 0 1))

(defun note-id-current-file-directory ()
    "Get the directory of the current buffer's file."
    (when buffer-file-name
    (file-name-directory buffer-file-name)))

(defun note-id-get-filename (directory id)
    "Get the file with the given ID"
    (let* ((id-string (note-id-to-string id))
	    (files (directory-files directory nil (concat "^" id-string ".*\\.org$") t)))
    (if (> (length files) 1)
	(error  "Multipe files with same ID")
	(car files))))

; (note-id-get-filename (note-id-current-file-directory) (list 0 0 0 8))

(defun note-id-increment-recursive (directory id depth max-depth)
    (if (> depth max-depth)
	(error "Maximum note depth reached.")
    (let* ((new-id (note-id-increment-position id depth))
	    (new-id-filename (note-id-get-filename directory new-id)))
	(if new-id-filename
	    (note-id-increment-recursive directory id (+ 1 depth) max-depth)
	    new-id))))

(defun note-id-increment (directory id)
    "Increment ID util a free ID is found (or max-depth reached)"
    (note-id-increment-recursive directory id (note-id-last-non-zero id) 3))

;(note-id-increment (note-id-current-file-directory) (list 3 1 0 0))
;(note-id-increment-position (list 0 0 0 0) 3)

(defun note-id-increment-recursive (directory id pos)
    (let* ((new-id (note-id-increment-position id pos))
	    (new-id-filename (note-id-get-filename directory new-id)))
	(if new-id-filename
	    (note-id-increment-recursive directory new-id pos)
	    new-id)))

(defun note-id-increment (directory id max-depth)
"Increment ID util a free ID is found (or max-depth reached)"
(let* ((pos (note-id-last-non-zero id))
	(new-id (note-id-increment-position id pos))
	(new-id-filename (note-id-get-filename directory new-id)))
    ; max-depth is in length units, i.e., max-depth=4 means 4-length id
    (if (>= pos max-depth)
	(error "Maximum note depth reached.")
    (if new-id-filename
	(note-id-increment-recursive directory id (+ 1 pos)) 
	new-id))))

;(note-id-increment (note-id-current-file-directory) (list 10 0 0 0) 4)

(defun note-id-create-and-link ()
"Create a new note with ID
    Generate a new ID, prompt for a filename, create the
    file, and insert org-mode links."
(interactive)
(let* ((directory (note-id-current-file-directory))
	(current-filename (file-name-nondirectory buffer-file-name))
	(current-id (note-id-from-filename current-filename))
	(new-id (note-id-increment directory current-id 4)) ; Adjust the max depth as needed
	(name-suffix (read-string "Enter filename suffix (empty for none): "))
	(new-filename (concat
			    (note-id-to-string new-id)
			    (if (string-empty-p name-suffix) "" (concat "_" name-suffix))
			    ".org"))
	(current-file-link (format "[[file:%s][%s]]" new-filename new-filename))
	(new-file-link (format "[[file:%s][%s]]" current-filename current-filename)))
    ;; Create new file and insert links
    (write-region "" nil new-filename)
    (find-file current-filename)
    (insert current-file-link "\n")
    (find-file new-filename)
    (insert new-file-link "\n")))

