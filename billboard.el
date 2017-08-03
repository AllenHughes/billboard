(defvar *announcements* ()
  "The main store for all announcemnets.")

;;;
;; handle file
;;;

(defvar billboard-announcements-flie nil
  "The file to store announcements")

;; TODO: may want to have this set in the .emacs
(setq billboard-announcements-file "~/repos/billboard/announcements.billboard")

;; TODO: This should not be a final solution. Need to do something that isn't such a
;; security risk.
(defun billboard-read-file ()
  (with-temp-buffer
    (insert "(setq *announcements*")
    (forward-line 1)
    (insert "'")
    (insert-file-contents-literally billboard-announcements-file)
    (goto-char (point-max))
    (insert ")")
    (eval-buffer)))

(defun billboard-write-file ()
  (with-temp-file billboard-announcements-file
    (insert (prin1-to-string *announcements*))))

;;;
;; Billboard commands
;;;

(defun make-announcement (ti co dl de no pr)
  "Create an announcemnet alist"
  (list (cons 'contact co)
	(cons 'title ti)
	(cons 'deadline dl)
	(cons 'description de)
	(cons 'notes no)
	(cons 'priority pr)))

(defun add-announcement (announcement) (push announcement *announcements*))

(defun prompt-new-announcement (ti co dl de no pr)
  "Interactively get the values for a new announcement and add to db."
  (interactive "MTitle: \nMContact: \nMDealine: \nMDescription: \nMNotes: \nMPriority: ")
  (add-announcement (make-announcement ti co dl de no pr)))

(defun find-by-contact (contact)
  "Finds all records that have the value of contact in its 'contact association"
  (let (results)
    (dolist (annoucement *announcements* results)
      (if (equal? contact (cdr (assoc 'contact annoucement)))
	  (push annoucement results)))))

;; Header line for main Billboard buffer
(defun set-billboard-header-line ()
  (setf header-line-format
	(center-header-line-text
	 (concat (propertize "Billboard: " 'face 'success)
		 (propertize "Keep track of Brushy Creek announcmnets" 'face 'success)))))

;;;
;; Billboard-list-mode
;;;

(defvar billboard-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "N") 'prompt-new-announcement)
    (define-key map (kbd "a") 'billboard-list-mark-archive)
    (define-key map (kbd "u") 'billboard-list-unmark)
    map))

(defun announcement-list-entries ()
  "This function is used build a list of entries

It creates a list of list of list, each having an id for the first element
and a vector of objects for the second element. It currently uses the 
global variable '*announcements', but it might be useful at some point to
differnt datastore"
  (let (entries)
    (dolist (anncmnt *announcements*)
      (let ((id (cdr (assoc 'id anncmnt)))
	    (priority (let ((value (cdr (assoc 'priority anncmnt))))
			(cond
			 ((equal value "LOW") "Low")
			 ((equal value "MED") (propertize "Med" 'face 'warning))
			 ((equal value "HIGH") (propertize "High" 'face 'error))
			 (t " "))))
	    (title (cdr (assoc 'title anncmnt)))
	    (deadline (cdr (assoc 'deadline anncmnt)))
	    (contact (cdr (assoc 'contact anncmnt)))
	    (notes (if (eq (cdr (assoc 'notes anncmnt)) nil)
		       " "
		     "  Y  ")))
	(push (list id
		    (vector " "
			    priority			    
			    title
			    deadline
			    contact
			    notes))
	      entries)))
    (setq tabulated-list-entries entries)))

(defun billboard-list-unmark ()
  "remove the action mark from the first col of the row"
  (interactive)
  (when (tabulated-list-get-entry)
    (tabulated-list-set-col 0 " " t)))

(defun billboard-list-mark-archive ()
  "mark the entry for archive"
  (interactive)
  (when (tabulated-list-get-entry)
    (tabulated-list-set-col 0 "A" t)))

(defun billboard-list-execute ()
  "execute all marks on the marked entries"
  (interactive)
  (save-excursion
    (list-beginning)
    (while (not (eobp))
      (let ((id (tabulated-list-get-id))
	    (entry (tabulated-list-get-entry)))
	(cond ((null entry) (forward-line 1))
	      ((equal (aref entry 0) "A")
	       ;;Archive entry
	       (tabulated-list-set-col 0 " " t))
	      ((equal (aref entry 0) "D")
	       ;;Delete entry
	       (tabulated-list-delete-entry))))))
  (tabulated-list-revert))

(define-derived-mode billboard-list-mode
  tabulated-list-mode "Billboard List"
  "Major mode for displaying a list of announcments"

  (setq tabulated-list-format
	[(" " 1 nil)
	 (" " 6 t)
	 ("Title" 30 nil)
	 ("Deadline" 12 t)
	 ("Contact" 20 t)
	 ("Notes" 5 t)])
  (use-local-map billboard-list-mode-map)
  (add-hook 'tabulated-list-revert-hook 'announcement-list-entries nil t)
  (tabulated-list-init-header))

;; This is the actual command to create the list buffer
(defun billboard ()
  "Create buffer for list and change major mode"
  (interactive)
  (let ((new-buffer (get-buffer-create "*Annoucements*")))
    (save-current-buffer
      (set-buffer new-buffer)
      (billboard-list-mode)
      (hl-line-mode)
      (announcement-list-entries)
      (tabulated-list-print)
      (switch-to-buffer new-buffer))))

;; For testing 
(defun null-vars ()
  (interactive)
  (setq *announcements* nil))

(setq *announcements*
      '(((id . 20170720574A)
	 (contact . "Allen Hughes")
	 (title . "Another thing")
	 (deadline . "2017-08-20")
	 (description . "Stuff and things")
	 (priority . "LOW")
	 (notes . "Nothing"))
	((id . 20170723584B)
	 (contact . "Pattsy Wilson")
	 (title . "Wedding Shower for Allen and Haleigh")
	 (deadline . "2017-07-16")
	 (description . "What is says")
	 (notes . "noNotes")
	 (priority . "LOW"))
	((id . 20170724884B)
	 (contact . "Haleigh Hughes")
	 (title . "A new thing were doing")
	 (deadline . "2017-08-23")
	 (description . "Another thing")
	 (priority . "MED")
	 (notes . nil))
      	((id . 2017072488548743FD)
	 (contact . "LInda Hughes")
	 (title . "Kniting")
	 (deadline . "2017-10-23")
	 (priority . "LOW")
	 (description . "A thing were the yall get together and knit")
	 (notes . nil))))

;;;
;; Utility
;;;

(defun build-spaces (num)
  (cond
   ((= 0 num) "")
   (t (concat " " (build-spaces (- num 1))))))

(defun center-header-line-text (str)
  (let ((line-length (window-width))
	(str-length (length str)))
    (concat (build-spaces (- (/ line-length 2) (/ str-length 2)))
	    str)))
