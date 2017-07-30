(defvar *announcements* nil
  "The main store for all announcemnets.")

(defun make-announcement (ti co dl de no pr)
  "Create an announcemnet alist"
  (list (cons 'contact co)
	(cons 'title ti)
	(cons 'deadline dl)
	(cons 'description de)
	(cons 'notes no)
	(cons 'priority pr)))

(defun add-announcement (announcement) (push announcement *announcements*))

(defun prompt-new-announcemnt (ti co dl de no pr)
  "Interactively get the values for a new announcement and add to db."
  (interactive "MTitle: \nMContact: \nMDealine: \nMDescription: \nMNotes: \nMPriority: ")
  (add-announcement (make-announcement ti co dl de no pr)))

(defun find-by-contact (contact)
  "Finds all records that have the value of contact in its 'contact association"
  (let (results)
    (dolist (annoucement *announcements* results)
      (if (equal? contact (cdr (assoc 'contact annoucement)))
	  (push annoucement results)))))

(defun build-spaces (num)
  (cond
   ((= 0 num) "")
   (t (concat " " (build-spaces (- num 1))))))

(defun center-header-line-text (str)
  (let ((line-length (window-width))
	(str-length (length str)))
    (concat (build-spaces (- (/ line-length 2) (/ str-length 2)))
	    str)))

;; Header line for main Billboard buffer
(defun set-billboard-header-line ()
  (setf header-line-format
	(center-header-line-text
	 (concat (propertize "Billboard: " 'face 'success)
		 (propertize "Keep track of Brushy Creek announcmnets" 'face 'success)))))


(define-derived-mode billboard-list-mode
  tabulated-list-mode "Billboard List"
  "Major mode for displaying a list of announcments"

  (setq tabulated-list-format
	[("Title" 30 nil)
	 ("Deadline" 12 t)
	 ("Contact" 20 t)
	 ("Notes" 5 t)
	 ("Description" 40 nil)])
  (tabulated-list-init-header))

;; This is the actual command to create the list buffer
(defun billboard-list ()
  "Create buffer for list and change major mode"
  (interactive)
  (let ((new-buffer (get-buffer-create "*Annoucements*")))
    (save-current-buffer
      (set-buffer new-buffer)
      (billboard-list-mode)
      (tabulated-list-print)
      (switch-to-buffer new-buffer))))

;; For testing 
(defun clear-vars ()
  (makunbound '*announcements*))

(defun reload-announcements ()
    (setq *announcements*
	  (((contact . "Allen Hughes")
	    (title . "Another thing")
	    (deadline . "2017-08-20")
	    (description . "Stuff and things")
	    (notes . "Nothing"))
	   ((contact . "Pattsy Wilson")
	    (title . "Wedding Shower for Allen and Haleigh")
	    (deadline . "2017-07-16")
	    (description . "What is says")
	    (notes . "noNotes")))))
