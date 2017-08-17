(defvar *announcements* ()
  "The main store for all announcemnets.")

;;;
;; Handle file
;;;

(defvar billboard-announcements-flie nil
  "The file to store announcements")

;; TODO: may want to have this set by user in .emacs
;;(setq billboard-announcements-file "~/repos/billboard/announcements.billboard")
(setq billboard-announcements-file "c:/Users/AllenWorkstation/Repos/billboard/announcements.billboard")


(defun billboard-read-file () 
;; TODO: This probably should not be a final solution.
  (setq *announcements*
	(with-temp-buffer
	  (insert-file-contents billboard-announcements-file)
	  (let ((value (condition-case nil
			  (read (buffer-string))
			(error ()))))
	    value))))

(defun billboard-write-file ()
  (with-temp-file billboard-announcements-file
    (prin1 *announcements* (current-buffer))))

;;;
;; Billboard commands
;;;

(defun delete-announcement (id-key announcement-store)
  (cond
   ((null announcement-store) '())
   ((equal id-key (car (car announcement-store)))
    (cdr announcement-store))
   (t (cons (car announcement-store)
	    (delete-announcement id-key (cdr announcement-store))))))

(defun make-announcement (ti co dl de no pr)
  "Create an announcemnet alist"
  (list
   (~random-id-string)
   (cons 'created (current-time))
   (cons 'contact co)
   (cons 'title ti)
   (cons 'deadline dl)
   (cons 'description de)
   (if (null no)
       (list 'notes)
     (list 'notes no))
   (cons 'priority pr)))

(defun add-announcement (announcement)
  (push announcement *announcements*)
  (billboard-write-file))

(defun prompt-new-announcement (ti co dl de no pr)
  "Interactively get the values for a new announcement and add to db."
  (interactive
   (list
    (read-string "Title: ")
    (read-string "Contact: ")
    (call-interactively 'prompt-deadline)
    (call-interactively 'prompt-description)
    (call-interactively 'prompt-notes)
    (completing-read "Priority: " '("HIGH" "MED" "LOW"))))
  (add-announcement (make-announcement ti co dl de no pr)))

(defun prompt-deadline (arg)
  (interactive
   (list
    (apply #'encode-time (parse-time-string (concat (read-string "Deadline: ") " 00:00:00")))))
  arg)

(defun prompt-description (arg)
  (interactive
   (list
    (read-string "Descripation: ")))
  arg)

(defun prompt-notes (arg)
  (interactive
   (list
    (read-string "Note: ")))
  (if (equal arg "")
      nil
    (cons arg
	  (current-time))))

(defun add-notes (arg)
  (interactive
   (list
    (call-interactively 'prompt-notes))
   (if (equal arg "")
       (message "No note added")
     ((push arg (cdr (assoc 'notes (car cdr assoc (tabulated-list-get-id) *announcements*))))))))

;; (defun prompt-new-announcement (ti co dl de no pr)
;;   "Interactively get the values for a new announcement and add to db."
;;   (interactive "MTitle: \nMContact: \nMDealine: \nMDescription: \nMNotes: \nMPriority: ")
;;   (add-announcement (make-announcement ti co dl de no pr)))


(defun find-by-contact (contact)
  "Finds all records that have the value of contact in its 'contact association"
  (let (results)
    (dolist (annoucement *announcements* results)
      (if (equal? contact (cdr (assoc 'contact annoucement)))
	  (push annoucement results)))))

;; Header line for main Billboard buffer
;; (defun set-billboard-header-line ()
;;   (setf header-line-format
;; 	(center-header-line-text
;; 	 (concat (propertize "Billboard: " 'face 'success)
;; 		 (propertize "Keep track of Brushy Creek announcmnets" 'face 'success)))))

;;;
;; Billboard-list-mode
;;;

(defvar billboard-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "N") 'prompt-new-announcement)
    (define-key map (kbd "a") 'billboard-list-mark-archive)
    (define-key map (kbd "d") 'billboard-list-mark-delete)
    (define-key map (kbd "u") 'billboard-list-unmark)
    (define-key map (kbd "x") 'billboard-list-execute)
    map))

(defun announcement-list-entries ()
  "This function is used build a list of entries

It creates a list of list of list, each having an id for the first element
and a vector of objects for the second element. It currently uses the 
global variable '*announcements', but it might be useful at some point to
differnt datastore"

  ;; TODO: Read from file
  (let (entries)
    (if (eq *announcements* nil)
	(setq entries ())
      (dolist (anncmnt *announcements*)
	(let ((id (car anncmnt))
	      (priority (let ((value (cdr (assoc 'priority (cdr anncmnt)))))
			  (cond
			   ((equal value "LOW") "Low")
			   ((equal value "MED") (propertize "Med" 'face 'warning))
			   ((equal value "HIGH") (propertize "High" 'face 'error))
			   (t " "))))
	      (title (cdr (assoc 'title (cdr anncmnt))))
	      (deadline (pretty-date (cdr (assoc 'deadline (cdr anncmnt)))))
	      (contact (cdr (assoc 'contact (cdr anncmnt))))
	      (notes (let ((car (cdr (assoc 'notes (cdr anncmnt)))) note)
	      	       (if (null note)
	      		   (message (prin1-to-string note))
	      		 note))))
	      ;; (notes (car (cdr (assoc 'notes (cdr anncmnt))))))
	  (push (list id
		      (vector " "
			      priority			    
			      title
			      deadline
			      contact
			      notes))
		entries))))
    (setq tabulated-list-entries entries)))

;;list functions

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

(defun billboard-list-mark-delete ()
  "mark the entry for delete"
  (interactive)
  (when (tabulated-list-get-entry)
    (tabulated-list-set-col 0 "D" t)))

(defun billboard-list-execute ()
  "execute all marks on the marked entries"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (let ((id (tabulated-list-get-id))
	    (entry (tabulated-list-get-entry)))
	(cond ((null entry) (forward-line 1))
	      ((equal (aref entry 0) "A")
	       (message "Archiving")
	       (tabulated-list-set-col 0 " " t))
  	      ((equal (aref entry 0) "D")
  	       (setq *announcements*(delete-announcement (tabulated-list-get-id) *announcements*))
  	       (tabulated-list-delete-entry)))
	(forward-line 1)))
    (billboard-write-file)))

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
      (billboard-read-file)
      (announcement-list-entries)
      (tabulated-list-print)
      (switch-to-buffer new-buffer))))

;;;
;; Upcoming Bulletin View
;; TODO:
;;;

;; Find Sundays from now until 'time'
;; -> (sunday-1 sunday-2 sunday-3)

;; Build list of announcements for each week
;; -> ((sunday-1 (announcment-id-1 announcment-id-2))
;;     (sunday-2 (announcment-id-1 announcment-id-2)))
;;     (sunday-3 (announcment-id-1 announcment-id-2)))

;; Display list for each week in a buffer ex. "upcoming-bulletins-ui-sketch"

(defun billboard-upcoming (arg)
  "Create a buffer to display each bulletin and its announcements untill a provided date"
  (interactive
   (list
    (apply #'encode-time (parse-time-string (concat (read-string "Display every sunday until: ") " 00:00:00")))))
    (let ((new-buffer (get-buffer-create "*Upcoming Bulletins*")))
      (save-current-buffer
	(set-buffer new-buffer)
	(setq-local bulletins (build-bulletins (find-sundays (current-time) arg))) ;TODO: build-bulletins
	(billboard-upcoming-mode) ;TODO: billboard-upcoming-mode
	(goto-char (point-min))
	(billboard-bulletins-display bulletins) ;TODO: billboard-bulletins-display
	(switch-to-buffer new-buffer))))

(defun build-bulletins (sundays) 
  "Build a lists of lists of a time as car and a list of ids for the cdr"
  (let ((announcements *announcements*))
      (mapcar (lambda (time)
		(cons time (funcall #'build-bulletin time announcements)))
	      sundays)))

(defun build-bulletin (time announcement-list)
			    (cond ((null announcement-list) '())
				  ((>= (time-to-seconds time) (time-to-seconds (cdr (assoc 'deadline (cdr (car announcement-list))))))
				   (cons (car (car announcement-list)) (build-bulletin time (cdr announcement-list))))
				  (t (build-bulletin time (cdr announcement-list)))))

(defun find-sundays (start-time end-time)
  ;TODO: Should probaly do something about all the 'decode-time'that I use in this.
  "Takes start-date and end-date and returns a list of dates for every sunday in that range."
  (let ((date (if (eq (nth 6 (decode-time start-time)) 0)
		  start-time
		(encode-time 0 0 0
			     (+ (+ (- 7 (nth 6 (decode-time start-time))) 0) (nth 3 (decode-time start-time))) ; finds the next sundays day of month
			     (nth 4 (decode-time start-time))
			     (nth 5 (decode-time start-time)))))
	(end-date (if (eq (nth 6 (decode-time end-time)) 0)
		      end-time
		    (encode-time 0 0 0
				 (- (nth 3 (decode-time end-time)) (nth 6 (decode-time end-time))) ;find previous sunday
				 (nth 4 (decode-time end-time))
				 (nth 5 (decode-time end-time)))))
	(sundays))
    (while (<= (time-to-seconds date) (time-to-seconds end-date))
      (setq sundays (cons date sundays))
      (setq date (encode-time 0 0 0
			      (+ (nth 3 (decode-time date)) 7) ;set day of month to next sunday
			      (nth 4 (decode-time date)) ;set month to the month of date
			      (nth 5 (decode-time date))))) ;set year to the year of date
    (reverse sundays)))

;;;
;; For testing 
;;;

(defun null-vars ()
  (interactive)
  (setq *announcements* nil))

(defun set-db ()
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
	 (notes . nil)))))

;;;
;; Utility
;;;

(defun pretty-date (time)
;;  (format "%s" time))
  (let ((string (format-time-string "%e %B" time)))
    string))

(defun print-decoded (list)
    (cond
     ((null (car list)) '())
     (t (cons (decode-time (car list))
	      (print-decoded (cdr list))))))

(let ((list (find-sundays (current-time) (apply #'encode-time (parse-time-string "2017-09-20 00:00:00")))))
  (print-decoded list))

;; (progn
;;  (forward-line 1)
;;  (insert (prin1-to-string (build-bulletins (find-sundays (current-time) (apply #'encode-time (parse-time-string (concat (read-string "Display every sunday until: ") " 00:00:00"))))

(defun ~random-id-string ()
  (md5 (format "%s%s%s%s"
	       (user-uid)
	       (current-time)
	       (random)
	       (recent-keys))))

(defun build-spaces (num)
  (cond
   ((= 0 num) "")
   (t (concat " " (build-spaces (- num 1))))))

(defun center-header-line-text (str)
  (let ((line-length (window-width))
	(str-length (length str)))
    (concat (build-spaces (- (/ line-length 2) (/ str-length 2)))
	    str)))
