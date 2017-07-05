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
  (let (results)
    (dolist (annoucement *announcements* results)
      (if (equal contact (cdr (assoc 'contact annoucement)))
	  (push annoucement results)))))


;; For testing 

;; (((contact . "Allen Hughes")
;;   (title . "Another thing")
;;   (deadline . "2017-08-20")
;;   (description . "Stuff and things")
;;   (notes . "Nothing"))
;;  ((contact . "Pattsy Wilson")
;;   (title . "Wedding Shower for Allen and Haleigh")
;;   (deadline . "2017-07-16")
;;   (description . "What is says")
;;   (notes . "noNotes")))

;; (makunbound '*announcements*)

;; (setq *test-db*
;;       (list
;; 	(cons 'contact (list (cons "Allen Hughes" (list "864-640-0788" "arh62090@gmail.com"))))
;; 	(cons 'title "Wedding Shower")))

;; (cdr (car (cdr (assoc 'contact *test-db*))))

