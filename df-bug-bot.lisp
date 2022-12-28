;;;; df-bug-bot.lisp

(in-package #:df-bug-bot)

(defvar *csv-file* #P"df-bugs.csv")
(defvar *parsed-csv* nil)

(defun parse-csv ()
  "removes all bugs that aren't currently open"
  (remove-if #'(lambda (record)
                       (not (string= (nth 13 record) "open")))
               (cl-csv:read-csv *csv-file*)))

(defun refresh-csv-file ()
  "downloads the bug CSV file and writes it to *CSV-FILE*"
  (with-open-file (output *csv-file* :direction :output
                                     :if-exists :supersede
                                     :if-does-not-exist :create)
    (format output "~A~%" (drakma:http-request "https://dwarffortress.mantishub.io/csv_export.php")))
  (setf *parsed-csv* (parse-csv)))

(defun get-random-bug ()
  "gets a random bug from the list"
  (nth (random (length *parsed-csv*)) *parsed-csv*))

(defun generate-post ()
  "generates a post"
  (let ((bug (get-random-bug)))
    (format nil "~A: ~A" (nth 0 bug) (nth 11 bug))))

(defun main ()
  "main binary entry point"
  
  ;; go ahead and download our CSV file
  (refresh-csv-file)
  
  (handler-case
      (with-user-abort
        (run-bot ((make-instance 'mastodon-bot :config-file "config.file")
                  :with-websocket nil)
          
          ;; after every 4 days (and 13 min) refresh the bug list
          ;;  this should hopefully not overlap with the posting, 
          ;;  because that might mess things up 
          (after-every (5773 :minutes :async t)
            (refresh-csv-file))
          
          ;; post a random bug every hour
          (after-every (1 :hour :run-immediately t)
            (post (generate-post)))))
    
    (user-abort ()
      (uiop:quit))))

