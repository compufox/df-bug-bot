;;;; df-bug-bot.lisp

(in-package #:df-bug-bot)

(defvar *csv-file* #P"df-bugs.csv")
(defvar *parsed-csv* nil)
(defvar *poll-chances* 0.25)

(defun post-poll-p ()
  (>= *poll-chances* (random 1.0)))

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

  (setf *random-state* (make-random-state t))
  
  ;; download our csv file if it doesnt exist
  ;; if its already downloaded go ahead and parse it
  (if (uiop:file-exists-p *csv-file*)
      (setf *parsed-csv* (parse-csv))
      (refresh-csv-file))
  
  (handler-case
      (with-user-abort
        (run-bot ((make-instance 'mastodon-bot :config-file "config.file")
                  :with-websocket nil)
          
          ;; after every 4 days (and 13 min) refresh the bug list
          ;;  this should hopefully not overlap with the posting, 
          ;;  because that might mess things up 
          (after-every ((time-to-seconds 4 :days 13 :min) :seconds :async t)
            (refresh-csv-file))
          
          ;; post a random bug every hour
          (after-every (1 :hour :run-immediately t)
            (let ((poll? (post-poll-p)))
              (post (generate-post)
                    :poll-options (when poll? '("Bug?" "Feature?"))
                    :poll-timeout (when poll? (time-to-seconds 1 :hour)))))))
    
    (user-abort ()
      (uiop:quit))))

