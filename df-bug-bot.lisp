;;;; df-bug-bot.lisp

(in-package #:df-bug-bot)

(declaim (inline reinitizalize-random-state post-poll-p get-random-bug))

(defvar *csv-file* #P"df-bugs.csv")
(defvar *parsed-csv* nil)
(defvar *poll-chances* 0.25)
(defvar *csv-regex* "(?m)^[0-9]{7,}.+,(open|reopened|unable to reproduce|not fixable|suspended|won't fix),.+$")
(defvar *recent-ids* nil
  "a list containing the 5 most recent bug ids")

(defun reinitizalize-random-state ()
  (setf *random-state* (make-random-state t)))

(defun post-poll-p ()
  (>= *poll-chances* (random 1.0)))

(defun parse-csv (csv)
  "transforms the CSV to ensure that it meets our specs"
  ;; remove all lines that arent actual bugs
  (ppcre:all-matches-as-strings *csv-regex*
                                ;; remove all non-terminated strings (to appease cl-csv
                                (ppcre:regex-replace-all "(?m)\".+$" csv "")))

(defun refresh-csv-file ()
  "downloads the bug CSV file and writes it to *CSV-FILE*"
  (let ((csv-string (drakma:http-request "https://dwarffortress.mantishub.io/csv_export.php")))
    (str:to-file *csv-file*
                 (str:join (string #\newline)
                           (parse-csv csv-string)))
    (setf *parsed-csv* (cl-csv:read-csv *csv-file*))))

(defun get-random-bug ()
  "gets a random bug from the list"
  (nth (random (length *parsed-csv*)) *parsed-csv*))

(defun generate-post ()
  "fetches a random bug and generates a post with it"
  (loop :with bug := (get-random-bug)
        :until (not (member (nth 0 bug) *recent-ids* :test #'string=))
        :do (setf bug (get-random-bug))

            ;; when we finally have a new bug we push the
            ;; bug id into the recent-id list and make sure that
            ;; we only have the most recent ids cached.
            ;;  then we redo the random state just for kicks
            ;;  before returning the text for the post
        :finally
           (push (nth 0 bug) *recent-ids*)
           (setf *recent-ids* (subseq *recent-ids* 0 (min (length *recent-ids*) 5)))
           (reinitizalize-random-state)
           (return (format nil "~A: ~A" (nth 0 bug) (nth 11 bug)))))

(defun main ()
  "main binary entry point"

  (reinitizalize-random-state)
  
  ;; download our csv file if it doesnt exist
  ;; if its already downloaded go ahead and parse it
  (if (uiop:file-exists-p *csv-file*)
      (setf *parsed-csv* (cl-csv:read-csv *csv-file*))
      (refresh-csv-file))
  
  (handler-case
      (with-user-abort
        (run-bot ((make-instance 'mastodon-bot :config-file "config.file")
                  :with-websocket nil)
          
          ;; after every 4 days (and 13 min) refresh the bug list
          ;;  this should hopefully not overlap with the posting, 
          ;;  because that might mess things up 
          (after-every ((time-to-seconds 4 :days 13 :minutes) :seconds :async t)
            (refresh-csv-file))
          
          ;; post a random bug every hour
          (after-every (1 :hour :run-immediately t)
            (let ((poll? (post-poll-p)))
              (post (generate-post)
                    :poll-options (when poll? '("Bug?" "Feature?" "Normal dwarven behavior?"))
                    :poll-timeout (when poll? 3600))))))

    (error (e)
      (format t "~A~%" e)
      (uiop:quit))
    
    (user-abort ()
      (uiop:quit))))

