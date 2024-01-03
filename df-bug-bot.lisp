;;;; df-bug-bot.lisp

(in-package #:df-bug-bot)

(declaim (inline reinitizalize-random-state post-poll-p get-random-bug))

(defvar *csv-file* #P"df-bugs.csv"
  "path to our CSV file")
(defvar *parsed-csv* nil
  "a list that contains our parsed CSV")
(defvar *poll-chances* 0.25
  "the chances for posting a status with a poll")
(defvar *csv-regex* "(?m)^[0-9]{7,}.+,(open|reopened|unable to reproduce|not fixable|suspended|won't fix),.+$"
  "regex used for pre-processing the downloaded CSV file")
(defvar *recent-ids* nil
  "a list containing the 5 most recent bug ids")
(defvar *bug-tracker-url* "https://dwarffortressbugtracker.com/csv_export.php"
  "link to the DF bug tracker CSV export")

(defun reinitizalize-random-state ()
  "reinitializes the random state with a new one"
  (setf *random-state* (make-random-state t)))

(defun post-poll-p ()
  "checks if we should post a poll"
  (>= *poll-chances* (random 1.0)))

(defun parse-csv (csv)
  "transforms the CSV to ensure that it meets our specs"
  ;; remove all lines that arent actual bugs
  (ppcre:all-matches-as-strings *csv-regex*
                                ;; remove all non-terminated strings (to appease cl-csv
                                (ppcre:regex-replace-all "(?m)\".+$" csv "")))

(defun refresh-csv-file ()
  "downloads the bug CSV file and writes it to *CSV-FILE*"
  (let ((csv-string (drakma:http-request *bug-tracker-url*)))
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

        ;; loops until we meet the following criteria:
        ;;  1- the bug hasnt been posted recently (last 5 posts)
        ;;  2- the bug text itself exists
        ;;  3- the bug text isnt an empty string
        :until (and (not (member (nth 0 bug) *recent-ids* :test #'string=)) 
                    (nth 11 bug)
                    (not (str:emptyp (nth 11 bug))))
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

