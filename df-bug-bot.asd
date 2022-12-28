;;;; df-bug-bot.asd

(asdf:defsystem #:df-bug-bot
  :description "mastodon bot that posts bugs from the dwarf fortress bug tracker"
  :author "ava fox"
  :license  "NPLv1+"
  :version "0.0.1"
  :serial t
  :depends-on (#:glacier #:cl-csv #:with-user-abort)
  :components ((:file "package")
               (:file "df-bug-bot"))
  :entry-point "df-bug-bot:main"
  :build-operation "program-op"
  :build-pathname "bin/df-bug-bot")
