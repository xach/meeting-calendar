;;;; meeting-calendar.asd

(asdf:defsystem #:meeting-calendar
  :depends-on (#:drakma #:cxml #:cxml-stp #:cl-who)
  :serial t
  :author "Zach Beane <xach@xach.com>"
  :license "BSD"
  :description "Write an HTML file for upcoming Lisp meetings."
  :components ((:file "package")
               (:file "xml")
               (:file "http")
               (:file "calendar")
               (:file "command-line")))
