;;;; package.lisp

(defpackage #:meeting-calendar
  (:use #:cl
        #:stp)
  (:export #:toplevel)
  (:shadowing-import-from #:cl-who
                          #:with-html-output-to-string
                          #:fmt
                          #:str
                          #:htm
                          #:esc))

(in-package #:meeting-calendar)

