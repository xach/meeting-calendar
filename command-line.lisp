;;;; command-line.lisp

(in-package #:meeting-calendar)

(defun tweet-tomorrows-meetings (calendar)
  (loop for meeting across (meetings-tomorrow calendar)
        do
        (when (and (urgentp meeting)
                   (not (postedp meeting)))
          (format *trace-output* "~&;Tweeting ~A...~%" meeting)
          (maybe-tweet-meeting meeting))))

(defun write-calendar-html-file (calendar file)
  (with-open-file (stream file :direction :output
                               :if-exists :supersede)
    (write-string (html-snippet calendar) stream))
  (format t "~&; Wrote ~S~%" (probe-file file)))

(defun toplevel (args)
  (let ((output-file (second args)))
    (if output-file
        (let ((calendar (make-calendar (fetch-calendar-document))))
          (write-calendar-html-file calendar output-file))
        (format t "Invalid args. Usage: meeting-calendar OUTPUT-FILE~%"))))
