(require 'asdf)

(setf asdf:*central-registry*
      (list* *load-truename*
             (mapcar #'directory-namestring
                     (directory "deps/**/*.asd"))))

(require 'meeting-calendar)

(sb-ext:save-lisp-and-die "meeting-calendar"
                          :executable t
                          :toplevel 'meeting-calendar::toplevel)
