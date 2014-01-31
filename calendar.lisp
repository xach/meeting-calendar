;;;; calendar.lisp

(in-package #:meeting-calendar)

(defparameter *calendar-url*
  "http://www.google.com/calendar/feeds/pm55j8kg30dnm54ib2if9fuocc%40group.calendar.google.com/public/composite")

(defclass calendar ()
  ((events
    :initarg :events
    :accessor events
    :documentation "A vector of events.")))

(defmethod print-object ((calendar calendar) stream)
  (print-unreadable-object (calendar stream :type t)
    (format stream "~D events" (length (events calendar)))))

(defclass event ()
  ((title
    :initarg :title
    :accessor title)
   (description
    :initarg :description
    :accessor description)
   (link
    :initarg :link
    :accessor link)
   (short-url
    :initarg :short-url
    :accessor short-url)
   (start-time
    :initarg :start-time
    :accessor start-time)
   (postedp
    :initarg :posted
    :initform nil
    :accessor postedp
    :documentation "Has this event been posted to twitter?")))

(defmethod slot-unbound (class (event event) (slot (eql 'short-url)))
  (setf (short-url event) (shorten-url (link event))))

(defmethod print-object ((event event) stream)
  (print-unreadable-object (event stream :type t)
    (format stream "~S on ~A"
            (title event)
            (date-string (start-time event)))))

;;; Tweeting

(defun tweet-string (event)
  (format nil "#Lisp meeting tomorrow (~A): ~A - ~A"
          (date-string (start-time event))
          (title event)
          (short-url event)))

(defun maybe-tweet-meeting (event)
  (unless (postedp event)
    (post-tweet (tweet-string event))))

(defun parse-date (string)
  (flet ((number-at (start length)
           (parse-integer string :start start :end (+ start length))))
    (let ((year (number-at 0 4))
          (month (number-at 5 2))
          (day (number-at 8 2)))
      (encode-universal-time 0 0 0 day month year 0))))

(defun start-times (node)
  (map-matches (node-matches :name "when"
                             :ns *gd*)
               (lambda (time)
                 (attribute-value time "startTime"))
               node))

(defun make-events (node)
  (mapcar (lambda (time-string)
            (make-instance
             'event
             :title (first-text (node-matches :name "title"
                                              :ns *atom*)
                                node)
             :description (first-text (node-matches :name "content"
                                                    :ns *atom*
                                                    :attributes '(("type" "text")))
                                      node)
             :link (first-attribute (node-matches :name "link"
                                                  :ns *atom*
                                                  :attributes '(("type" "text/html")))
                                    node "href")
             :start-time (parse-date time-string)))
          (start-times node)))


(defun map-events (fun doc)
  (map-matches (node-matches :name "entry"
                             :ns *atom*)
               fun
               doc))

(defun in-the-past-p (object)
  (< (start-time object) (get-universal-time)))

(defun document-events (doc)
  (remove-if #'in-the-past-p
             (mapcan #'identity
                     (map-matches (node-matches :name "entry"
                                                :ns *atom*)
                                  (lambda (node)
                                    (unless (recurrent-entry-p node)
                                      (make-events node)))
                                  doc))))

(defun make-calendar (document)
  (let ((events (document-events document)))
    (make-instance 'calendar
                   :events (sort (coerce events 'vector) #'<
                                 :key #'start-time))))

(defun load-calendar ()
  (make-calendar (fetch-calendar-document)))

(defun meetings-tomorrow (calendar)
  (let* ((start (get-universal-time))
         (end (+ start 86400)))
    (remove-if-not (lambda (event)
                     (<= start (start-time event) end))
                   (events calendar))))

(defun pretty-date (event)
  (date-string (start-time event)))

(defun eventsp (calendar)
  (plusp (length (events calendar))))

(defun urgentp (event)
  (< (start-time event)
     (+ (get-universal-time) (* 86400 1))))

(defun html-snippet (calendar)
  (with-html-output-to-string (stream)
    (:div
     :id "lispmeetings"
     (:ul
      (if (eventsp calendar)
          (loop for event across (events calendar)
                do (htm (:li :class (when (urgentp event)
                                      "urgent")
                             (fmt "~A: " (pretty-date event))
                             (:a :href (link event) (str (title event)))
                             )))
          (htm
           (:li :id "nomeetings" "No upcoming meetings")))))))


;;; HTTP

(defun fetch-calendar ()
  (fetch *calendar-url* :params (params :start-min (now+ 0)
                                    :start-max (now+ 30))))

(defun fetch-calendar-document ()
  (cxml:parse (fetch-calendar) (make-builder)))

