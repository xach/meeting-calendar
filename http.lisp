;;;; http.lisp

(in-package #:meeting-calendar)



(defun date-string (universal-time)
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time universal-time 0)
    (declare (ignore second minute hour))
    (format nil "~4,'0D-~2,'0D-~2,'0D" year month day)))

(defun now+ (days)
  "Return a date string DAYS into the future. Typically either 1 or 30."
  (let ((step 86400))
    (date-string (+ (get-universal-time) (* days step)))))

(defun today ()
  "Return a universal time for today."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time (get-universal-time) 0)
    (declare (ignore second minute hour))
    (encode-universal-time 0 0 0 day month year 0)))



(define-condition http-error (error)
  ((code
    :initarg :code
    :reader http-error-code)
   (phrase
    :initarg :phrase
    :reader http-error-phrase))
  (:report (lambda (condition stream)
             (format stream "Bad HTTP status ~A (~A)"
                     (http-error-code condition)
                     (http-error-phrase condition)))))


(defun stringify (thing)
  (typecase thing
    (string thing)
    (symbol (symbol-name thing))
    (t (princ-to-string thing))))

(defun params (&rest args &key &allow-other-keys)
  "Construct an ALIST based on all keyword arguments passed to the
function. Keywords are converted to their lowercase symbol name and
values are converted to strings."
  (loop for (key value) on args by #'cddr
        when value
        collect (cons (if (symbolp key)
                          (string-downcase (symbol-name key))
                          key)
                      (stringify value))))



(defun fetch (url &key (method :get) binary params username password)
  (multiple-value-bind (response code headers uri stream closep phrase)
      (drakma:http-request url
                           :parameters params
                           :method method
                           :force-binary binary
                           :basic-authorization (and username
                                                     password
                                                     (list username password))
                           :want-stream nil)
    (declare (ignore headers uri))
    (when closep
      (close stream))
    (if (= code 200)
        response
        (error 'http-error
               :code code
               :phrase phrase))))

(defun shorten-url (url)
  (fetch "http://l1sp.org/r/new"
         :method :post
         :params (params :x "nerdo" :url url)))

