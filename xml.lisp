;;;; xml.lisp

(in-package #:meeting-calendar)

(defvar *atom* "http://www.w3.org/2005/Atom")
(defvar *gd* "http://schemas.google.com/g/2005")

(defun attribute-matches (&key name (ns "") value)
  (if value
      (lambda (node)
        (equal (attribute-value node name ns) value))
      (lambda (node)
        (not (null (attribute-value node name ns))))))

(defun all-attributes-match (attributes)
  (let ((funs (mapcar (lambda (attributes)
                        (destructuring-bind (name &optional value (ns ""))
                            attributes
                          (attribute-matches :name name :value value
                                             :ns ns)))
                      attributes)))
    (lambda (node)
      (every (lambda (fun) (funcall fun node)) funs))))

(defun node-matches (&key name (ns "") attributes)
  (let ((attributes-fun (if attributes
                            (all-attributes-match attributes)
                            (constantly t))))
    (lambda (node)
      (and (typep node 'element)
           (equal (local-name node) name)
           (equal (namespace-uri node) ns)
           (funcall attributes-fun node)))))

(defun node-text (node)
  (with-output-to-string (stream)
    (do-children (child node)
      (write-string (data child) stream))))

(defun first-attribute (pred source attribute-name &optional (attribute-ns ""))
  (map-recursively (lambda (node)
                     (when (funcall pred node)
                       (return-from first-attribute
                         (attribute-value node attribute-name attribute-ns))))
                   source))

(defun first-text (pred source)
  (map-recursively (lambda (node)
                     (when (funcall pred node)
                       (return-from first-text (node-text node))))
                   source))

(defun child-contents (doc name uri)
  (let ((child (find-child-if (of-name name uri) doc)))
    (when child
      (node-text child))))

(defun recurrent-entry-p (entry)
  (funcall (stp:of-name "entryLink" *gd*)
           (stp:parent entry)))

(defun map-matches (pred fun source)
  (let ((result '()))
    (map-recursively (lambda (node)
                       (when (funcall pred node)
                         (push (funcall fun node) result)))
                     source)
    (nreverse result)))
