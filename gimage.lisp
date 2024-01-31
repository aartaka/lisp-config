(use-package :graven-image)

(defmacro with-useful-printing (&body body)
  `(let ((*print-case* :downcase)
         (*print-level* 2)
         (*print-lines* 1)
         (*print-length* 7)
         (*print-circle* nil))
     ,@body))

(defmethod gimage:apropos* :around (string &optional package external-only docs-too)
  (declare (ignorable string package external-only docs-too))
  (with-useful-printing
    (call-next-method)))

(defmethod gimage:inspect* :around (object)
  (declare (ignorable object))
  (with-useful-printing
    (call-next-method)))

(defmethod gimage:describe* :around (object &optional stream respect-methods)
  (declare (ignorable object stream respect-methods))
  (with-useful-printing
    (call-next-method)))

(defmethod gimage:documentation* :around (object &optional doc-type)
  "Implementations throw tantrums getting nonexistent entities' docs."
  (declare (ignorable object doc-type))
  (ignore-errors (call-next-method)))
