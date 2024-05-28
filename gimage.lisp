(in-package :cl-user)

(use-package :graven-image)

(defmethod gimage:apropos* :around (string &optional package external-only docs-too)
  (declare (ignorable string package external-only docs-too))
  (cl-user::with-useful-printing
    (call-next-method)))

(defmethod gimage:inspect* :around (object)
  (declare (ignorable object))
  (cl-user::with-useful-printing
    (call-next-method)))

(defmethod gimage:describe* :around (object &optional stream respect-methods)
  (declare (ignorable object stream respect-methods))
  (cl-user::with-useful-printing
    (call-next-method)))

(defmethod gimage:documentation* :around (object &optional doc-type)
  "Implementations throw tantrums getting nonexistent entities' docs."
  (declare (ignorable object doc-type))
  (ignore-errors (call-next-method)))

#-abcl
(gimage:dribble*)
