(in-package :cl-user)

(use-package :graven-image)

(defmethod gimage:apropos* :around (string &optional package external-only docs-too)
  (declare (ignorable string package external-only docs-too))
  (cl-user::with-useful-printing
    (call-next-method)))

#-abcl
(gimage:dribble*)
