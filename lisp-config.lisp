;;; The following lines added by ql:add-to-init-file:
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(require "asdf")
#-(or ecl abcl)
(ignore-errors (ql:quickload "cffi"))
#-(or ecl abcl)
(when (find-package "CFFI")
  (push #p"~/.guix-extra-profiles/nyxt-profile/nyxt-profile/lib/"
        (symbol-value (find-symbol "*FOREIGN-LIBRARY-DIRECTORIES*" :CFFI)))
  (push #p"~/.guix-profile/lib/"
        (symbol-value (find-symbol "*FOREIGN-LIBRARY-DIRECTORIES*" :CFFI))))
#+ecl
(require "package-locks")
#+sbcl
(require "sb-introspect")

;; (declaim (optimize speed))
(declaim (optimize (safety 3) (debug 3)))

(asdf:load-asd #p"~/git/graven-image/graven-image.asd")
(asdf:load-system :graven-image)
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

(defmethod gimage:describe* :around (object &optional (stream t) ignore-methods)
  "Ignore the `describe-object' methods."
  (declare (ignorable ignore-methods))
  (with-useful-printing
    (call-next-method object stream t)))

(defmethod gimage:documentation* :around (object &optional doc-type)
  "Implementations throw tantrums getting nonexistent entities' docs."
  (declare (ignorable object doc-type))
  (ignore-errors (call-next-method)))

;;; FIXME: *print-case* :downcase breaks some symbol-generation.
(setf ;; *print-case* :downcase
      *print-circle* nil
      *print-right-margin* (or (ignore-errors (parse-integer (uiop:getenv "COLUMNS")))
                               100))
