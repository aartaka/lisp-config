(in-package :cl-user)

(defun home (path)
  (merge-pathnames path (user-homedir-pathname)))

(defun config (path)
  "Resolve the PATH against Lisp config dir.
Useful for dependency-based config files."
  (merge-pathnames path (home ".config/common-lisp/")))

;;; The following lines added by ql:add-to-init-file:
#-quicklisp
(let ((quicklisp-init (home "quicklisp/setup.lisp")))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(require "asdf")

(defun load-source (component)
  (asdf:oos 'asdf:load-source-op component :verbose t))

#-(or ecl abcl)
(ignore-errors (ql:quickload "cffi"))
#-(or ecl abcl)
(when (find-package "CFFI")
  (push (home ".guix-extra-profiles/nyxt-profile/nyxt-profile/lib/")
        (symbol-value (find-symbol "*FOREIGN-LIBRARY-DIRECTORIES*" :CFFI)))
  (push (home ".guix-profile/lib/")
        (symbol-value (find-symbol "*FOREIGN-LIBRARY-DIRECTORIES*" :CFFI))))
#+ecl
(require "package-locks")
#+sbcl
(require "sb-introspect")
#+sbcl
(require "sb-aclrepl")

;; (declaim (optimize speed))
(declaim (optimize (safety 3) (debug 3)))

(defun load-after-system (system &rest files)
  "A simplistic copy of Nyxt's macro of the same name"
  ;; On Guix, all the SBCL FASLs are put into read-only directories,
  ;; causing compilation errors. Using `asdf:load-source-op' helps that,
  ;; as loading from source does not cause re-compilation.
  (when (ignore-errors (asdf:oos 'asdf:load-source-op system :verbose t))
    (when files
      (mapcar #'load files))))

;;; FIXME: *print-case* :downcase breaks some symbol-generation.
;;; FIXME: same for lines/length/level—they break too much code
(setf
 ;; *print-case* :downcase
 ;; *print-lines* 5
 ;; *print-length* 5
 ;; *print-level* 3
 *print-circle* nil
 *print-right-margin* (or (ignore-errors (parse-integer (uiop:getenv "COLUMNS")))
                          70))

(defmacro with-useful-printing (&body body)
  `(let ((*print-case* :downcase)
         (*print-level* 2)
         (*print-lines* 1)
         (*print-length* 5)
         (*print-circle* nil))
     ,@body))

(load-after-system :graven-image (config "gimage.lisp"))
(load-after-system :trivial-toplevel-prompt (config "prompt.lisp"))
(load-after-system :trivial-inspect)
(load-after-system :trivial-toplevel-commands
                   (config "commands.lisp")
                   (config "ed.lisp")
                   (config "version-control.lisp")
                   (config "documentation.lisp"))

(load-after-system :trivial-gray-streams (config "talkative.lisp"))

(load-source :arrow-macros)
(use-package :arrow-macros)

(load-after-system :trivial-arguments (config "reader.lisp"))

(defmacro with ((&rest vars+bindings) &body body)
  (labels ((recur (vars+bindings body)
             (cond
               ((and vars+bindings
                     (symbolp (first vars+bindings)))
                `(let ((,@(subseq vars+bindings 0 2)))
                   (declare (ignorable ,(first vars+bindings)))
                   ,(recur (cddr vars+bindings)
                           body)))
               ((and vars+bindings
                     (listp (first vars+bindings)))
                `(multiple-value-bind ,(first vars+bindings)
                     ,(second vars+bindings)
                   (declare (ignorable ,@(first vars+bindings)))
                   ,(recur (cddr vars+bindings)
                           body)))
               (t `(progn ,@body)))))
    (recur vars+bindings body)))
