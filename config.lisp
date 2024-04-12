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
  (asdf:oos 'asdf:load-source-op component))

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

(defun load-after-system (system &optional file)
  "A simplistic copy of Nyxt's macro of the same name."
  ;; On Guix, all the SBCL FASLs are put into read-only directories,
  ;; causing compilation errors. Using `asdf:load-source-op' helps that,
  ;; as loading from source does not cause re-compilation.
  (when (ignore-errors (asdf:oos 'asdf:load-source-op system))
    (when file
      (load file))))

;;; FIXME: *print-case* :downcase breaks some symbol-generation.
(setf ;; *print-case* :downcase
      *print-circle* nil
      *print-right-margin* (or (ignore-errors (parse-integer (uiop:getenv "COLUMNS")))
                               100)
      *print-lines* 5
      *print-length* 5
      *print-level* 3)

(load-after-system :graven-image (config "gimage.lisp"))
(load-after-system :trivial-toplevel-prompt (config "prompt.lisp"))
(load-after-system :trivial-toplevel-commands (config "commands.lisp"))

(load-after-system :trivial-gray-streams (config "talkative.lisp"))

(load-source :arrow-macros)
(use-package :arrow-macros)

(defun question-reader (stream char arg)
  (declare (ignorable char arg))
  (let ((val (read stream nil nil t)))
    (typecase val
      (keyword (apropos* val nil t))
      (symbol (format t "~&~a" (lambda-list* val)))
      (list (format t "~&~a" (documentation* (first val) (second val)))))
    (terpri)
    (values)))

(set-dispatch-macro-character
 #\# #\? #'question-reader)

(load (config "ed.lisp"))
