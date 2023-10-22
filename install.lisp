(in-package :cl-user)
(require "asdf")

;; For prettier printing.
(setf *print-case* :downcase)

(defmacro with-open-override ((var file) &body body)
  "A small shortcut for overridable file writing."
  `(with-open-file (,var ,file
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
     ,@body))

;; All config load the same file.
(dolist (file (list #p"~/.sbclrc" #p"~/.ccl-init.lisp" #p"~/.eclrc" #p"~/.clisprc.lisp" #p"~/.abclrc"
                    ;; Allegro:
                    #p"~/.clinit.cl"))
  (with-open-override (f file)
    (format f "~s" '(load #p"~/.config/common-lisp/lisp-config.lisp"))))

;; Completions for every impl when run with readline.
(dolist (impl-executable '("sbcl" "ccl" "ecl" "abcl" "clisp" "alisp"))
  (with-open-override (f (uiop:merge-pathnames*
                          (uiop:strcat "." impl-executable "_completions")
                          (user-homedir-pathname)))
    (flet ((echo-freshline (file)
             (princ (uiop:read-file-string file ) f)
             (fresh-line f)))
      (echo-freshline #p"~/.config/common-lisp/standard-completions.txt")
      (echo-freshline #p"~/.config/common-lisp/keyword-completions.txt")
      (echo-freshline #p"~/.config/common-lisp/gimage-completions.txt")
      (echo-freshline #p"~/.config/common-lisp/uiop-completions.txt")
      (echo-freshline #p"~/.config/common-lisp/asdf-completions.txt"))))

;; TODO: ~/.inputrc
