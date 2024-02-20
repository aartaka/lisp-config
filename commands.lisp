(in-package :cl-user)

(use-package :trivial-toplevel-commands)

(define-command/eval :qq (&optional code)
  "Quit properly."
  (uiop:quit (or code 0)))

(defmacro define-command/raw (&rest args)
  `(#-clozure
    define-command/string
    #+clozure
    define-command/eval
    ,@args))

(define-command/raw (:sh :!) (command)
 "Run shell command synchronously."
 (ignore-errors
   (uiop:run-program command
                     :output :interactive
                     :error-output :interactive
                     :input :interactive)))


(define-command/raw (:sha :&) (command)
  "Run shell command asynchronously."
  (ignore-errors
   (uiop:launch-program command
                        :output :interactive
                        :error-output :interactive)))

(define-command/eval (:loadsys :lsd) (system &optional asd-file)
  "Load an ASDF SYSTEM."
  (let ((system (if (asdf:find-system system nil)
                    system
                    (intern (package-name (find-package system)) :keyword))))
    (when asd-file
      (asdf:load-asd (etypecase asd-file
                       (string (uiop:parse-native-namestring asd-file))
                       (pathname asd-file))))
    (load-source system)))

(define-command/eval (:quill :ql) (system)
  "Load an SYSTEM via Quicklisp."
  (ql:quickload system))

(define-command/raw (:directory :dir) (#+clozure &optional
                                       dir)
  "(Switch to DIR, if provided) and list all the files in the current directory."
  (unless (uiop:emptyp dir)
    (uiop:chdir (merge-pathnames
                 (uiop:parse-native-namestring dir)
                 (user-homedir-pathname))))
  (format t "Directory ~a:~{~&~a/~}~{~&~a~}"
          (uiop:getcwd)
          (mapcar (lambda (d)
                    (car (last (pathname-directory d))))
                  (uiop:subdirectories (uiop:getcwd)))
          (mapcar #'file-namestring
                  (uiop:directory-files (uiop:getcwd)))))

;; TODO: Page the string if FORMS return a string instead of printing.
(define-command (:page :pg) (&rest forms)
  "Page the FORMS.

- If FORMS is a single string, send it to the shell and page program
  output.
- If FORMS is a list of Lisp forms, evaluate them and page the
  *standard-output* used while evaluating."
  (let* ((to-scroll 10)
         (output (with-output-to-string (*standard-output*)
                   (typecase (first forms)
                     (string (uiop:run-program (first forms) :output t))
                     (t (eval `(progn ,@forms))))))
         (lines (uiop:split-string output :separator '(#\Newline))))
    (labels ((print-page ()
               (when lines
                 (loop for i below (min to-scroll (length lines))
                       do (format t "~&~a" (elt lines i))
                       finally (setf lines (nthcdr to-scroll lines))))))
      (loop initially (print-page)
            while lines
            for read = (read-line)
            when (uiop:emptyp read)
              do (format t "~&~a" (first lines))
              and do (setf lines (rest lines))
            else when (char= #\Space (elt read 0))
                   do (print-page)
            else when (char-equal #\q (elt read 0))
                   do (return nil)))))

