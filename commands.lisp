(uiop:define-package :commands
  (:use :cl :graven-image))
(in-package :commands)

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
  (uiop:run-program command :output t :error-output t)))

(define-command/raw (:shi :?) (command)
  "Run shell command synchronously and interactively.
Beware that it uses OS shell input/output and thus is not really
controllable from CL (and Talkative)."
  (uiop:run-program command
                    :output :interactive
                    :error-output :interactive
                    :input :interactive))


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
    (cl-user::load-source system)))

(define-command/eval (:quill :ql) (&rest systems)
  "Load a SYSTEM via Quicklisp."
  (ql:quickload systems))

(define-command/raw (:directory :dir) (#+clozure &optional
                                       dir)
  "(Switch to DIR, if provided) and list all the files in the current directory."
  (block dir
    (let ((resolved-dir (merge-pathnames
                         (uiop:parse-native-namestring dir)
                         (uiop:getcwd))))
      (unless (uiop:directory-exists-p resolved-dir)
        (if (yes-or-no-p* "Create a ~a directory?" dir)
            (ensure-directories-exist (uiop:ensure-directory-pathname resolved-dir))
            (return-from dir)))
      (unless (uiop:emptyp dir)
        (uiop:chdir resolved-dir)))
    (format t "Directory ~a~:[~;:~{~&~a/~}~{~&~a~}~]"
            (uiop:getcwd)
            (uiop:emptyp dir)
            (mapcar (lambda (d)
                      (car (last (pathname-directory d))))
                    (uiop:subdirectories (uiop:getcwd)))
            (mapcar #'file-namestring
                    (uiop:directory-files (uiop:getcwd))))
    (values)))

(define-command :loadrc ()
  "Reload the config file."
  (load (cl-user::config "config.lisp")))

(defvar %page-buffer nil)
(defvar %page-index)
(defun %page (args)
  (let ((args (uiop:ensure-list args)))
    (flet ((print-n-lines (n)
             "Scroll down N lines, incrementing `%page-index' in the process."
             (loop repeat (min n (- (length %page-buffer)
                                    %page-index))
                   when %page-index
                   do (format t "~&~d: ~a~%" %page-index (elt %page-buffer %page-index))
                   unless (= 1 n)
                     do (incf %page-index))))
      (typecase (first args)
        (null
         (print-n-lines (max 5 *print-lines*)))
        ((or string symbol)
         (setf %page-index 0
               %page-buffer (uiop:split-string
                             (uiop:run-program
                              (if (rest args)
                                  (mapcar (lambda (f)
                                            (string-downcase (string f)))
                                          args)
                                  (string-downcase (string (first args))))
                              :output '(:string :stripped t))
                             :separator '(#\Newline)))
         (print-n-lines 1))
        ((integer 0)
         (setf %page-index (min (first args) (1- (length %page-buffer))))
         (print-n-lines 1))
        ((integer * 0)
         (setf %page-index (max 0 (- %page-index (first args))))
         (print-n-lines 1))
        (pathname
         (setf %page-index 0
               %page-buffer (uiop:read-file-lines (first args)))
         (print-n-lines 1))
        (t
         (setf %page-buffer (uiop:split-string
                             (with-output-to-string (*standard-output*)
                               (eval `(progn ,@args)))
                             :separator '(#\Newline)))
         (print-n-lines 1))))))

(define-command (:page :pg) (&rest args)
  "Page the provided arguments' output.
- String/symbol: run ARGS as shell commands and page the output.
- Positive integer: scroll to Nth line of paged content.
- Negative integer: scroll back relative to current line.
- Pathname: scroll the file contents.
- No arguments or NIL: scroll `*print-lines*' lines down.
- Any other data type: `eval'-uate it and page the output."
  (%page args))
(define-command (:manual :man) (&rest args)
  "View the UNIX/GNU/Linux manuals.
This command merely invokes the pager, so use :page for all the
subsequent actions on the manual."
  (%page (uiop:strcat "man --pager=cat " (format nil "~{~(~a~)~^ ~}" args))))

;; TODO: Git commands (check out Shinmera's legit)
