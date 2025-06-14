(in-package :cl-user)
(use-package :trivial-toplevel-commands)

(uiop:define-package :commands
  (:use :cl :graven-image :trivial-toplevel-commands))
(in-package :commands)

(define-command/eval :qq (&optional code)
  "Quit properly"
  (uiop:quit (or code 0)))

(defmacro define-command/raw (&rest args)
  `(#-clozure
    define-command/string
    #+clozure
    define-command/eval
    ,@args))

(define-command/raw (:sh :!) (command)
 "Run shell command synchronously"
 (ignore-errors
  (uiop:run-program command :output t :error-output t)))

(define-command/raw :shi (command)
  "Run shell command synchronously and interactively
Beware that it uses OS shell input/output and thus is not really
controllable from CL (and Talkative)."
  (uiop:run-program command
                    :output :interactive
                    :error-output :interactive
                    :input :interactive))


(define-command/raw (:sha :&) (command)
  "Run shell command asynchronously"
  (ignore-errors
   (uiop:launch-program command :output t :error-output t)))

(define-command/raw (:sudo :su) (command)
  "Run the command as sudo, passing the password in"
  (ignore-errors
   (uiop:run-program (uiop:strcat "sudo " command)
                     :output t :error-output t :input :interactive)))

(define-command/eval (:loadsys :lsd) (system &optional asd-file)
  "Load an ASDF SYSTEM"
  (when asd-file
    (asdf:load-asd (uiop:merge-pathnames*
                    (etypecase asd-file
                      (string (uiop:parse-native-namestring asd-file))
                      (pathname asd-file))
                    (uiop:getcwd))))
  (cl-user::load-source system))

(define-command/eval (:quill :ql) (&rest systems)
  "Load a SYSTEM via Quicklisp"
  (ql:quickload systems :verbose t))

(define-command/raw (:directory :dir) (#+clozure &optional
                                       dir)
  "(Switch to DIR, if provided) and list all the files in the current directory"
  (block dir
    (unless (uiop:emptyp dir)
      (let* ((tilde (eql #\~ (elt dir 0)))
             (resolved-dir (merge-pathnames
                            (uiop:parse-native-namestring
                             (if tilde
                                 (subseq dir 2)
                                 dir))
                            (if tilde
                                (user-homedir-pathname)
                                (uiop:getcwd)))))
        (unless (uiop:directory-exists-p resolved-dir)
          (if (yes-or-no-p* "Create a ~a directory?" dir)
              (ensure-directories-exist (uiop:ensure-directory-pathname resolved-dir))
              (return-from dir)))
        (unless (uiop:emptyp dir)
          (uiop:chdir resolved-dir))))
    (format t "~:[Switched to d~;D~]irectory ~a~:[~;:~{~&~a/~}~{~&~a~}~]"
            (uiop:emptyp dir)
            (uiop:getcwd)
            (uiop:emptyp dir)
            (mapcar (lambda (d)
                      (car (last (pathname-directory d))))
                    (uiop:subdirectories (uiop:getcwd)))
            (mapcar #'file-namestring
                    (uiop:directory-files (uiop:getcwd))))))

(define-command :loadrc ()
  "Reload the config file"
  (load #p"cfg:config.lisp"))

(defvar %page-buffer nil)
(defvar %page-index nil)
(defun %page (args)
  (let ((args (uiop:ensure-list args)))
    (flet ((print-n-lines (n)
             "Scroll down N lines, incrementing `%page-index' in the process"
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
         (setf %page-index (max 0 (+ %page-index (first args))))
         (print-n-lines 1))
        (pathname
         (setf %page-index 0
               %page-buffer (uiop:read-file-lines (first args)))
         (print-n-lines 1))
        (t
         (setf %page-buffer (uiop:split-string
                             (with-output-to-string (*standard-output*)
                               (eval `(progn ,@args)))
                             :separator '(#\Newline))
               %page-index 0)
         (print-n-lines 1))))))

(define-command (:page :pg) (&rest args)
  "Page the provided arguments' output
- String/symbol: run ARGS as shell commands and page the output.
- Positive integer: scroll to Nth line of paged content.
- Negative integer: scroll back relative to current line.
- Pathname: scroll the file contents.
- No arguments or NIL: scroll `*print-lines*' lines down.
- Any other data type: `eval'-uate it and page the output."
  (%page args))
(define-command (:manual :man) (&rest args)
  "View the UNIX/GNU/Linux manuals
This command merely invokes the pager, so use :page for all the
subsequent actions on the manual."
  (%page (uiop:strcat "man --pager=cat " (format nil "~{~(~a~)~^ ~}" args))))

(defvar *inspected-thing* nil)

(defun %inspect (thing)
  (unless (null thing)
    (let ((val (loop for (index field value) in (trivial-inspect:fields *inspected-thing*)
                     when (or (equal thing index)
                              (equal thing field))
                       do (return value))))
      (setf *inspected-thing* (if val
                                  (setf *inspected-thing* val)
                                  thing))))
  (loop for (index key value) in (trivial-inspect:fields *inspected-thing*)
        do (cl-user::with-useful-printing
             (format t "~&~d ~s~20t = ~s" index key value))))

(define-command/eval (:inspect :in) (thing)
  "Inspect the THING or the THING-indexed field in the current thing"
  (%inspect thing))

(define-command/eval (:describe :de) (thing)
  "Describe the THING in a human-friendly way."
  (trivial-inspect:description thing t))

;; TODO: :install command to get packages on Debian/Raspberry
;; vs. GNU/Linux/Guix (via uname -a)
