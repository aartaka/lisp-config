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
    (load-source system)))

(define-command/eval (:quill :ql) (&rest systems)
  "Load an SYSTEM via Quicklisp."
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

(defvar *page-content* '() "Currently scrolled with :page.")
(defvar *page-index* 0 "Index into `*paged-content*'.")

(defun split-lines (string)
  (uiop:split-string string :separator '(#\Newline)))

;; TODO: Page the string if FORMS return a string instead of printing.
(define-command (:page :pg) (&rest forms)
  "Page the FORMS.

- If FORMS is a single string, send it to the shell and page program
  output.
- If FORMS is a list of Lisp forms, evaluate them and page the
  *standard-output* used while evaluating.
- If FORMS is a single pathname, page the file contents."
  (typecase (first forms)
    (integer
     (setf *page-index* (first forms))
     (format t "Paged line ~d" *page-index*))
    (null
     nil)
    ((eql t)
     (format t "Paged line ~d" *page-index*))
    ((or symbol string)
     (setf *page-content*
           (split-lines (uiop:run-program (if (rest forms)
                                              (string-downcase (string (first forms)))
                                              (mapcar (lambda (s)
                                                        (string-downcase (string s)))
                                                      forms))
                                          :output '(:string :stripped t))))
     (format t "Paging ~s, ~d lines" forms (length *page-content*)))
    (pathname
     (setf *page-content* (uiop:read-file-lines (first forms)))
     (format t "Paging ~s, ~d lines" forms (length *page-content*)))
    (cons
     (setf *page-content*
           (split-lines
            (with-output-to-string (*standard-output*)
              (eval `(progn ,@forms)))))
     (format t "Paging ~s, ~d lines" forms (length *page-content*))))
  (loop for i from *page-index*
          below (min (+ *page-index* (or *print-lines* 10))
                     (length *page-content*))
        do (format t "~&~a" (elt *page-content* i))
        do (incf *page-index*))
  (values))
