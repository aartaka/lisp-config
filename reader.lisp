(uiop:define-package :reader-macros
  (:use :cl :graven-image))
(in-package :reader-macros)

;; TODO: hash table reader syntax like CL21

(defun question-reader (stream char arg)
  "Provide documentation/help for the form following #?.
Depends on the form:
- KEYWORD: `apropos' for the keyword name.
- (KEYWORD PACKAGE): `apropos' in PACKAGE.
- SYMBOL: Print argument list and argument types for SYMBOL-named
  function.
- (SYMBOL TYPE): Print the `documentation' for TYPEd SYMBOL."
  (declare (ignorable char arg))
  (let ((val (read stream nil nil t))
        (*print-case* :downcase))
    (typecase val
      ((or keyword string
           (cons (or keyword string)))
       (apropos (first (uiop:ensure-list val))
                (second (uiop:ensure-list val))))
      (symbol (format t "~&~a~%~a -> ~a"
                      (trivial-arguments:arglist val)
                      (nth-value 0 (trivial-arguments:argtypes val))
                      (nth-value 1 (trivial-arguments:argtypes val))))
      (list (format t "~&~a" (documentation (first val) (or (second val) 'function)))))
    (terpri)
    (values)))

(set-dispatch-macro-character
 #\# #\? #'question-reader)

(defun lambda-reader (stream char arg)
  "Create a lambda from the #^ARGS.BODY spec.
ARGS are a sequence of (maximum 3) chars to serve as (all optional) arguments.
BODY is a single valid Lisp form, possibly referring to ARGS.
ARGS and BODY are separated by a single period.

Examples:
#^x.x ;; Identity function.
#^kv.(print (list k v)) ;; Useful fun for e.g. maphash
#^.(print 'hello) ;; No-argument function
#^.nil ;; Void and nothingness"
  (declare (ignorable char arg))
  (let* ((args (loop for char = (read-char stream nil nil t)
                     until (char= char #\.)
                     collect (intern (make-string 1 :initial-element (char-upcase char)) *package*)))
         (form (read stream nil nil t)))
    `(lambda ,(append
               (unless arg
                 '(&optional))
               args)
       (declare (ignorable ,@args))
       ,form)))

(set-dispatch-macro-character
 #\# #\^ #'lambda-reader)

(defun bang-reader (stream char arg)
  "Read a shell command (until a newline) and run it.
Print the output of the command to `*standard-output*'.
When ARG is provided, only print ARG lines or less."
  (declare (ignore char))
  (let* ((arg (or arg 1000))
         (command (read-line stream))
         (output (ignore-errors
                  (uiop:run-program command :output '(:string :stripped t))))
         (lines (uiop:split-string output :separator '(#\Newline))))
    (loop for line in lines
          for i below arg
          do (format t "~&~a" line))
    (fresh-line)
    (values)))

(set-dispatch-macro-character
 #\# #\! #'bang-reader)
