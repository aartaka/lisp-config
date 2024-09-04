(uiop:define-package :reader-macros
  (:use :cl :graven-image))
(in-package :reader-macros)

;; TODO: hash table reader syntax like CL21

(defun question-reader (stream char arg)
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
  (declare (ignorable char))
  (check-type arg (or null (integer 1 3))
              "optional number of args (maximum 3)")
  (let* ((args (loop for char = (read-char stream nil nil t)
                     until (char= char #\.)
                     collect (intern (make-string 1 :initial-element (char-upcase char)) *package*)))
         (extra-args (when arg
                       (loop for i below (- arg (length args))
                             collect (gensym))))
         (form (read stream nil nil t)))
    `(lambda ,(append
               (unless arg
                 '(&optional))
               (append args extra-args))
       (declare (ignorable ,@args ,@extra-args))
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
