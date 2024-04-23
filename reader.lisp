(uiop:define-package :reader-macros
  (:use :cl :graven-image))
(in-package :reader-macros)

(defun question-reader (stream char arg)
  (declare (ignorable char arg))
  (let ((val (read stream nil nil t)))
    (typecase val
      ((or keyword string
           (cons (or keyword string)))
       (apropos* (first (uiop:ensure-list val))
                 (second (uiop:ensure-list val))))
      (symbol (format t "~&~a" (lambda-list* val)))
      (list (format t "~&~a" (documentation* (first val) (second val)))))
    (terpri)
    (values)))

(set-dispatch-macro-character
 #\# #\? #'question-reader)

(defun lambda-reader (stream char arg)
  (declare (ignorable char))
  (check-type arg (or null (integer 1 3))
              "optional number of args (maximum 3)")
  (let* ((form (read stream nil nil t)))
    `(lambda ,(append
               (unless arg
                 '(&optional))
               (loop for i from 1 upto (or arg 3)
                     collect (intern (make-string i :initial-element #\_) *package*)))
       ,form)))

(set-dispatch-macro-character
 #\# #\^ #'lambda-reader)
