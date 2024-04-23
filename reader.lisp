(in-package :cl-user)

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
