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
  (labels ((find-str (str form)
             (find str form :test #'equal))
           (fmt-sym (&rest args)
             (intern (apply #'format nil args) *package*)))
    (let* ((form (read stream nil nil t))
           (form-list (uiop:ensure-list form))
           (arg (or arg
                    (cond
                      ((find-str "_9" form-list) 9)
                      ((find-str "_8" form-list) 8)
                      ((find-str "_7" form-list) 7)
                      ((find-str "_6" form-list) 6)
                      ((find-str "_5" form-list) 5)
                      ((find-str "_4" form-list) 4)
                      ((or (find-str "_3" form-list)
                           (find-str "___" form-list))
                       3)
                      ((or (find-str "_2" form-list)
                           (find-str "__" form-list))
                       2)
                      ((or (find-str "_1" form-list)
                           (find-str "_" form-list))
                       1)
                      (t 9))
                    1)))
      `(lambda ,(loop for i from 1 upto 9
                      collect (fmt-sym "_~d" i)
                        into args
                      finally (return (append '(&optional) args
                                              `(&aux ,(list (fmt-sym "_" i) (fmt-sym "_1" i))
                                                     ,(list (fmt-sym "__" i) (fmt-sym "_2" i))
                                                     ,(list (fmt-sym "___" i) (fmt-sym "_3" i))))))
         ,form))))

(set-dispatch-macro-character
 #\# #\^ #'lambda-reader)
