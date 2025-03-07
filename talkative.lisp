(uiop:define-package :talkative
  (:use :cl)
  (:export #:speak-string #:enable #:*speed*))
(in-package :talkative)

(defclass talkative-stream (trivial-gray-streams:fundamental-character-output-stream)
  ((buffer :initform '())
   (panicky-p :initform nil :initarg :panicky-p)))

(defmethod trivial-gray-streams:stream-line-column ((stream talkative-stream))
  0)

(defvar *speed* 240)

(defun speak-string (string &key panicky-p)
  (ignore-errors
   (uiop:run-program
    `("espeak-ng" "--punct"
                  "-s" ,(princ-to-string *speed*)
                  "-v" "en-us"
                  "-k" "20"
                  ,@(when panicky-p
                      (list "-p" "70"))
                  ,(if (uiop:string-prefix-p "-" string)
                       (uiop:strcat #\Space string)
                       string)))))

(defun post-process-string (string)
  (uiop:frob-substrings
   string '("#<" "#x" "#o" "#b" "0x" "#P\"" "#p\"")
   (lambda (match frob)
     (case (elt match 1)
       (#\< (funcall frob "UNREADABLE "))
       (#\x (funcall frob "HEX "))
       (#\o (funcall frob "OCTAL "))
       (#\b (funcall frob "BINARY "))
       ((#\p #\P) (funcall frob "FILE "))))))

(defun speak-buffer (stream)
  (speak-string
   (post-process-string
    (coerce (reverse (slot-value stream 'buffer)) 'string))
   :panicky-p (slot-value stream 'panicky-p))
  (setf (slot-value stream 'buffer) '()))

(defmethod trivial-gray-streams:stream-write-char ((stream talkative-stream) character)
  (cond
    ((eql #\Newline character)
     (speak-buffer stream))
    (t
     (push character (slot-value stream 'buffer)))))

(defmethod trivial-gray-streams:stream-finish-output ((stream talkative-stream))
  (speak-buffer stream))
(defmethod trivial-gray-streams:stream-force-output ((stream talkative-stream))
  (speak-buffer stream))
(defmethod trivial-gray-streams:stream-clear-input ((stream talkative-stream)))

(defun talkative-in (stream)
  (make-echo-stream stream (make-instance 'talkative-stream)))
(defun talkative-out (stream)
  (make-broadcast-stream stream (make-instance 'talkative-stream)))
(defun talkative-error (stream)
  (make-broadcast-stream stream (make-instance 'talkative-stream :panicky-p t)))

(defvar *talkative-enabled* nil)

;; ;; Noisy, not sure if it's worth it
;; (defmethod print-object :around ((object standard-object) stream)
;;   (if *talkative-enabled*
;;       (let ((representation (with-output-to-string (s)
;;                               (call-next-method object s))))
;;         (if (uiop:string-enclosed-p "#<" representation ">")
;;             (write-string (uiop:strcat "UNREADABLE "
;;                                        (subseq representation 2 (1- (length representation))))
;;                           stream)
;;             (call-next-method)))
;;       (call-next-method)))

(defun enable ()
  (setf *talkative-enabled* t
        *standard-output* (talkative-out *standard-output*)
        *error-output* (talkative-error *error-output*)
        *debug-io* (make-two-way-stream (talkative-in *debug-io*)
                                        (talkative-error *debug-io*))
        *standard-input* (talkative-in *standard-input*)
        *trace-output* (talkative-out *trace-output*)
        *query-io* (make-two-way-stream (talkative-in *query-io*)
                                        (talkative-out *query-io*)))
  nil)
