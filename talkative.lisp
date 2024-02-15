(in-package :cl-user)

;; TODO: Unreadable object method and integer with *print-base* and *print-radix*.

(defclass talkative-stream (trivial-gray-streams:fundamental-character-output-stream)
  ((buffer :initform '())
   (panicky-p :initform nil :initarg :panicky-p)))

(defmethod trivial-gray-streams:stream-line-column ((stream talkative-stream))
  0)

(defun speak-string (string &key panicky-p)
  (ignore-errors
   (uiop:run-program
    `("espeak-ng" "--punct"
                  "-s" "200"
                  "-v" "en-us"
                  ,@(when panicky-p
                      (list "-p" "70"))
                  ,string))))

(defun speak-buffer (stream)
  (speak-string
   (coerce (reverse (slot-value stream 'buffer)) 'string)
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

(defun talkative-enable ()
  (setf *standard-output* (talkative-out *standard-output*)
        *error-output* (talkative-error *error-output*)
        *debug-io* (make-two-way-stream (talkative-in *debug-io*)
                                        (talkative-error *debug-io*))
        *standard-input* (talkative-in *standard-input*)
        *trace-output* (talkative-out *trace-output*)
        *query-io* (make-two-way-stream (talkative-in *query-io*)
                                        (talkative-out *query-io*))))
