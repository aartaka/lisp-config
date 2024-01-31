(defun home (path)
  (merge-pathnames path (user-homedir-pathname)))

;;; The following lines added by ql:add-to-init-file:
#-quicklisp
(let ((quicklisp-init (home "quicklisp/setup.lisp")))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(require "asdf")

(defun load-source (component)
  (asdf:oos 'asdf:load-source-op component))

#-(or ecl abcl)
(ignore-errors (ql:quickload "cffi"))
#-(or ecl abcl)
(when (find-package "CFFI")
  (push (home ".guix-extra-profiles/nyxt-profile/nyxt-profile/lib/")
        (symbol-value (find-symbol "*FOREIGN-LIBRARY-DIRECTORIES*" :CFFI)))
  (push (home ".guix-profile/lib/")
        (symbol-value (find-symbol "*FOREIGN-LIBRARY-DIRECTORIES*" :CFFI))))
#+ecl
(require "package-locks")
#+sbcl
(require "sb-introspect")
#+sbcl
(require "sb-aclrepl")

;; (declaim (optimize speed))
(declaim (optimize (safety 3) (debug 3)))

(defmacro load-after-system (system &optional file)
  "A simplistic copy of Nyxt's macro of the same name."
  ;; On Guix, all the SBCL FASLs are put into read-only directories,
  ;; causing compilation errors. Using `asdf:load-source-op' helps that,
  ;; as loading from source does not cause re-compilation.
  `(when (ignore-errors (asdf:oos 'asdf:load-source-op ,system))
     ,(when file
        `(load ,file))))

;;; FIXME: *print-case* :downcase breaks some symbol-generation.
(setf ;; *print-case* :downcase
      *print-circle* nil
      *print-right-margin* (or (ignore-errors (parse-integer (uiop:getenv "COLUMNS")))
                               100))

(load-after-system :graven-image "/home/aartaka/.config/common-lisp/gimage.lisp")
(load-after-system :trivial-toplevel-prompt "/home/aartaka/.config/common-lisp/prompt.lisp")
(load-after-system :trivial-toplevel-commands "/home/aartaka/.config/common-lisp/commands.lisp")

;; TODO: Unreadable object method and integer with *print-base* and *print-radix*.

;; (load-source :trivial-gray-streams)

;; (defclass talkative-stream (trivial-gray-streams:fundamental-character-output-stream)
;;   ((buffer :initform '())
;;    (panicky-p :initform nil :initarg :panicky-p)))

;; (defmethod trivial-gray-streams:stream-line-column ((stream talkative-stream))
;;   0)

;; (defun speak-string (string &key panicky-p)
;;   (ignore-errors
;;    (uiop:run-program
;;     `("espeak-ng" "--punct" "-s" "200"
;;                   ,@(when panicky-p
;;                       (list "-p" "70"))
;;                   ,string))))

;; (defun speak-buffer (stream)
;;   (speak-string
;;    (coerce (reverse (slot-value stream 'buffer)) 'string)
;;    :panicky-p (slot-value stream 'panicky-p))
;;   (setf (slot-value stream 'buffer) '()))

;; (defmethod trivial-gray-streams:stream-write-char ((stream talkative-stream) character)
;;   (cond
;;     ((eql #\Newline character)
;;      (speak-buffer stream))
;;     (t
;;      (push character (slot-value stream 'buffer)))))

;; (defmethod trivial-gray-streams:stream-finish-output ((stream talkative-stream))
;;   (speak-buffer stream))
;; (defmethod trivial-gray-streams:stream-force-output ((stream talkative-stream))
;;   (speak-buffer stream))
;; (defmethod trivial-gray-streams:stream-clear-input ((stream talkative-stream)))

;; (defun talkative-in (stream)
;;   (make-echo-stream stream (make-instance 'talkative-stream)))
;; (defun talkative-out (stream)
;;   (make-broadcast-stream stream (make-instance 'talkative-stream)))
;; (defun talkative-error (stream)
;;   (make-broadcast-stream stream (make-instance 'talkative-stream :panicky-p t)))

;; (setf *standard-output* (talkative-out *standard-output*)
;;       *error-output* (talkative-error *error-output*)
;;       *debug-io* (make-two-way-stream (talkative-in *debug-io*)
;;                                       (talkative-error *debug-io*))
;;       *standard-input* (talkative-in *standard-input*)
;;       *trace-output* (talkative-out *trace-output*)
;;       *query-io* (make-two-way-stream (talkative-in *query-io*)
;;                                       (talkative-out *query-io*)))
