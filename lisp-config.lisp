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

(asdf:load-asd (home ".config/common-lisp/graven-image/graven-image.asd"))
(load-source :graven-image)
(use-package :graven-image)

(defmacro with-useful-printing (&body body)
  `(let ((*print-case* :downcase)
         (*print-level* 2)
         (*print-lines* 1)
         (*print-length* 7)
         (*print-circle* nil))
     ,@body))

(defmethod gimage:apropos* :around (string &optional package external-only docs-too)
  (declare (ignorable string package external-only docs-too))
  (with-useful-printing
    (call-next-method)))

(defmethod gimage:inspect* :around (object)
  (declare (ignorable object))
  (with-useful-printing
    (call-next-method)))

(defmethod gimage:describe* :around (object &optional stream respect-methods)
  (declare (ignorable object stream respect-methods))
  (with-useful-printing
    (call-next-method)))

(defmethod gimage:documentation* :around (object &optional doc-type)
  "Implementations throw tantrums getting nonexistent entities' docs."
  (declare (ignorable object doc-type))
  (ignore-errors (call-next-method)))

;;; FIXME: *print-case* :downcase breaks some symbol-generation.
(setf ;; *print-case* :downcase
      *print-circle* nil
      *print-right-margin* (or (ignore-errors (parse-integer (uiop:getenv "COLUMNS")))
                               100))

(asdf:load-asd (home ".config/common-lisp/trivial-toplevel-prompt/trivial-toplevel-prompt.asd"))
(load-source :trivial-toplevel-prompt)
(trivial-toplevel-prompt:set-toplevel-prompt "~*~a~*~@[/D~d~]~*~@[/I~*~]? ")

(asdf:load-asd (home ".config/common-lisp/trivial-toplevel-commands/trivial-toplevel-commands.asd"))
(load-source :trivial-toplevel-commands)

(tpl-cmd:define-command/eval (:quit :qq) (&optional code)
  "Quit properly."
  (uiop:quit (or code 0)))

;; (asdf:load-asd (home ".config/common-lisp/trivial-gray-streams/trivial-gray-streams.asd"))
;; (load-source :trivial-gray-streams)

;; (defclass talkative-stream (trivial-gray-streams:fundamental-character-output-stream)
;;   ((buffer :initform '())))

;; (defmethod trivial-gray-streams:stream-line-column ((stream talkative-stream))
;;   0)

;; (defun speak-buffer (stream)
;;   (ignore-errors
;;     (uiop:run-program
;;      (list "espeak-ng" "--punct" "-s" "200"
;;            (coerce (reverse (slot-value stream 'buffer)) 'string))))
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

;; (defun talkative-in (stream)
;;   (make-echo-stream stream (make-instance 'talkative-stream)))
;; (defun talkative-out (stream)
;;   (make-broadcast-stream stream (make-instance 'talkative-stream)))

;; (setf *standard-output* (talkative-out *standard-output*)
;;       *debug-io* (make-two-way-stream (talkative-in *debug-io*)
;;                                       (talkative-out *debug-io*))
;;       *standard-input* (talkative-in *standard-input*)
;;       *trace-output* (talkative-out *trace-output*)
;;       *query-io* (make-two-way-stream (talkative-in *query-io*)
;;                                       (talkative-out *query-io*)))
