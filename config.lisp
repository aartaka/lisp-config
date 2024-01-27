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

(load-source :trivial-toplevel-prompt)
(trivial-toplevel-prompt:set-toplevel-prompt "~*~a~*~@[/D~d~]~*~@[/I~*~]? ")

(load-source :trivial-toplevel-commands)

(tpl-cmd:define-command/eval :qq (&optional code)
  "Quit properly."
  (uiop:quit (or code 0)))

#-clozure
(tpl-cmd:define-command/string (:sh :!) (command)
  "Run shell command synchronously."
  (ignore-errors (uiop:run-program
                  command
                  :output :interactive
                  :error-output :interactive
                  :input :interactive)))

#-clozure
(tpl-cmd:define-command/string (:sha :&) (command)
  "Run shell command asynchronously."
  (ignore-errors
    (uiop:launch-program
     command
     :output t :error-output t)))

(tpl-cmd:define-command/eval (:loadsys :lsd) (system)
  "Load an ASDF SYSTEM."
  (load-source system))

(tpl-cmd:define-command/eval (:quill :ql) (system)
  "Load an SYSTEM via Quicklisp."
  (ql:quickload system))

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
