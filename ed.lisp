(uiop:define-package :editor
  (:use :cl :trivial-toplevel-commands))
(in-package :editor)

(defun split-lines (string)
  (uiop:split-string string :separator '(#\Newline)))

(defvar %ed-object nil)
(defvar %ed-buffer nil)
(defvar %ed-stack '())
(defvar %ed-index 0)
(defvar %ed-clipboard nil)

(defun print-line ()
  (when %ed-buffer
    (format t "~&~d: ~s"
            %ed-index (elt %ed-buffer %ed-index))))

(define-command (:edit :ed) (&rest to-edit)
  "Edit the TO-EDIT data.
TO-EDIT might be one of:
- String or pathname to edit the file it points to.
- List of forms to evaluate and edit the `*standard-output*' of.
- An integer to move to an indexed line in editor buffer."
  (let ((head (first to-edit)))
    (typecase head
      (pathname
       (setf %ed-object head
             %ed-index 0
             %ed-buffer (uiop:read-file-lines
                         (uiop:merge-pathnames* (first to-edit) (uiop:getcwd))
                         :if-does-not-exist :create)))
      ((or string symbol)
       (unless (uiop:emptyp head)
         (setf %ed-index 0
               %ed-buffer (split-lines
                           (uiop:run-program
                            (mapcar (lambda (f)
                                      (string-downcase (string f)))
                                    to-edit)
                            :output '(:string :stripped t))))))
      ((or (cons (eql function))
           function)
       (setf %ed-object (typecase head
                          (cons (fdefinition (second head)))
                          (function head))
             %ed-index 0
             %ed-buffer (gimage:function-lambda-expression* (second head))))
      (cons
       (setf %ed-index 0
             %ed-buffer (eval `(progn ,@to-edit))))
      (integer
       (setf %ed-index
             (max 0 (if (minusp head)
                        (- %ed-index head)
                        (min head (length %ed-buffer)))))))
    (print-line)))

(define-command (:eject :ej) (&optional that-many-lines)
  "Remove THAT-MANY-LINES starting from (and including) the current one."
  (setf %ed-clipboard (subseq %ed-buffer %ed-index (min (+ %ed-index that-many-lines)
                                                        (length %ed-buffer)))
        %ed-buffer (append (subseq %ed-buffer 0 %ed-index)
                           (nthcdr (+ %ed-index (or that-many-lines 1)) %ed-buffer))))

(define-command (:egress :eg) (&optional before)
  "Paste the last :ekill-ed text to BEFORE or after the current line."
  (setf %ed-buffer
        (append (subseq %ed-buffer 0 (if before
                                         %ed-index
                                         (1+ %ed-index)))
                %ed-clipboard
                (subseq %ed-buffer (if before
                                       %ed-index
                                       (1+ %ed-index))))))

(define-command (:eik :ei) (&optional index)
  "Descend into the current indexed (or form at INDEX) form to edit it."
  (let ((index (or index %ed-index)))
    (when (listp (elt %ed-buffer index))
      (setf %ed-stack (append (list %ed-buffer index) %ed-stack)
            %ed-buffer (elt %ed-buffer index)
            %ed-index 0)
      (print-line))))

(defun buffer-up (that-much-levels)
  (loop repeat that-much-levels
        while %ed-stack
        for buffer = (pop %ed-stack)
        for index = (pop %ed-stack)
        do (setf (elt buffer index) %ed-buffer
                 %ed-buffer buffer
                 %ed-index index)))

(define-command (:escape :es) (&optional that-many-levels)
  "Get back to the previous editor state, replacing the previously edited part.
In case there's THAT-MANY-LEVELS, pop several levels up."
  (buffer-up (or that-many-levels 1))
  (print-line))

;; TODO: :eavesdrop (search)

(defun forms-or-read (forms)
  (if (uiop:emptyp forms)
      (list (read *standard-input* nil nil))
      (uiop:ensure-list forms)))

;; TODO: Better behavior for Lisp vs. Line forms.
(define-command (:effuse :ef) (&rest forms)
  "Add new text after the current line."
  (let ((lines (forms-or-read forms)))
    (setf %ed-buffer
          (append (when %ed-buffer
                    (subseq %ed-buffer 0 (1+ %ed-index)))
                  lines
                  (when %ed-buffer
                    (subseq %ed-buffer (1+ %ed-index)))))
    (incf %ed-index)))

(define-command (:embed :em) (&rest forms)
  "Add new text before the current line."
  (let ((lines (forms-or-read forms)))
    (setf %ed-buffer
          (append (subseq %ed-buffer 0 %ed-index)
                  lines
                  (subseq %ed-buffer %ed-index)))))

(define-command (:enact :en) (&optional object)
  "Save the contents of the editor buffer to OBJECT or current edited object."
  (typecase (or object %ed-object)
    (pathname
     (with-open-file (s %ed-object :direction :output)
       (if (member (pathname-type %ed-object)
                   '("lisp" "lsp" "scm") ;; Scheme!?
                   :test #'string=)
           (progn
             ;; Arbitrarily big number. No one will have a 100-levels deep
             ;; Lisp form, right? Right?
             (buffer-up 100)
             (loop for form in %ed-buffer
                   do (let ((*print-lines* nil)
                            (*print-length* nil)
                            (*print-level* nil)
                            (*print-case* :downcase))
                        (prin1 form s))))
           (loop for string in %ed-buffer
                 do (write-sequence (string string) s)
                 do (terpri s)))))
    (function
     (buffer-up 100)
     (compile (gimage:function-name* %ed-object) %ed-buffer))))

(define-command (:examine :ex) ()
  "Print the current line."
  (print-line))

(define-command (:eye :ey) ()
  "Scroll the editor buffer down `*print-lines*' times, printing them."
  (loop for i below (or *print-lines* 5)
        while (<= %ed-index (1- (length %ed-buffer)))
        do (print-line)
        do (incf %ed-index))
  (setf %ed-index (min (1- (length %ed-buffer)) %ed-index)))

(define-command (:etch :et) ()
  "Print the current line number."
  (print %ed-index))
