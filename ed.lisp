(uiop:define-package :editor
  (:use :cl :trivial-toplevel-commands))
(in-package :editor)

(defmacro define-command/raw (&rest args)
  `(#-clozure
    define-command/string
    #+clozure
    define-command/eval
    ,@args))

(defun split-lines (string)
  (uiop:split-string string :separator '(#\Newline)))

(defvar %ed-object nil)
(defvar %ed-buffer nil)
(defvar %ed-stack '())
(defvar %ed-index 0)
(defvar %ed-clipboard nil)

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
                         (uiop:merge-pathnames* (first to-edit) (uiop:getcwd)))))
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
             %ed-buffer (split-lines
                         (with-output-to-string (*standard-output*)
                           (eval `(progn ,@to-edit))))))
      (integer
       (setf %ed-index
             (max 0 (if (minusp head)
                        (- %ed-index head)
                        (min head (length %ed-buffer)))))))
    (print-line nil)))

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

;; Lisp-only.
(define-command (:eik :ei) (&optional index)
  "Descend into the current indexed (or form at INDEX) form to edit it."
  (let ((index (or index %ed-index)))
    (when (listp (elt %ed-buffer %ed-index))
      (setf %ed-stack (append (list %ed-buffer %ed-index) %ed-stack)
            %ed-buffer (elt %ed-buffer %ed-index)
            %ed-index 0))))

(define-command (:escape :es) (&optional that-much-levels)
  "Get back to the previous editor state, replacing the previously edited part.
In case there's THAT-MUCH-LEVELS, pop several levels up."
  (destructuring-bind (buffer index)
      (loop repeat (or that-much-levels 1)
            while %ed-stack
            for buffer = (pop %ed-stack)
            for index = (pop %ed-stack)
            finally (return (list buffer index)))
      (when index
        (setf (elt buffer index) %ed-buffer
              %ed-index index))
      (setf %ed-buffer buffer)
      (print-line)))

;; TODO: Lisp.
(let ((%query% nil))
  (define-command (:eavesdrop :ea) (&optional query)
    "Search for QUERY (or last used query, if not provided) in the buffer."
    (setf %ed-index (or (position-if (lambda (line)
                                       (search (or query %query%) line :test #'equal))
                                     %ed-buffer
                                     :start %ed-index)
                        %ed-index)
          %query% (or query %query%))))

(defun lines-or-read (lines)
  (if (uiop:emptyp lines)
      (loop for line = (read-line *standard-input* nil nil)
            until (string-equal "." line)
            collect line)
      (uiop:ensure-list lines)))

;; TODO: Better behavior for Lisp vs. Line forms.
(define-command/raw (:effuse :ef) (&rest forms)
  "Add new text after the current line."
  (let ((lines (lines-or-read forms)))
    (setf %ed-buffer
          (append (subseq %ed-buffer 0 (1+ %ed-index))
                  lines
                  (subseq %ed-buffer (1+ %ed-index))))
    (incf %ed-index)))

;; TODO: Better behavior for Lisp vs. Line forms.
(define-command/raw (:embed :em) (&rest forms)
  "Add new text before the current line."
  (let ((lines (lines-or-read forms)))
    (setf %ed-buffer
          (append (subseq %ed-buffer 0 (max 0 (1- %ed-index)))
                  lines
                  (subseq %ed-buffer (max 0 (1- %ed-index)))))))

;; TODO: Lisp
(define-command (:enact :en) (&optional object)
  "Save the contents of the editor buffer to OBJECT or current edited object."
  (when file
    (setf %ed-file file))
  (with-open-file (s %ed-file)
                  (dolist (line %ed-buffer)
                    (format s "~a~%" line))))

(defun print-line (detail)
  (format t "~&~:[~*~a~;~d: ~s~]"
          detail %ed-index (elt %ed-buffer %ed-index)))

(define-command (:examine :ex) (&optional detail)
  "Print the current line depending on DETAIL.
DETAIL is one of
- T for listing with line number and standard printing.
- NIL (default) for regular human-readable printing."
  (print-line detail))

(define-command (:eye :ey) (&optional detail)
  "Scroll the editor buffer down `*print-lines*' times, printing them.
Printing depends on DETAIL, as in :eprint."
  (loop for i below (or *print-lines* 5)
        while (<= %ed-index (1- (length %ed-buffer)))
        do (print-line detail)
        do (incf %ed-index))
  (setf %ed-index (min (1- (length %ed-buffer)) %ed-index)))

(define-command (:etch :et) ()
  "Print the current line number."
  (print %ed-index))
