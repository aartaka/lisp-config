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

(defun print-line (&optional detail)
  (let ((*print-lines* 1)
        (*print-length* 10)
        (*print-level* 3)
        (*print-case* :downcase))
    (when %ed-buffer
      (format t "~&~:[~*~a~;~d: ~s~]"
              detail %ed-index (elt %ed-buffer %ed-index)))))

(defun edit (&rest to-edit)
  (let ((head (first to-edit)))
    (typecase head
      (pathname
       (let ((lisp-file-p (member (pathname-type %ed-object)
                                  '("lisp" "lsp" "scm") ;; Scheme!?
                                  :test #'string=)))
         (setf %ed-object head
               %ed-index 0
               %ed-buffer (funcall (if lisp-file-p
                                       #'uiop:read-file-forms
                                       #'uiop:read-file-lines)
                                   (uiop:merge-pathnames* (first to-edit) (uiop:getcwd))
                                   :if-does-not-exist :create))
         ;; So that it's saved with the right set of symbols.
         (when lisp-file-p
           (setf *package* (find-package
                            (or (second (find 'in-package (remove-if #'atom %ed-buffer)
                                              :key #'first))
                                :cl-user))))))
      ((or string symbol)
       (unless (uiop:emptyp head)
         (setf %ed-index 0
               %ed-buffer (split-lines
                           (uiop:run-program
                            (if (rest to-edit)
                                (mapcar (lambda (f)
                                          (string-downcase (string f)))
                                        to-edit)
                                (first to-edit))
                            :output '(:string :stripped t))))))
      ((or (cons (eql function))
           function)
       (let ((fn (typecase head
                   (cons (fdefinition (second head)))
                   (function head))))
         (setf %ed-object fn
               %ed-index 0
               %ed-buffer (gimage:function-lambda-expression* fn))))
      (cons
       (setf %ed-index 0
             %ed-buffer (eval `(progn ,@to-edit))))
      (integer
       (setf %ed-index
             (max 0 (if (minusp head)
                        (- %ed-index head)
                        (min head (length %ed-buffer)))))))
    (print-line)))

(define-command (:edit :ed) (&rest to-edit)
  "Edit the TO-EDIT data.
TO-EDIT might be one of:
- String or pathname to edit the file it points to.
- List of forms to evaluate and edit the `*standard-output*' of.
- An integer to move to an indexed line in editor buffer."
  (apply #'edit to-edit))

(define-command (:eject :ej) (&optional that-many-lines)
  "Remove THAT-MANY-LINES starting from (and including) the current one.
EJECT: put out or expel from a place."
  (setf %ed-clipboard (subseq %ed-buffer %ed-index (min (+ %ed-index (or that-many-lines 1))
                                                        (length %ed-buffer)))
        %ed-buffer (append (subseq %ed-buffer 0 %ed-index)
                           (nthcdr (+ %ed-index (or that-many-lines 1)) %ed-buffer))))

(define-command (:egress :eg) (&optional before)
  "Paste the last :ekill-ed text to BEFORE or after the current line.
EGRESS: the reappearance of a celestial body after an eclipse"
  (setf %ed-buffer
        (append (subseq %ed-buffer 0 (if before
                                         %ed-index
                                         (1+ %ed-index)))
                %ed-clipboard
                (subseq %ed-buffer (if before
                                       %ed-index
                                       (1+ %ed-index))))))

(define-command (:eik :ei) (&optional index)
  "Descend into the current indexed (or form at INDEX) form to edit it.
EIK: to get with great difficulty."
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
  "Get back THAT-MANY-LEVELS to previous editor state, replacing the previously edited part.
ESCAPE: cut and run."
  (buffer-up (or that-many-levels 1))
  (print-line))

(defun rec-search (thing)
  (let ((found-position (position-if (lambda (form)
                                       (equal form thing))
                                     %ed-buffer)))
    (if found-position
        (throw 'found (setf %ed-index found-position))
        (loop for form in %ed-buffer
              for index from 0
              when (listp form)
                do (setf %ed-stack (append (list %ed-buffer index) %ed-stack)
                         %ed-buffer (elt %ed-buffer index)
                         %ed-index 0)
                and do (rec-search thing)
                and do (setf %ed-buffer (pop %ed-stack)
                             %ed-index (pop %ed-stack))))))

(define-command (:eavesdrop :ea) (to-search)
  "Find the TO-SEARCH in the editor buffer.
If in line mode, search the text across lines.
If in s-expressions mode, recursively search for TO-SEARCH in forms.
EAVESDROP: listen without the speaker's knowledge."
  (print to-search)
  (if (every #'stringp %ed-buffer)
      (setf %ed-index
            (or (position-if (lambda (line)
                               (search (princ-to-string to-search) line :test #'equalp))
                             %ed-buffer
                             :start (1+ %ed-index))
                %ed-index))
      (catch 'found
        (rec-search to-search)))
  (print-line))

(defun forms-or-read (forms)
  (if (uiop:emptyp forms)
      (list (read *standard-input* nil nil))
      (uiop:ensure-list forms)))

;; TODO: Better behavior for Lisp vs. Line forms.
(define-command (:effuse :ef) (&rest forms)
  "Add new form/FORMS after the current line.
EFFUSE: flow or spill forth."
  (let ((lines (forms-or-read forms)))
    (setf %ed-buffer
          (append (when %ed-buffer
                    (subseq %ed-buffer 0 (1+ %ed-index)))
                  lines
                  (when %ed-buffer
                    (subseq %ed-buffer (1+ %ed-index)))))
    (incf %ed-index)))

(define-command (:embed :em) (&rest forms)
  "Add new form/FORMS before the current line.
EMBED: attach to."
  (let ((lines (forms-or-read forms)))
    (setf %ed-buffer
          (append (subseq %ed-buffer 0 %ed-index)
                  lines
                  (subseq %ed-buffer %ed-index)))))

;; TODO: :err?
(define-command (:erase :er) (&rest forms)
  "Replace the part of current contents with new form/FORMS.
ERASE: remove by or as if by rubbing or erasing."
  (let ((lines (forms-or-read forms)))
    (setf %ed-buffer
          (append (subseq %ed-buffer 0 %ed-index)
                  lines
                  (nthcdr (+ %ed-index (length lines)) %ed-buffer)))))

;; TODO: slurp + barf

(define-command (:enact :en) (&optional object)
  "Save the contents of the editor buffer to OBJECT or current edited object.
ENACT: represent or perform as if in a play."
  (typecase (or object %ed-object)
    (pathname
     (with-open-file (s %ed-object
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create)
       (let ((*print-lines* nil)
             (*print-length* nil)
             (*print-level* nil)
             (*print-case* :downcase))
         (if (member (pathname-type %ed-object)
                     '("lisp" "lsp" "scm") ;; Scheme!?
                     :test #'string=)
             (progn
               ;; Arbitrarily big number. No one will have a 100-levels deep
               ;; Lisp form, right? Right?
               (buffer-up 100)
               (loop for form in %ed-buffer
                     do (let ((*print-escape* t))
                          (format s "~&~s~%~%" form))))
             (loop for string in %ed-buffer
                   do (if (stringp string)
                          (write-sequence string s)
                          (prin1 string s))
                   do (terpri s))))))
    (function
     (buffer-up 100)
     (eval `(setf (fdefinition (quote ,(gimage:function-name* %ed-object)))
                  ,%ed-buffer)))))

;; TODO: :echo? ECHO: call to mind.
(define-command (:examine :ex) (&optional details)
  "Print the current line.
EXAMINE: observe, check out, and look over carefully or inspect."
  (print-line details))

(define-command (:eye :ey) (&optional lines details)
  "Scroll the editor buffer down LINES (or `*print-lines*') times, printing them with DETAILs.
EYE: look at."
  (loop for i below (or lines *print-lines* 5)
        while (<= %ed-index (1- (length %ed-buffer)))
        do (print-line details)
        do (incf %ed-index))
  (when (< %ed-index (1- (length %ed-buffer)))
    (print-line details))
  (setf %ed-index (min (1- (length %ed-buffer)) %ed-index)))

(define-command (:eval :ev) (&rest forms)
  "Evaluate the current indexed form or FORM."
  (print (eval (if forms
                   `(progn ,@forms)
                   (elt %ed-buffer %ed-index)))))


(defmacro define-command/raw (&rest args)
  `(#-clozure
    define-command/string
    #+clozure
    define-command/eval
    ,@args))
(#-clozure define-command/string #+clozure define-command/eval (:manual :man) (args)
 "`edit' the man-ual of ARGS to scroll and read it."
 (edit (uiop:strcat "man --pager=cat " args)))
