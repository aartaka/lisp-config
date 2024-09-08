(in-package :cl-user)

(handler-bind ((warning #'muffle-warning))
  (#+(and sbcl sb-package-locks) sb-ext:without-package-locks
     #+(and ecl package-locks) ext:without-package-locks
     #+clisp ext:without-package-lock #+clisp ()
     #-(or (and sbcl sb-package-locks)
           (and ecl package-locks)
           clisp)
     progn
     (defmethod documentation ((x null) (doc-type (eql t)))
       "A recursion-terminating method for non-existent X.
Useful in the symbol-resolving method below."
       (declare (ignore x doc-type))
       nil)

     (defmethod documentation ((x symbol) (doc-type (eql t)))
       "A DWIM method on X resolving it against different entities."
       (macrolet ((doc (type)
                    `(documentation x (quote ,type))))
         (when x
           (or (documentation (macro-function x) t)
               (doc function)
               (documentation (fdefinition x) t)
               (documentation (symbol-function x) t)
               (doc variable)
               (doc type)
               (documentation (find-class x) t)
               (doc structure)
               (documentation (find-package x) t)
               (doc compiler-macro)
               (doc setf)
               (doc method-combination)))))

     (defmethod documentation (x (doc-type (eql 'package)))
       "A convenience method with PACKAGE doc-type.
If something can be found via `find-package', then why not resolve it?"
       (declare (ignore doc-type))
       (documentation (find-package x) t))

     (defmethod (setf documentation) (value (x null) (doc-type (eql t)))
       (declare (ignore value x doc-type))
       nil)

     (defmethod (setf documentation) (value (x symbol) (doc-type (eql t)))
       (macrolet ((doc (type)
                    `(documentation x (quote ,type)))
                  (set-doc (type)
                    `(setf (documentation x (quote ,type))
                           value)))
         (cond
           ((null x) nil)
           ((or (doc function)
                (documentation (fdefinition x) t)
                (documentation (symbol-function x) t))
            (set-doc function)
            (setf (documentation (fdefinition x) t)
                  value)
            (setf (documentation (symbol-function x) t)
                  value)
            (when (macro-function x)
              (setf (documentation (macro-function x) t)
                    value)))
           ((doc variable) (set-doc type))
           ((or (doc type)
                (documentation (find-class x) t))
            (set-doc type)
            (ignore-errors
             (setf (documentation (find-class x) t)
                   value)))
           ((doc structure)
            (set-doc structure))
           ((documentation (find-package x) t)
            (setf (documentation (find-package x) t)
                  value))
           ((doc compiler-macro) (set-doc compiler-macro))
           ((doc setf) (set-doc setf))
           ((doc method-combination) (set-doc method-combination)))))

     (defmethod (setf documentation) (value x (doc-type (eql 'package)))
       (declare (ignore doc-type))
       (setf (documentation (find-package x) t)
             value))

     (defmethod documentation :around (x doc-type)
       (declare (ignore x doc-type))
       (ignore-errors (call-next-method)))))

(#+clozure tpl-cmd:define-command/eval
 #-clozure tpl-cmd:define-command/read (:documentation :doc) (thing)
  "Print documentation for THING."
  (princ (documentation thing t)))
