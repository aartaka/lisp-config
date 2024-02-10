(tpl-cmd:define-command/eval :qq (&optional code)
  "Quit properly."
  (uiop:quit (or code 0)))

(defmacro define-command/raw (&rest args)
  `(#-clozure
    tpl-cmd:define-command/string
    #+clozure
    tpl-cmd:define-command/eval
    ,@args))

(define-command/raw (:sh :!) (command)
 "Run shell command synchronously."
 (ignore-errors
   (uiop:run-program command
                     :output :interactive
                     :error-output :interactive
                     :input :interactive)))


(define-command/raw (:sha :&) (command)
  "Run shell command asynchronously."
  (ignore-errors
   (uiop:launch-program command
                        :output :interactive
                        :error-output :interactive)))

(tpl-cmd:define-command/eval (:loadsys :lsd) (system &optional asd-file)
  "Load an ASDF SYSTEM."
  (let ((system (if (asdf:find-system system nil)
                    system
                    (intern (package-name (find-package system)) :keyword))))
    (when asd-file
      (asdf:load-asd (etypecase asd-file
                       (string (uiop:parse-native-namestring asd-file))
                       (pathname asd-file))))
    (load-source system)))

(tpl-cmd:define-command/eval (:quill :ql) (system)
  "Load an SYSTEM via Quicklisp."
  (ql:quickload system))

(define-command/raw (:directory :dir) (#+clozure &optional
                                       dir)
  "(Switch to DIR, if provided) and list all the files in the current directory."
  (unless (uiop:emptyp dir)
    (uiop:chdir (merge-pathnames
                 (uiop:parse-native-namestring dir)
                 (user-homedir-pathname))))
  (format t "Directory ~a:~{~&~a/~}~{~&~a~}"
          (uiop:getcwd)
          (mapcar (lambda (d)
                    (car (last (pathname-directory d))))
                  (uiop:subdirectories (uiop:getcwd)))
          (mapcar #'file-namestring
                  (uiop:directory-files (uiop:getcwd)))))

;; TODO: Directory change command (:cd, :chdir, :pwd, :cwd, :dir?)
;; TODO: Pager command (:page, :less, :head?)
