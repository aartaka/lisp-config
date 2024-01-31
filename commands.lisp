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
  (when asd-file
    (asdf:load-asd (etypecase asd-file
                     (string (uiop:parse-native-namestring asd-file))
                     (pathname asd-file))))
  (load-source system))

(tpl-cmd:define-command/eval (:quill :ql) (system)
  "Load an SYSTEM via Quicklisp."
  (ql:quickload system))

;; TODO: Directory change command (:cd, :chdir, :pwd, :cwd, :dir?)
;; TODO: Pager command (:page, :less, :head?)
