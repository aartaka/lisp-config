(uiop:define-package :version-control
  (:use :cl :trivial-toplevel-commands))
(in-package :version-control)

(define-command (:vivisect :vi) ()
  "Show the diff of the last commit.
VIVISECT: cut (a body) open while still alive."
  (uiop:run-program
   (list "git" "diff" "HEAD")
   :output t
   :error-output t))

(defmacro define-command/raw (&rest args)
  `(#-clozure
    define-command/string
    #+clozure
    define-command/eval
    ,@args))

(define-command/eval (:vclone :vc) (repo &optional dirname)
  "Clone the REPO to DIRNAME or the repo name.
TODO: NAME THIS COMMAND MNEMONICALLY"
  (let ((old-dir (uiop:getcwd)))
    (uiop:run-program
     (list* "git" "clone"
            (if (uiop:string-prefix-p "http" repo)
                repo
                (uiop:strcat "https://" repo))
            (when dirname
              (list dirname)))
     :output t
     :error-output t)
    (uiop:chdir (or dirname
                    (car (last (uiop:split-string repo :separator (list #\/))))))
    (uiop:run-program
     (list "git" "fetch" "--unshallow"))
    (uiop:chdir old-dir)))

(define-command/string (:vadd :va) (message)
  "Stage, commit, and send all the changes in the repo."
  (uiop:run-program
   (list "git" "add" "--all"))
  (uiop:run-program
   (list "git" "commit" "-m" message))
  (uiop:run-program
   (list "git" "push")))

(define-command/string (:vpull :vp) ()
  "Pull the latest changes."
  (uiop:run-program
   (list "git" "fetch"))
  (uiop:run-program
   (list "git" "pull")))
