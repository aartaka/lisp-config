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

;; Branc+create command
;; Rebase
;; Merge
;; List commits
;; Append to the last commit

(defmacro define-command/raw (&rest args)
  `(#-clozure
    define-command/string
    #+clozure
    define-command/eval
    ,@args))

(define-command/eval (:vclone :vc) (repo &optional dirname)
  "Clone the REPO to DIRNAME or repo name, and switch to the cloned dir.
TODO: NAME THIS COMMAND MNEMONICALLY"
  (uiop:run-program
   (list* "git" "clone"
          "--recursive"
          (if (uiop:string-prefix-p "http" repo)
              repo
            (uiop:strcat "https://" repo))
          (when dirname
            (list dirname)))
   :output t
   :error-output t)
  (uiop:chdir (or dirname
                  (car (last (uiop:split-string repo :separator (list #\/)))))))

(define-command/string (:vadd :va) (message)
  "Stage, commit, and send all the changes in the repo."
  (uiop:run-program
   (list "git" "add" "--all"))
  (uiop:run-program
   (list "git" "commit" "-m" message)
   :output t
   :error-output t)
  (uiop:run-program
   (list "git" "push")
   :output t
   :error-output t))

(define-command/string (:vpull :vp) ()
  "Pull the latest changes."
  (uiop:run-program
   (list "git" "fetch"))
  (uiop:run-program
   (list "git" "pull")
   :output t
   :error-output t))
