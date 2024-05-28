(uiop:define-package :version-control
  (:use :cl :trivial-toplevel-commands))
(in-package :version-control)

(defun current-branch ()
  (subseq (find-if
           (lambda (x)
             (uiop:string-prefix-p "* " x))
           (uiop:split-string
            (uiop:run-program
             (list "git" "branch")
             :output '(:string :stripped t))
            :separator (list #\Newline)))
          2))

(defun run-program-with-out (program)
  (uiop:run-program
   program
   :output t
   :error-output t))

(define-command (:vivisect :viv) ()
  "Show the diff of the last commit.
VIVISECT: cut (a body) open while still alive."
  (run-program-with-out (list "git" "branch"))
  (run-program-with-out (list "git" "status" "--porcelain"))
  (run-program-with-out (list "git" "show"))
  (run-program-with-out (list "git" "diff" "HEAD")))

;; Branch+create+setupstream command
;; Rebase
;; Merge
;; Amend the last commit (git commit --amend --no-edit)

(defmacro define-command/raw (&rest args)
  `(#-clozure
    define-command/string
    #+clozure
    define-command/eval
    ,@args))

(define-command/eval (:voyage :vo) (repo &optional dirname)
  "Clone the REPO to DIRNAME or repo name, and switch to the cloned dir.
VOYAGE: travel on water propelled by wind or by other means."
  (run-program-with-out
   (list* "git" "clone"
          "--recursive"
          (if (uiop:string-prefix-p "http" repo)
              repo
            (uiop:strcat "https://" repo))
          (when dirname
            (list dirname))))
  (uiop:chdir (or dirname
                  (car (last (uiop:split-string repo :separator (list #\/)))))))

(define-command/string (:venture :ve) (message)
  "Stage, commit, and send all the changes in the repo.
VENTURE: put forward, of a guess, in spite of possible refutation."
  (run-program-with-out
   (list "git" "commit" "-am" message))
  (run-program-with-out
   (list "git" "push" "origin" (current-branch))))

(define-command/string (:visit :vi) (upstream)
  "Pull the latest changes.
VISIT: go to certain places as for sightseeing."
  (let ((upstream (if (uiop:emptyp upstream)
                      "origin"
                      upstream)))
    (uiop:run-program
     (list "git" "fetch" upstream))
    (run-program-with-out
     (list "git" "pull" upstream (current-branch)))))

(define-command/string (:verge :ver) (branch)
  "Checkout BRANCH or create it if it doesn't exist.
VERGE: border on; come close to."
  (handler-case
      (run-program-with-out (list "git" "checkout" "-b" (string-downcase (string branch))))
    (error ()
      (run-program-with-out (list "git" "checkout" (string-downcase (string branch)))))))
