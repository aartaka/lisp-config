(in-package :commands)

(define-command :time (form)
  "Time the FORM with prettier/universal TRIVIAL-TIME output."
  (eval `(trivial-time:time ,form)))

(define-command (:benchmark :bench) (form &optional times)
  "Benchmark the running time/params of FORM."
  (eval `(trivial-time:benchmark
             ,(if times (list times) '())
           ,form)))
