#lang racket/base

(require "flame-graph.rkt"
         profile
         racket/file
         racket/port
         racket/system)

(provide profile-thunk-fg)

(define (profile-thunk-fg thunk
                          [filename #f]
                          #:delay [delay 0.05]
                          #:repeat [iterations 1]
                          #:threads [threads? #f]
                          #:periodic-renderer [periodic-renderer #f]
                          #:use-errortrace? [use-errortrace? #f]
                          #:order [order 'topological])
  (profile-thunk thunk
                 #:delay delay
                 #:repeat iterations
                 #:threads threads?
                 #:render (do-print filename)
                 #:periodic-renderer periodic-renderer
                 #:use-errortrace? use-errortrace?
                 #:order order))


(define (do-print filename)
  (λ (pf _)
    (cond [filename
           (define fg-path (find-executable-path "flamegraph.pl"))
           (unless fg-path
             (error "flamegraph.pl not in your executable path"))

           (define tmp (make-temporary-file))
           (define tmp-out (open-output-file tmp #:exists 'replace))
           (parameterize ([current-output-port tmp-out])
             (print-stacks (profile->stacks pf)))
           (close-output-port tmp-out)

           (define out (open-output-file filename))

           (define ports (process* fg-path tmp))
           (copy-port (car ports) out)
           (close-output-port out)]
          [else
           (print-stacks (profile->stacks pf))])))
