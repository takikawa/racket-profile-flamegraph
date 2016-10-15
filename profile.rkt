#lang racket/base

(require "flame-graph.rkt"
         profile)

(provide profile-thunk-fg)

(define (profile-thunk-fg thunk
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
                 #:render (λ (pf _) (print-stacks (profile->stacks pf)))
                 #:periodic-renderer periodic-renderer
                 #:use-errortrace? use-errortrace?
                 #:order order))
