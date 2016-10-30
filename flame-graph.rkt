#lang racket/base

;; This module implements a conversion from Racket's profiler analyzer
;; output to a format that FlameGraph understands

(require profile/analyzer
         racket/format
         racket/list
         racket/match
         racket/set)

(provide profile->stacks
         print-stacks)

;; names - (listof any/c)
;; count - integer?
(struct stack (names count) #:transparent)

;; profile? -> (listof stack?)
(define (profile->stacks pf)
  (match-define (profile _ _ _ _ _ *-node) pf)

  ;; track already seen nodes to avoid repetition in depth-first
  ;; traversal of the call graph
  (define seen (mutable-seteq))

  ;; construct all stacks for this node and its children
  (define (recur nd parents)
    (match-define (node id src _ total _ _ callees) nd)

    ;; format a string to be used in the graph labels
    (define name
      (~a id (or (and src (srcloc->string src)) "")))

    ;; it should be the case that this is either a singleton list or null
    (define leaf-cases
      (filter (λ (e) (eq? (edge-callee e) *-node)) callees))
    ;; reverse b/c it appears the node with the deepest call chain tends to
    ;; be the last one
    (define rcallees (reverse callees))
    (define children
      (apply append
             (for/list ([callee (in-list (remove* leaf-cases rcallees eq?))]
                        #:unless (set-member? seen (edge-callee callee)))
               (define next-node (edge-callee callee))
               (set-add! seen next-node)
               (recur next-node (cons name parents)))))
    (cond [(null? leaf-cases)
           children]
          [else
           (define children-total
             (apply + (map stack-count children)))
           ;; cdr off the top because it's always "#f" for the ROOT node
           (cons (stack (cdr (reverse (cons name parents)))
                        (- total children-total))
                 children)]))

  (recur *-node '()))

(define (print-stacks stacks)
  (for ([stack (in-list stacks)])
    (display
     (apply string-append
            (add-between (map ~a (stack-names stack)) ";")))
    (display " ")
    (displayln (stack-count stack))))
