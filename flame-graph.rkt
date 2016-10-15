#lang racket/base

;; This module implements a conversion from Racket's profiler analyzer
;; output to a format that FlameGraph understands

(require profile/analyzer
         racket/list
         racket/match
         racket/set)

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
    (match-define (node id _ _ total _ _ callees) nd)
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
               (recur next-node (cons id parents)))))
    (cond [(null? leaf-cases)
           children]
          [else
           (define children-total
             (apply + (map stack-count children)))
           ;; cdr off the top because it's always "#f" for the ROOT node
           (cons (stack (cdr (reverse (cons id parents)))
                        (- total children-total))
                 children)]))

  (recur *-node '()))

(require racket/format)

(define (print-stacks stacks)
  (for ([stack (in-list stacks)])
    (display
     (apply string-append
            (add-between (map ~a (stack-names stack)) ";")))
    (display " ")
    (displayln (stack-count stack))))

;; taken from the feature-profiler example
(require racket/port (only-in profile profile-thunk))

(define (divisible x n)
  (= 0 (modulo x n)))

(define (fizzbuzz n)
  (for ([i (range n)])
    (cond [(divisible i 15) (printf "FizzBuzz\n")]
          [(divisible i 5)  (printf "Buzz\n")]
          [(divisible i 3)  (printf "Fizz\n")]
          [else             (printf "~a\n" i)])))

(profile-thunk (lambda () (parameterize ([current-output-port (open-output-nowhere)]) (fizzbuzz 10000000)))
               #:render (lambda (pf _) (print-stacks (profile->stacks pf))))
