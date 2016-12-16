#lang racket/base

;; This module implements a conversion from Racket's profiler sampler
;; output to a format that FlameGraph understands

(require data/order
         racket/format
         racket/list
         racket/match
         racket/set)

(provide samples->stacks
         print-stacks)

;; names - (listof any/c)
;; count - integer?
(struct stack (names count) #:transparent)

;; any -> (listof stack?)
;; The input is the *undocumented* format accepted by profile/analyzer's analyze-samples.
(define (samples->stacks cpu-time+samples)
  (match-define (cons cpu-time samples) cpu-time+samples)

  ;; format a string to be used in the graph labels
  (define (frame->name f)
    (match-define (cons id src) f)
    (~a (or id "")
        (if id " @ " "")
        (or (and src (srcloc->string src)) "")))

  ;; format a sample into a stack with a single count
  (define (->stack1 e)
    (match-define (list* thread-id _timestamp-ms frames) e)
    (stack (cons (format "thread-~a" thread-id)
                 (append (map frame->name (reverse (remove-adjacent-duplicates frames)))))
           1))

  (define-values (_prev _prev-count acc)
    (for/fold [(prev #f) (prev-count 0) (acc '())]
              [(s (sort (map ->stack1 samples) (order-<? datum-order)))]
      (cond [(equal? prev (stack-names s)) (values prev (+ prev-count 1) acc)]
            [(not prev) (values (stack-names s) (stack-count s) acc)]
            [else (values (stack-names s) (stack-count s) (cons (stack prev prev-count) acc))])))

  acc)

(define (remove-adjacent-duplicates lst)
  (cond [(null? lst) null]
        [else
         (define-values (new-lst _)
           (for/fold ([new-lst '()]
                      [prev (car lst)])
                     ([elem (in-list (cdr lst))])
             (if (equal? elem prev)
                 (values new-lst prev)
                 (values (cons prev new-lst) elem))))
         (reverse new-lst)]))

(define (print-stacks stacks)
  (for ([stack (in-list stacks)])
    (display
     (apply string-append
            (add-between (map ~a (stack-names stack)) ";")))
    (display " ")
    (displayln (stack-count stack))))
