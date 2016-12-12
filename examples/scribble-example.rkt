#lang racket

(require profile-flame-graph)

(define pth
  (path->string (collection-file-path "reference.scrbl" "scribblings/reference")))

(define runpth
  (let-values ([(p _1 _2) (split-path (collection-file-path "run.rkt" "scribble"))])
    (path->string p)))

(profile-thunk (λ ()
                 (parameterize ([current-directory runpth]
                                [current-command-line-arguments `#("--markdown" ,pth)])
                   (dynamic-require 'scribble/run #f))
                 (void))
               #:svg-path "scribble.svg")
