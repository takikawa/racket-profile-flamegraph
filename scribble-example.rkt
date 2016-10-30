#lang racket

(require "profile.rkt")

(define pth
  (path->string (collection-file-path "reference.scrbl" "scribblings/reference")))

(define runpth
  (let-values ([(p _1 _2) (split-path (collection-file-path "run.rkt" "scribble"))])
    (path->string p)))

(profile-thunk-fg (λ ()
                    (parameterize ([current-directory runpth]
                                   [current-command-line-arguments `#(,pth)])
                      (dynamic-require 'scribble/run #f))
                    (void))
                  #:delay 0.01
                  "scribble.svg")
