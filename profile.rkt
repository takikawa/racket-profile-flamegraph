#lang racket/base

(require pict
         profile-flame-graph/flame-graph
         profile
         racket/file
         racket/port
         racket/system
         rsvg
         (for-syntax racket/base))

(provide (rename-out [profile-thunk-fg profile-thunk]
                     [profile-fg profile]))

(define (profile-thunk-fg thunk
                          #:svg-path [svg-path #f]
                          #:preview? [preview? #f]
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
                 #:render (do-print svg-path preview?)
                 #:periodic-renderer periodic-renderer
                 #:use-errortrace? use-errortrace?
                 #:order order))

(define-syntax (profile-fg stx)
  (syntax-case stx ()
    [(_ e . kws)
     (syntax/loc stx (profile-thunk-fg (λ () e) . kws))]))

(define (do-print filename preview?)
  (λ (pf _)
    (cond [(or filename preview?)
           (define fg-path (find-executable-path "flamegraph.pl"))
           (unless fg-path
             (error "flamegraph.pl not in your executable path"))

           (define tmp (make-temporary-file))
           (define tmp-out (open-output-file tmp #:exists 'replace))
           (parameterize ([current-output-port tmp-out])
             (print-stacks (profile->stacks pf)))
           (close-output-port tmp-out)

           (define filename*
             (or filename (make-temporary-file)))
           (define out (open-output-file filename* #:exists 'replace))

           (define ports (process* fg-path tmp "--countname" "milliseconds"))
           (copy-port (car ports) out)
           (close-output-port out)

           (when preview?
             (show-pict (svg-file->pict filename)))]
          [else
           (print-stacks (profile->stacks pf))])))
