#lang racket/base

(require net/sendurl
         profile-flame-graph/flame-graph
         profile/sampler
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
                          #:repeat [rpt 1]
                          #:threads [threads? #f]
                          #:use-errortrace? [et? #f])
  (define cust (and threads? (make-custodian (current-custodian))))
  (define sampler (create-sampler (if threads?
                                    (list cust (current-thread))
                                    (current-thread))
                                  delay
                                  #:use-errortrace? et?))
  (define (run) (for/last ([i (in-range rpt)]) (thunk)))
  (begin0 (with-handlers ([void (λ (e) (eprintf "profiled thunk error: ~a\n"
                                                (if (exn? e)
                                                    (exn-message e)
                                                    (format "~e" e))))])
            (if threads?
                (parameterize ([current-custodian cust]) (run))
                (run)))
    (sampler 'stop)
    (do-print svg-path preview? (sampler 'get-snapshots))))

(define-syntax (profile-fg stx)
  (syntax-case stx ()
    [(_ e . kws)
     (syntax/loc stx (profile-thunk-fg (λ () e) . kws))]))

(define (do-print filename preview? pf)
  (cond [(or filename preview?)
         (define fg-path (find-executable-path "flamegraph.pl"))
         (unless fg-path
           (error "flamegraph.pl not in your executable path"))

         (define tmp (make-temporary-file))
         (define tmp-out (open-output-file tmp #:exists 'replace))
         (parameterize ([current-output-port tmp-out])
           (print-stacks (samples->stacks pf)))
         (close-output-port tmp-out)

         (define filename*
           (or filename (make-temporary-file)))
         (define out (open-output-file filename* #:exists 'replace))

         (define ports (process* fg-path tmp))
         (copy-port (car ports) out)
         (close-output-port out)

         (when preview?
           (send-url/file filename))]
        [else
         (print-stacks (samples->stacks pf))]))
