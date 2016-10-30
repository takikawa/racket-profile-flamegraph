#lang scribble/manual

@title{profile-flame-graph}

@(require (for-label racket/base
                     profile-flame-graph))

@defmodule[profile-flame-graph]

This library lets you output Racket profiler captures
from the @racketmodname[profile] library in a format that the
@hyperlink["https://github.com/brendangregg/FlameGraph" "flamegraph.pl"]
script understands.

@defproc[(profile-thunk
          [thunk (-> any/c)]
          [#:svg-path svg-path (or/c path? #f) #f]
          [#:delay   delay      (>=/c 0.0)        0.05]
          [#:repeat  iterations exact-nonnegative-integer? 1]
          [#:threads threads?   any/c                      #f]
          [#:periodic-renderer periodic-renderer
           (or/c #f (list/c (>=/c 0.0)
                            (-> profile?
                                (or/c 'topological 'self 'total)
                                any/c)))
           #f]
          [#:use-errortrace? use-errortrace? any/c #f]
          [#:order order (or/c 'topological 'self 'total) 'topological])
         any/c]{
  Identical to the function in the @racketmodname[profile] library
  except that it does not accept a @racket[#:render] keyword and takes
  an optional @racket[svg-path] argument.

  If @racket[svg-path] is provided, this function will attempt to call
  the @tt{flamegraph.pl} script (a path to the script must be in the
  @tt{PATH} variable) to output an SVG document to @racket[svg-path].

  Otherwise, the function will print the profile results in a format
  that is understood by the @tt{flamegraph.pl} script to the current
  output port.
}

@defform[(profile expr keyword-arguments ...)]{

Similar to the macro with the same name in the @racketmodname[profile] library
except that it takes an additional @racket[#:svg-path] argument that is passed onto
@racket[profile-thunk].
}
