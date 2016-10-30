# profile-flame-graph

[![Scribble](https://img.shields.io/badge/Docs-Scribble-blue.svg)](http://docs.racket-lang.org/profile-flame-graph/index.html)

A library for providing Racket profiler outputs as
[flame graphs](https://github.com/brendangregg/FlameGraph).

The library can optionally call out to the `flamegraph.pl` script.
For this feature, you will need to check out the
[FlameGraph](https://github.com/brendangregg/FlameGraph) repo and
add it to your `PATH` variable.

# Quick usage

Install with:

```
raco pkg install profile-flame-graph
```

You will need to install FlameGraph. On a unix system, that might
look like this:

```
git clone https://github.com/brendangregg/FlameGraph.git
export PATH="$PATH:$PWD/FlameGraph"
```

Wrap the expression you want to profile in your program like this:

```racket
(profile <your-code-here>
         #:svg-path "my-profile.svg"
         #:preview? #t)
```

and run your program:

```
racket <your-program>.rkt
```

which will write the profile output to `my-profile.svg` and pop
up a window showing the flame graph.

---

Copyright (c) 2016 Asumu Takikawa

This program is free software: you can redistribute it and/or modify it under
the terms of the GNU Lesser General Public License as published by the Free
Software Foundation, either version 3 of the License, or (at your option) any
later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE. See the GNU Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public License along
with this program. If not, see http://www.gnu.org/licenses.
