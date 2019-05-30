# Bexp

Bexp (Block-expressions) is a library for building block-based programming
languages.

## Building

    opam switch 4.07.1
    opam install . --deps-only
    dune build example/main.bc.js
    dune build example/index.html
    dune build example/stylesheet.css

Then, navigate to `_build/main/example/index.html`.

**See the example at https://emmeline.gitlab.io/bexp/example/index.html.**
