# Bexp

Bexp (Block-expressions) is a library for building block-based programming
languages.

## Building

    opam switch 4.07.1
    opam install dune
    opam install menhir
    opam install stdio
    opam install core
    opam install odoc
    opam install ppxlib
    opam install ppx_jane
    opam install js_of_ocaml
    opam install js_of_ocaml-ppx
    dune build example/main.bc.js
    dune build example/index.html
    dune build example/stylesheet.css

Then, navigate to `_build/main/example/index.html`.

**See the example at https://emelle.gitlab.io/bexp/example/index.html.**
