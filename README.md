# Bexp

Bexp (Block-expressions) is a library for building block-based programming
languages.

## Building

    opam switch 4.07.1
    opam install core_kernel
    opam install js_of_ocaml
    opam install js_of_ocaml-ppx
    opam install dune
    dune build example/main.bc.js
    dune build example/index.html

Then, navigate to `_build/main/example/index.html`.

**See the example at https://emelle.gitlab.io/bexp/example/index.html.**
