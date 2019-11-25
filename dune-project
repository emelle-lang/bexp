(lang dune 1.10)
(name bexp)
(using menhir 2.0)
(generate_opam_files true)

(license MPL-2.0)
(authors "Types Logics Cats" "TheAspiringHacker@protonmail.com")
(maintainers "Types Logics Cats" "TheAspiringHacker@protonmail.com")
(source (uri "git://gitlab.com/emmeline/bexp.git"))
(bug_reports "https://gitlab.com/emmeline/bexp/issues")
(homepage "https://gitlab.com/emmeline/bexp")

(package
  (name bexp)
  (synopsis "Visual language syntax library")
  (description
    "Library for making block-based languages")
  (depends
    (base (and (>= v0.13) (< v0.14)))
    (dune (>= 2.0))
    (js_of_ocaml (>= 3.5))
    (js_of_ocaml-ppx (>= 3.5))
  )
)

(package
  (name ppx_bexp)
  (synopsis "PPX helper for Bexp")
  (description
    "PPX helper for Bexp")
  (depends
    (base (and (>= v0.13) (< v0.14)))
    (bexp :dev)
    (dune (>= 2.0))
    (ppxlib (and (>= 0.9) (< 0.10)))
  )
)

(package
  (name bexp_dsl)
  (synopsis "Grammar DSL for Bexp")
  (description
    "Grammar DSL for Bexp")
  (depends
    (base (and (>= v0.13) (< v0.14)))
    (dune (>= 2.0))
    (menhir (>= 20190924))
    (stdio (and (>= v0.13) (< v0.14)))
  )
)
