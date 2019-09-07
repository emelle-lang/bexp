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
    (base (>= v0.12))
    (dune (and :build (>= 1.10)))
    (js_of_ocaml (>= 3.4))
    (js_of_ocaml-ppx (>= 3.4))
  )
)

(package
  (name ppx_bexp)
  (synopsis "PPX helper for Bexp")
  (description
    "PPX helper for Bexp")
  (depends
    (base (>= v0.12))
    (bexp :dev)
    (dune (and :build (>= 1.10)))
    (ppxlib (and (>= 0.5.0) (< 0.9)))
  )
)

(package
  (name bexp_dsl)
  (synopsis "Grammar DSL for Bexp")
  (description
    "Grammar DSL for Bexp")
  (depends
    (base (>= v0.12))
    (dune (and :build (>= 1.10)))
    (menhir (>= 20181113))
    (stdio (>= v0.12))
  )
)
