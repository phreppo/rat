(lang dune 2.7)

(using menhir 2.1)

(generate_opam_files true)

(source
 (github parof/rat))

(authors "Francesco Parolini")

(maintainers "Francesco Parolini")

(license MIT)

(package
 (name rat)
 (synopsis "A regular expression denial of service attacks analyzer.")
 (description "Rat analyzes regular expressions and finds vulnerabilities to regular expression denial of service attacks (ReDoS). It is mathematically proved that once it classifies a regular expression as safe, that regular expression cannot have a vulnerability.")
 (depends
  (ocaml
   (>= 4.8))
  dune
  (menhir
   (>= 2.1))
  alcotest
  js_of_ocaml
  js_of_ocaml-ppx)
 (tags
  (ReDoS
   DoS
   "regular expression denial of service"
   "regular expression"
   "regex")))
