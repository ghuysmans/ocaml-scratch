(lang dune 3.3)

(name scratch)

(generate_opam_files true)

(source
 (github ghuysmans/ocaml-scratch))

(authors "Guillaume Huysmans <ghuysmans99@gmail.com>")

(maintainers "Guillaume Huysmans <ghuysmans99@gmail.com>")

(license GPL-2.0-or-later)

(package
 (name scratch)
 (synopsis "Scratch parser")
 (depends ocaml dune ppx_deriving_yojson))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
