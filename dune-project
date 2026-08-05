(lang dune 3.24)

(name sat)

(generate_opam_files true)

(source
 (github notbowen/sat-practice))

(authors "Hu Bowen <contact@hubowen.dev>")

(license MIT)

(package
 (name sat)
 (synopsis "A local SAT revision and practice tool")
 (description
  "A localhost-only SAT revision tool backed by SQLite and the College Board question bank")
 (depends
  (ocaml (>= 5.1))
  (dune (>= 3.24))
  dream
  (caqti (and (>= 2.0) (< 3.0)))
  (caqti-lwt (and (>= 2.0) (< 3.0)))
  caqti-driver-sqlite3
  cohttp-lwt-unix
  lwt
  yojson
  uri
  argon2
  digestif
  mirage-crypto-rng
  ptime
  markup
  lambdasoup
  alcotest)
 (tags
  ("sat" "revision" "practice" "education")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
