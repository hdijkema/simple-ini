#lang info

(define pkg-authors '(hnmdijkema))
(define version "0.20")
(define license 'Apache-2.0)
;(define collection "simple-ini")
(define pkg-desc "A Simple .ini file reader/writer for racket")

(define scribblings
  '(
    ("scribblings/ini.scrbl" () (library) "simple-ini")
    )
  )

(define deps
  '("base" "roos"))

(define build-deps
  '("racket-doc"
    "rackunit-lib"
    "scribble-lib"))
