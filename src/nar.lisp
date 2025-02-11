;;;; -*- mode: lisp; syntax: common-lisp; base: 10; -*-
;;;; nar.lisp --- create or inspect NAR files

(uiop:define-package #:vix/src/nar
  (:use #:cl
        #:marie
        #:vix/src/core))

(in-package #:vix/src/nar)

(define-command nar cat^nar-cat ()
  "print the contents of a file inside a NAR file on stdout"
  nil nil nil
  "To display a file in a NAR file"
  "nar-cat ./hello.nar /share/man/man1/hello.1.gz | gunzip")

(define-command nar dump-path^nar-dump-path (nar-dump nar-pack)
  "serialise a path to stdout in NAR format"
  nil nil nil
  "To serialise directory `foo' as a NAR file"
  "nar-dump ./foo > foo.nar")

(define-command nar ls^nar-ls ()
  "show information about a path inside a NAR file"
  nil nil nil
  "To list a specific file in a NAR file"
  "nar-ls --long ./hello.nar /bin/hello")
