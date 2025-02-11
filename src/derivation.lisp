;;;; -*- mode: lisp; syntax: common-lisp; base: 10; -*-
;;;; derivation.lisp --- work with derivations

(uiop:define-package #:vix/src/derivation
  (:use #:cl
        #:marie
        #:vix/src/core))

(in-package #:vix/src/derivation)

(define-command derivation add^derivation-add ()
  "add a store derivation"
  nil nil nil
  "Add a derivation"
  "derivation-add")

(define-options derivation-show)
(define-handler derivation-show ("derivation" "show"))
(define-command derivation show^derivation-show ()
  "show the contents of a store derivation"
  nil t t
  "Show the `hello' derivation"
  "derivation-show -n hello")
