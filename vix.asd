;;;; -*- mode: lisp; syntax: common-lisp; base: 10; -*-
;;;; vix.asd --- top-level ASDF file for vix

(defsystem #:vix
    :name "vix"
    :long-name "vix"
    :description ""
    :long-description ""
    :version (:read-file-form #P"version.lisp")
    :author "Rommel Martínez <rommel.martinez@valmiz.com>"
    :maintainer "Rommel Martínez <rommel.martinez@valmiz.com>"
    :license ""
    :homepage ""
    :bug-tracker ""
    :source-control ""
    :class :package-inferred-system
    :depends-on (#:marie
                 #:clingon
                 #:vix/src/core
                 #:vix/src/rebuild
                 #:vix/src/search
                 #:vix/src/profile
                 #:vix/src/flake
                 #:vix/src/develop
                 #:vix/src/main
                 #:vix/src/driver
                 #:vix/src/user)
    :in-order-to ((test-op (test-op "vix-tests")))
    :build-operation "program-op"
    :build-pathname "vix"
    :entry-point "vix/src/main:main")
