;;;; -*- mode: lisp; syntax: common-lisp; base: 10; -*-
;;;; vix.asd --- main ASDF file of vix

(defsystem #:vix
    :name "vix"
    :description "A thin wrapper for interacting with the Nix CLI ecosystem."
    :version #.(uiop:read-file-form (make-pathname :directory '(:relative "src") :name "version" :type "lisp"))
    :author "VEDA Technologies, Inc. <hello@veda-tech.com>"
    :license "BSD-2-Clause"
    :class :package-inferred-system
    :depends-on (#:marie
                 #:cl-ppcre
                 #:clingon
                 #:vix/src/specials
                 #:vix/src/core
                 #:vix/src/registry
                 #:vix/src/rebuild
                 #:vix/src/profile
                 #:vix/src/flake
                 #:vix/src/store
                 #:vix/src/ext
                 #:vix/src/config
                 #:vix/src/derivation
                 #:vix/src/hash
                 #:vix/src/key
                 #:vix/src/nar
                 #:vix/src/main
                 #:vix/src/driver
                 #:vix/src/user)
    :in-order-to ((test-op (test-op "vix-tests")))
    :build-operation "program-op"
    :build-pathname "vix"
    :entry-point "vix/src/main:main")
