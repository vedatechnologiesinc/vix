;;;; -*- mode: lisp; syntax: common-lisp; base: 10; -*-
;;;; vix.asd --- main ASDF file of vix

(defsystem #:vix
    :name "vix"
    ;; :version #.(uiop:read-file-string (subpathname *load-pathname* "version.lisp"))
    ;; :version #.(uiop:read-file-form (make-pathname :directory '(:relative "src") :name "version" :type "lisp"))
    :description "A program for interacting with the Nix ecosystem"
    :author "Rommel Martínez <ebzzry@icloud.com>"
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
