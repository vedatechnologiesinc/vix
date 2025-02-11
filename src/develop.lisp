;;;; -*- mode: lisp; syntax: common-lisp; base: 10; -*-
;;;; develop.lisp --- run a dev shell

(uiop:define-package #:vix/src/develop
  (:use #:cl
        #:marie
        #:vix/src/core))

(in-package #:vix/src/develop)

(def- develop/options ()
  (list
   (clingon:make-option :string
                        :description "specify command"
                        :short-name #\c
                        :long-name "command"
                        :required nil
                        :key :opt-command)))

(def- develop/handler (cmd)
  "Handler for the `develop' command."
  (let* ((args (clingon:command-arguments cmd))
         (opt-command (clingon:getopt cmd :opt-command))
         (full-args (append args
                            (when opt-command '("--command")))))
    (nrun "develop" full-args)))

(define-command nil develop (dev)
  "run a dev shell of a derivation build environment"
  nil
  (develop/options)
  #'develop/handler
  "Start a dev shell with the build environment of the current directory"
  "dev"
  "Start a dev shell and run `make' inside"
  "dev -c make")

(def make/handler (cmd)
  "Handler for the `make' command."
  (let ((args (clingon:command-arguments cmd)))
    (nrun "develop" "--command" "make" args)))

(define-command nil make (mk)
  "run make inside a dev shell"
  nil
  nil
  #'make/handler
  "Run `make' inside a dev shell"
  "make")
