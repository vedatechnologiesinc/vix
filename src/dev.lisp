;;;; -*- mode: lisp; syntax: common-lisp; base: 10; -*-
;;;; dev.lisp --- run a dev shell

(uiop:define-package #:vix/src/dev
  (:use #:cl
        #:marie
        #:vix/src/core))

(in-package #:vix/src/dev)

(def dev/command ()
  "dev command"
  (clingon:make-command
   :name "dev"
   :description "run a bash shell of a derivation build environment"
   :usage "[option...]"
   :handler (lambda (cmd)
              (let ((args (clingon:command-arguments cmd)))
                (nrun "develop" args)))
   :examples (mini-help "Run a dev shell in CWD"
                        "vix dev")))

(def command/handler (cmd)
  "command command handler"
  (let ((args (clingon:command-arguments cmd)))
    (nrun "develop" "--command" args)))

(def command/command ()
  "command command"
  (clingon:make-command
   :name "command"
   :description "Run a command inside a dev shell"
   :usage "[option...]"
   :handler #'command/handler
   :examples (mini-help "Run a command inside a dev shell"
                        "vix command make")))

(def make/handler (cmd)
  "make command handler"
  (let ((args (clingon:command-arguments cmd)))
    (nrun "develop" "--command" "make" args)))

(def make/command ()
  "make command"
  (clingon:make-command
   :name "make"
   :description "Run `make' inside a dev shell"
   :usage "[option...]"
   :handler #'make/handler
   :examples (mini-help "Run `make' inside a dev shell"
                        "vix make")))
