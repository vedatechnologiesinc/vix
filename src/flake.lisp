;;;; -*- mode: lisp; syntax: common-lisp; base: 10; -*-
;;;; flake.lisp --- direct nix flake commands

(uiop:define-package #:vix/src/flake
  (:use #:cl
        #:marie
        #:vix/src/core))

(in-package #:vix/src/flake)

(define-command flake init (fi)
  "create a flake in the current directory"
  nil nil nil
  "Create a flake using the default template"
  "init")

(define-command flake metadata (fm)
  "show flake metadata"
  nil nil nil
  "Show flake metadata"
  "meta")

(define-command flake show (fs)
  "show the outputs provided by a flake"
  nil nil nil
  "Show the output attributes provided by the CWD flake"
  "show"
  "List available templates"
  "show templates")

(define-command flake update (fu)
  "update flake lock file"
  nil nil nil
  "Update all inputs"
  "update")

(define-command flake new (fn)
  "create a flake in the specified directory from a template"
  nil nil nil
  "Create a flake in the directory hello"
  "new hello"
  "Create a flake in the directory hello using template haskell-hello"
  "new hello -t templates#haskell-hello")

(define-command flake clone (fc)
  "clone flake repository"
  nil nil nil
  "Check out the source code of the dwarffs flake"
  "clone dwarffs --dest dwarffs")

(define-command flake check (fh)
  "check whether the flake evaluates and run its tests"
  nil nil nil
  "Evaluate the flake in the current directory, and build its checks"
  "check")

(define-command flake archive (fa)
  "copy a flake and all its inputs to a store"
  nil nil nil
  "Fetch the dwarffs flake to the local Nix store"
  "achive dwarffs")

(define-command flake prefetch (fp)
  "download the flake source tree into the Nix store"
  nil nil nil
  "Download the dwarffs flake"
  "prefetch dwarffs --json")
