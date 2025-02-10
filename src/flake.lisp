;;;; -*- mode: lisp; syntax: common-lisp; base: 10; -*-
;;;; flake.lisp --- direct nix flake commands

(uiop:define-package #:vix/src/flake
  (:use #:cl
        #:marie
        #:vix/src/core))

(in-package #:vix/src/flake)

(define-command flake init (fi)
  "create a flake in the current directory"
  nil
  nil
  nil
  "Create a flake using the default template"
  "vix init")

(define-command flake metadata (fm)
  "show flake metadata"
  nil
  nil
  nil
  "Show flake metadata"
  "vix meta")

(define-command flake show (fs)
  "show the outputs provided by a flake"
  nil
  nil
  nil
  "Show the output attributes provided by the CWD flake"
  "vix show"
  "List available templates"
  "vix show templates")

(define-command flake update (fu)
  "update flake lock file"
  nil
  nil
  nil
  "Update all inputs"
  "vix update")

(define-command flake new (fn)
  "create a flake in the specified directory from a template"
  nil
  nil
  nil
  "Create a flake in the directory hello"
  "vix new hello"
  "Create a flake in the directory hello using template haskell-hello"
  "vix new hello -t templates#haskell-hello"
  )

(define-command flake clone (fc)
  "clone flake repository"
  nil
  nil
  nil
  "Check out the source code of the dwarffs flake"
  "vix clone dwarffs --dest dwarffs")

(define-command flake check (fck)
  "check whether the flake evaluates and run its tests"
  nil
  nil
  nil
  "Evaluate the flake in the current directory, and build its checks"
  "vix check")

(define-command flake archive (fa)
  "copy a flake and all its inputs to a store"
  nil
  nil
  nil
  "Fetch the dwarffs flake to the local Nix store"
  "vix achive dwarffs")

(define-command flake prefetch (fp)
  "download the flake source tree into the Nix store"
  nil
  nil
  nil
  "Download the dwarffs flake"
  "vix prefetch dwarffs --json")
