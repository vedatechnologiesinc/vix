;;;; -*- mode: lisp; syntax: common-lisp; base: 10; -*-
;;;; etc.lisp --- etc the system

(uiop:define-package #:vix/src/etc
  (:use #:cl
        #:marie
        #:vix/src/core))

(in-package #:vix/src/etc)

(define-options build)
(define-handler build ("build"))
(define-command nil build ()
  "build a derivation or fetch a store path"
  "[-n]"
  (build/options)
  #'build/handler
  "Build the default package from the flake in the current directory"
  "vix build"
  "Build `hello' and `cowsay' from Nixpkgs, leaving two result symlinks"
  "vix build -n hello cowsay")

(define-options run)
(define-handler run ("run"))
(define-command nil run ()
  "run a Nix application"
  nil
  (run/options)
  #'run/handler
  "Run `vim' from the `nixpkgs' flake"
  "vix run -n vim")

(define-options bundle)
(define-handler bundle ("bundle"))
(define-command nil bundle ()
  "bundle an application so that it works outside of the Nix store"
  nil
  (bundle/options)
  #'bundle/handler
  "Bundle `hello'"
  "vix bundle -n vim")

(define-command nil copy ()
  "start an interactive environment for evaluating Nix expressions"
  nil
  nil
  nil
  "Copy all store paths from a local binary cache"
  "vix copy --all --from file:///tmp/cache")

(define-options edit)
(define-handler edit ("edit"))
(define-command nil edit ()
  "open the Nix expression of a Nix package in $EDITOR"
  nil
  (edit/options)
  #'edit/handler
  "Open the Nix expression of the `hello' package"
  "vix edit -n hello")

(define-command nil eval ()
  "evaluate a Nix expression"
  nil
  nil
  nil
  "Evaluate a Nix expression given on the command line"
  "vix eval --expr '1 + 2'")

(define-command nil fmt (format)
  "reformat your code in the standard style"
  nil
  nil
  nil
  "Format the current flake"
  "vix fmt")
