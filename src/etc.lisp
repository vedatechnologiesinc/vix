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
  "[-n]" t t
  "Build the default package from the flake in the current directory"
  "build"
  "Build `hello' and `cowsay' from Nixpkgs, leaving two result symlinks"
  "build -n hello cowsay")

(define-options run)
(define-handler run ("run"))
(define-command nil run ()
  "run a Nix application"
  nil t t
  "Run `vim' from the `nixpkgs' flake"
  "run -n vim")

(define-options bundle)
(define-handler bundle ("bundle"))
(define-command nil bundle ()
  "bundle an application so that it works outside of the Nix store"
  nil t t
  "Bundle `hello'"
  "bundle -n vim")

(define-command nil copy ()
  "start an interactive environment for evaluating Nix expressions"
  nil nil nil
  "Copy all store paths from a local binary cache"
  "copy -- --all --from file:///tmp/cache")

(define-options edit)
(define-handler edit ("edit"))
(define-command nil edit ()
  "open the Nix expression of a Nix package in $EDITOR"
  nil t t
  "Open the Nix expression of the `hello' package"
  "edit -n hello")

(define-command nil eval ()
  "evaluate a Nix expression"
  nil nil nil
  "Evaluate a Nix expression given on the command line"
  "eval -- --expr '1 + 2'")

(define-command nil fmt (format)
  "reformat your code in the standard style"
  nil nil nil
  "Format the current flake"
  "fmt")

(define-command nil repl ()
  "start an interactive environment for evaluating Nix expressions"
  nil nil nil
  "Evaluate some simple Nix expressions"
  "repl")

(define-options path-info)
(define-handler path-info ("path-info"))
(define-command nil path-info (path)
  "query information about store paths"
  nil t t
  "Print the store path produced by nixpkgs#hello"
  "path -n hello")

(define-options why-depends)
(define-handler why-depends ("why-depends"))
(define-command nil why-depends (why)
  "show why a package has another package in its closure"
  nil t t
  "Show one path through the dependency graph leading from `hello' to `glibc'"
  "why -n hello glibc")

(define-options env-shell)
(define-handler env-shell ("env" "shell"))
(define-command env shell^env-shell (shell)
  "run a shell in which the specified packages are available"
  nil t t
  "Start a shell providing `yt-dlp' from the `nixpkgs' flake"
  "shell -n yt-dlp")

(define-options print-dev-env)
(define-handler print-dev-env ("print-dev-env"))
(define-command nil print-dev-env (print)
  "print shell code of derivation"
  nil t t
  "Get the build environment"
  "print -n hello")

(define-command nil daemon ()
  "daemon to perform store operations on behalf of non-root clients"
  nil nil nil
  "Run the daemon"
  "daemon"
  "Run the daemon and force all connections to be trusted"
  "daemon -- --force-trusted")

(define-options realisation-info)
(define-handler realisation-info ("realisation" "info"))
(define-command realisation info^realisation-info ()
  "manipulate a Nix realisation"
  nil t t
  "Show some information about the realisation of the hello package"
  "realisation-info -n hello")

(define-command nil upgrade-nix ()
  "upgrade Nix to the latest stable version"
  nil nil nil
  "Upgrade Nix to the stable version declared in Nixpkgs"
  "upgrade-nix")
