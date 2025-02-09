;;;; -*- mode: lisp; syntax: common-lisp; base: 10; -*-
;;;; profile.lisp --- direct nix profile commands

(uiop:define-package #:vix/src/profile
  (:use #:cl
        #:marie
        #:vix/src/core))

(in-package #:vix/src/profile)

;;; TODO
;;; vix install nixpkgs#btop and nixpkgs#htop
;;; vix install -n btop htop
(define-command profile install install
  "install a package into a profile"
  nil
  nil
  "Install a package from Nixpkgs"
  "vix install nixpkgs#hello"
  "Install a package from a specific Nixpkgs revision"
  "vix install nixpkgs/d734#hello")

(define-command profile remove uninstall
  "uninstall packages from a profile"
  nil
  nil
  "Remove a package by name"
  "vix uninstall hello"
  "Remove all packages"
  "vix uninstall --all")

(define-command profile upgrade upgrade
  "upgrade packages using their most recent flake"
  nil
  nil
  "Upgrade a specific package by name"
  "vix upgrade hello")

(define-command profile list list
  "list the installed packages"
  nil
  nil
  "List packages installed in the default profile"
  "vix list")

(define-command profile rollback rollback
  "roll back to a previous version of a profile"
  nil
  nil
  "Roll back your default profile to the previous version"
  "vix rollback")

(define-command profile history history
  "show all versions of a profile"
  nil
  nil
  "Show the changes between each version of your default profile"
  "vix history")

(define-command profile wipe-history wipe-history
  "delete non-current versions of a profile"
  nil
  nil
  "Delete all versions of the default profile older than 30 days"
  "vix wipe-history --profile /tmp/profile --older-than 30d")

(define-command profile diff-closures diff-closures
  "show the closure difference between each version of a profile"
  nil
  nil
  "Show what changed between each version of the NixOS system profile"
  "vix diff-closures --profile /nix/var/nix/profiles/system")
