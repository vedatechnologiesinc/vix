;;;; -*- mode: lisp; syntax: common-lisp; base: 10; -*-
;;;; profile.lisp --- direct nix profile commands

(uiop:define-package #:vix/src/profile
  (:use #:cl
        #:marie
        #:vix/src/core))

(in-package #:vix/src/profile)

(define-options install)
(define-handler install ("profile" "install"))
(define-command profile install (i)
  "install a package into a profile"
  nil t t
  "Install a package from Nixpkgs"
  "install -n hello"
  "Install a package from a specific Nixpkgs revision"
  "install nixpkgs/d734#hello")

(define-command profile remove (uninstall u)
  "uninstall packages from a profile"
  nil nil nil
  "Remove a package by name"
  "uninstall hello"
  "Remove all packages"
  "uninstall -- --all")

(define-command profile upgrade (up)
  "upgrade packages using their most recent flake"
  nil nil nil
  "Upgrade a specific package by name"
  "upgrade hello")

(define-command profile list (ls)
  "list the installed packages"
  nil nil nil
  "List packages installed in the default profile"
  "list")

(define-command profile rollback (back)
  "roll back to a previous version of a profile"
  nil nil nil
  "Roll back your default profile to the previous version"
  "rollback")

(define-command profile history (hist)
  "show all versions of a profile"
  nil nil nil
  "Show the changes between each version of your default profile"
  "history")

(define-command profile wipe-history (wipe)
  "delete non-current versions of a profile"
  nil nil nil
  "Delete all versions of the default profile older than 30 days"
  "wipe-history -- --profile /tmp/profile --older-than 30d")

(define-command profile diff-closures (diff)
  "show the closure difference between each version of a profile"
  nil nil nil
  "Show what changed between each version of the NixOS system profile"
  "diff-closures -- --profile /nix/var/nix/profiles/system")
