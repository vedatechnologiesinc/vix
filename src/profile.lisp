;;;; -*- mode: lisp; syntax: common-lisp; base: 10; -*-
;;;; profile.lisp --- Nix profile commands

(uiop:define-package #:vix/src/profile
  (:use #:cl
        #:marie
        #:vix/src/core)
  (:export #:add/command
           #:remove/command
           #:upgrade/command
           #:list/command))

(in-package #:vix/src/profile)

(define-command profile add (install)
  "add a package into a profile"
  "<package>..."
  t
  t
  nil
  "Add a package from Nixpkgs"
  "p add n#hello"
  "Add a package from a specific Nixpkgs revision"
  "p add nixpkgs/d734#hello")

(defalias profile/add/command add/command)

(define-command profile remove (rm)
  "remove packages from a profile"
  "<package>..."
  nil
  t
  nil
  "Remove a package by name"
  "p rm hello"
  "Remove all packages"
  "p rm -- --all")

(defalias profile/remove/command remove/command)

(define-command profile upgrade (up)
  "upgrade packages using their most recent flake"
  "<package>..."
  nil
  t
  nil
  "Upgrade a specific package by name"
  "p up hello")

(defalias profile/upgrade/command upgrade/command)

(define-command profile list (ls)
  "list the installed packages"
  ""
  nil
  t
  nil
  "List packages installed in the default profile"
  "p ls")

(defalias profile/list/command list/command)

(define-command profile rollback (rb)
  "roll back to a previous version of a profile"
  ""
  nil
  t
  nil
  "Roll back your default profile to the previous version"
  "p rb"
  "Roll back your default profile to version 500"
  "p rb -- --to 500")

(define-command profile history (h)
  "show all versions of a profile"
  ""
  nil
  t
  nil
  "Show the changes between each version of your default profile"
  "p h")

(define-command profile wipe-history (w)
  "delete non-current versions of a profile"
  ""
  nil
  t
  nil
  "Delete all versions of the default profile older than 30 days"
  "p w -- --profile /tmp/profile --older-than 30d")

(define-command profile diff-closures (d)
  "show the closure difference between each version of a profile"
  ""
  nil
  t
  nil
  "Show what changed between each version of the NixOS system profile"
  "p d -- --profile /nix/var/nix/profiles/system")

(define-command nil profile (p)
  "profile commands"
  "<command>"
  nil
  #'print-usage
  (add remove upgrade list rollback history wipe-history diff-closures))
