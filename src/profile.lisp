;;;; -*- mode: lisp; syntax: common-lisp; base: 10; -*-
;;;; profile.lisp --- Nix profile commands

(uiop:define-package #:vix/src/profile
  (:use #:cl
        #:marie
        #:vix/src/core))

(in-package #:vix/src/profile)

(define-command nil install (i)
  "add a package into a profile"
  "<package>..."
  nil
  (lambda (cmd)
    (let* ((args (clingon:command-arguments cmd))
           (args-prefixed (prefix-nixpkgs args)))
      (exe "profile" "install" args-prefixed)))
  nil
  "Install packages from Nixpkgs"
  "i htop btop")

(define-command nil add (a)
  "add a package into a profile"
  "<package>..."
  nil
  (lambda (cmd)
    (let* ((args (clingon:command-arguments cmd)))
      (exe "profile" "install" args)))
  nil
  "Install a package from Nixpkgs"
  "a n#hello"
  "Install a package from a specific Nixpkgs revision"
  "a nixpkgs/d734#hello")

(define-command nil uninstall (u rm)
  "remove packages from a profile"
  "<package>..."
  nil
  (lambda (cmd)
    (let ((args (clingon:command-arguments cmd)))
      (exe "profile" "remove" args)))
  nil
  "Uninstall a package by name"
  "u hello"
  "Remove all packages"
  "u -- --all")

(define-command nil upgrade (up)
  "upgrade packages using their most recent flake"
  "<package>..."
  nil
  (lambda (cmd)
    (let ((args (clingon:command-arguments cmd)))
      (exe "profile" "upgrade" args)))
  nil
  "Upgrade a specific package by name"
  "up hello")

(define-command nil list (l)
  "list the installed packages"
  ""
  nil
  (lambda (cmd)
    (let ((args (clingon:command-arguments cmd)))
      (exe "profile" "list" args)))
  nil
  "List packages installed in the default profile"
  "l")

(define-command nil rollback ()
  "roll back to a previous version of a profile"
  ""
  nil
  (lambda (cmd)
    (let ((args (clingon:command-arguments cmd)))
      (exe "profile" "rollback" args)))
  nil
  "Roll back your default profile to the previous version"
  "b"
  "Roll back your default profile to version 500"
  "b -- --to 500")

(define-command nil history (h)
  "show all versions of a profile"
  ""
  nil
  (lambda (cmd)
    (let ((args (clingon:command-arguments cmd)))
      (exe "profile" "history" args)))
  nil
  "Show the changes between each version of your default profile"
  "h")

(define-command nil wipe-history (wh)
  "delete non-current versions of a profile"
  ""
  nil
  (lambda (cmd)
    (let ((args (clingon:command-arguments cmd)))
      (exe "profile" "wipe-history" args)))
  nil
  "Delete all versions of the default profile older than 30 days"
  "wh -- --profile /tmp/profile --older-than 30d")

(define-command profile diff-closures (dc)
  "show the closure difference between each version of a profile"
  ""
  nil
  (lambda (cmd)
    (let ((args (clingon:command-arguments cmd)))
      (exe "profile" "diff-closures" args)))
  nil
  "Show what changed between each version of the NixOS system profile"
  "dc -- --profile /nix/var/nix/profiles/system")

;; (define-command nil profile (p)
;;   "profile commands"
;;   "<command>"
;;   nil
;;   #'print-usage
;;   (install
;;    remove
;;    upgrade
;;    list
;;    rollback
;;    history
;;    wipe-history
;;    diff-closures))
