;;;; -*- mode: lisp; syntax: common-lisp; base: 10; -*-
;;;; profile.lisp --- direct nix profile commands

(uiop:define-package #:vix/src/profile
  (:use #:cl
        #:marie
        #:vix/src/core))

(in-package #:vix/src/profile)

;; (def- install/options ()
;;   "Returns the options for the `install' command."
;;   (list
;;    (clingon:make-option :flag
;;                         :description "toggle Nixpkgs"
;;                         :short-name #\n
;;                         :long-name "nixpkgs"
;;                         :required nil
;;                         :key :opt-nixpkgs)))

;; (def- install/handler (cmd)
;;   "The handler for the `search' command"
;;   (let* ((args (clingon:command-arguments cmd))
;;          (opt-nixpkgs (clingon:getopt cmd :opt-nixpkgs))
;;          (final-args (cond (opt-nixpkgs (append '("profile" "install") (uiop:split-string (pipe-args args))))
;;                            (t (append '("profile" "install") args)))))
;;     (apply #'nrun final-args)))

(define-options install)
(define-handler install ("profile" "install"))
(define-command profile install (i)
  "install a package into a profile"
  nil
  (install/options)
  #'install/handler
  "Install a package from Nixpkgs"
  "vix install nixpkgs#hello"
  "Install a package from a specific Nixpkgs revision"
  "vix install nixpkgs/d734#hello")

(define-command profile remove (uninstall u)
  "uninstall packages from a profile"
  nil
  nil
  nil
  "Remove a package by name"
  "vix uninstall hello"
  "Remove all packages"
  "vix uninstall --all")

(define-command profile upgrade (up)
  "upgrade packages using their most recent flake"
  nil
  nil
  nil
  "Upgrade a specific package by name"
  "vix upgrade hello")

(define-command profile list (ls)
  "list the installed packages"
  nil
  nil
  nil
  "List packages installed in the default profile"
  "vix list")

(define-command profile rollback (back)
  "roll back to a previous version of a profile"
  nil
  nil
  nil
  "Roll back your default profile to the previous version"
  "vix rollback")

(define-command profile history (hist)
  "show all versions of a profile"
  nil
  nil
  nil
  "Show the changes between each version of your default profile"
  "vix history")

(define-command profile wipe-history (wipe)
  "delete non-current versions of a profile"
  nil
  nil
  nil
  "Delete all versions of the default profile older than 30 days"
  "vix wipe-history --profile /tmp/profile --older-than 30d")

(define-command profile diff-closures (diff)
  "show the closure difference between each version of a profile"
  nil
  nil
  nil
  "Show what changed between each version of the NixOS system profile"
  "vix diff-closures --profile /nix/var/nix/profiles/system")
