;;;; -*- mode: lisp; syntax: common-lisp; base: 10; -*-
;;;; registry.lisp --- manipulate the Nix registry

(uiop:define-package #:vix/src/registry
  (:use #:cl
        #:marie
        #:vix/src/core))

(in-package #:vix/src/registry)

(define-command registry registry-add (ra)
  "create a flake in the current directory"
  nil nil nil
  "Set the nixpkgs flake identifier to a specific branch of Nixpkgs"
  "registry-add nixpkgs github:NixOS/nixpkgs/nixos-20.03")

(define-command registry registry-remove (rr)
  "remove flake from user flake registry"
  nil nil nil
  "Remove the entry nixpkgs from the user registry"
  "registry-remove nixpkgs")

(define-command registry registry-list (rl)
  "list available Nix flakes"
  nil nil nil
  "Show the contents of all registries"
  "registry-list")

(define-command registry registry-pin (rp)
  "pin a flake to its current version"
  nil nil nil
  "Pin nixpkgs to its most recent Git revision"
  "registry-pin nixpkgs")
