;;;; -*- mode: lisp; syntax: common-lisp; base: 10; -*-
;;;; store.lisp --- manipulate the Nix store

(uiop:define-package #:vix/src/store
  (:use #:cl
        #:marie
        #:vix/src/core))

(in-package #:vix/src/store)

(define-command store add^store-add ()
  "add a file or directory to the Nix store"
  nil nil nil
  "Add a directory to the store"
  "store-add ./dir")

(define-command store cat^store-cat ()
  "print the contents of a file in the Nix store on stdout"
  nil nil nil
  "Show the contents of a file in a binary cache"
  "store-cat --store https://cache.nixos.org/ /nix/store/0i2jd68mp5g6h2sa5k9c85rb80sn8hi9-hello-2.10/bin/hello")

(define-command store copy-log^store-copy-log ()
  "copy build logs between Nix stores"
  nil nil nil
  "Copy build logs between Nix stores"
  "copy-log --from https://cache.nixos.org --eval-store auto nixpkgs#hello")

;; TODO add example(s)
(define-command store copy-sigs^store-copy-sigs ()
  "copy store path signatures from substituters"
  nil nil nil
  "Copy sigs"
  "store-copy-sigs")

(define-command store delete^store-delete ()
  "delete paths from the Nix store"
  nil nil nil
  "Delete a specific store path"
  "store-delete /nix/store/yb5q57zxv6hgqql42d5r8b5k5mcq6kay-hello-2.10")

(define-command store diff-closures^store-diff-closures ()
  "show what packages and versions were added and removed"
  nil nil nil
  "Show what got added and removed between two versions of the NixOS system profile"
  "store-diff-closures /nix/var/nix/profiles/system-655-link /nix/var/nix/profiles/system-658-link")

(define-command store dump-path^store-dump-path ()
  "serialise a store path to stdout in NAR format"
  nil nil nil
  "To get a NAR from the binary cache https://cache.nixos.org/"
  "store-dump-path --store https://cache.nixos.org/ /nix/store/7crrmih8c52r8fbnqb933dxrsp44md93-glibc-2.25 > glibc.nar")

(define-command store gc^store-gc ()
  "perform garbage collection on a Nix store"
  nil nil nil
  "Delete unreachable paths in the Nix store"
  "store-gc")

(define-command store info^store-info (store-ping)
  "test whether a store can be accessed"
  nil nil nil
  "Test whether connecting to a remote Nix store via SSH works"
  "store-info --store ssh://mac1")

(define-command store ls^store-ls ()
  "show information about a path in the Nix store"
  nil nil nil
  "To list the contents of a store path in a binary cache"
  "store-ls --store https://cache.nixos.org/ --long --recursive /nix/store/0i2jd68mp5g6h2sa5k9c85rb80sn8hi9-hello-2.10")

(define-options store-make-content-addressed)
(define-handler store-make-content-addressed ("store" "make-content-addressed"))
(define-command store make-content-addressed^store-make-content-addressed ()
  "rewrite a path or closure to content-addressed form"
  nil t t
  "Create a content-addressed representation of the closure of `hello'"
  "store-make-content-addressed -n hello")

(define-command store optimise^store-optimise ()
  "replace identical files in the store by hard links"
  nil nil nil
  "Optimise the Nix store"
  "store-optimise")

(define-command store path-from-hash-part^store-path-from-hash-part ()
  "get a store path from its hash part"
  nil nil nil
  "Return the full store path with the given hash part"
  "store-path-from-hash-part --store https://cache.nixos.org/ 0i2jd68mp5g6h2sa5k9c85rb80sn8hi9")

(define-command store prefetch-file^store-prefetch-file ()
  "download a file into the Nix store"
  nil nil nil
  "Download a file to the Nix store"
  "store-prefetch-file https://releases.nixos.org/nix/nix-2.3.10/nix-2.3.10.tar.xz")

(define-command store repair^store-repair ()
  "repair store paths"
  nil nil nil
  "Repair a store path, after determining that it is corrupt"
  "store-repair /nix/store/yb5q57zxv6hgqql42d5r8b5k5mcq6kay-hello-2.10")

;; TODO add example(s)
(define-command store sign^store-sign ()
  "sign store paths with a local key"
  nil nil nil
  "Sign store"
  "store-sign")

(define-command store verify^store-verify ()
  "verify the integrity of store paths"
  nil nil nil
  "Verify the entire Nix store"
  "store-verify --all")
