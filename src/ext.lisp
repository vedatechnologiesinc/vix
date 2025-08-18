;;;; -*- mode: lisp; syntax: common-lisp; base: 10; -*-
;;;; ext.lisp --- ext operations

(uiop:define-package #:vix/src/ext
  (:use #:cl
        #:marie
        #:vix/src/core))

(in-package #:vix/src/ext)

(define-command nil develop (d)
  "run a dev shell"
  nil
  t
  t
  nil
  "Run a dev shell"
  "d"
  "Run a dev shell and run `htop' inside"
  "d -c htop")

(define-command nil make (m)
  "run `make' inside a dev shell"
  nil
  nil
  (lambda (cmd)
    (let ((args (clingon:command-arguments cmd)))
      (exe "develop" "--command" "make" args)))
  nil
  "Run `make' inside a dev shell"
  "make")

(define-command nil command (exec)
  "execute a command inside a dev shell"
  "command..."
  nil
  (lambda (cmd)
    (let ((args (clingon:command-arguments cmd)))
      (exe "develop" "--command" args)))
  nil
  "Run `ncdu' inside a dev shell"
  "exec ncdu")

(define-command nil search ()
  "search for packages"
  "[-n|<flake>] <package>..."
  t
  (lambda (cmd)
    (let* ((args (clingon:command-arguments cmd))
           (final-args (cons "search" args)))
      (apply #'exe final-args)))
  nil
  "Search in `nixpkgs' flake for packages named `firefox'"
  "search nixpkgs firefox")

(define-command nil find (/)
  "search for packages in the `nixpkgs' flake"
  "<package>..."
  nil
  (lambda (cmd)
    (let* ((args (clingon:command-arguments cmd))
           (final-args (list "search" "nixpkgs" (or-args args))))
      (apply #'exe final-args)))
  nil
  "Search in `nixpkgs' flake for packages named `firefox'"
  "/ firefox")

(define-command nil build (b)
  "build a derivation or fetch a store path"
  nil
  t
  t
  nil
  "Build the default package from the flake in the current directory"
  "build"
  "Build `hello' and `cowsay' from `nixpkgs' flake, leaving two result symlinks"
  "build n#hello n#cowsay")

(define-command nil run ()
  "run a Nix application"
  nil
  t
  t
  nil
  "Run `vim' from the `nixpkgs' flake"
  "run n#vim")

(define-command nil bundle ()
  "bundle an application so that it works outside of the Nix store"
  nil
  t
  t
  nil
  "Bundle `hello'"
  "bundle n#vim")

(define-command nil copy ()
  "start an interactive environment for evaluating Nix expressions"
  nil
  nil
  t
  nil
  "Copy all store paths from a local binary cache"
  "copy -- --all --from file:///tmp/cache")

(define-command nil edit ()
  "open the Nix expression of a Nix package in $EDITOR"
  nil
  t
  t
  nil
  "Open the Nix expression of the `hello' package"
  "edit n#hello")

(define-command nil eval (e)
  "evaluate a Nix expression"
  nil
  nil
  t
  nil
  "Evaluate a Nix expression given on the command line"
  "e -- --expr '1 + 2'"
  "Print the store path of the `hello' package"
  "e -- --raw n#hello")

(define-command nil fmt (format)
  "reformat your code in the standard style"
  nil
  nil
  t
  nil
  "Format the current flake"
  "fmt")

(define-command nil repl ()
  "start an interactive environment for evaluating Nix expressions"
  nil
  nil
  t
  nil
  "Evaluate some simple Nix expressions"
  "repl")

(define-command nil path-info ()
  "query information about store paths"
  nil
  t
  t
  nil
  "Print the store path produced by n#hello"
  "pi n#hello")

(define-command nil why-depends (wd)
  "show why a package has another package in its closure"
  nil
  t
  t
  nil
  "Show one path through the dependency graph leading from `hello' to `glibc'"
  "wd n#hello n#glibc")

(define-command nil shell (sh)
  "run a shell in which the specified packages are available"
  nil
  t
  (lambda (cmd)
    (let ((args (clingon:command-arguments cmd)))
      (exe "env" "shell" args)))
  nil
  "Start a shell providing `yt-dlp' from the `nixpkgs' flake"
  "sh n#yt-dlp")

(define-command nil print-dev-env (pde)
  "print shell code of derivation"
  nil
  t
  t
  nil
  "Get the build environment of `hello'"
  "print-dev-env n#hello")

(define-command nil daemon ()
  "daemon to perform store operations on behalf of non-root clients"
  nil
  nil
  t
  nil
  "Run the daemon"
  "daemon"
  "Run the daemon and force all connections to be trusted"
  "daemon -- --force-trusted")

(define-command nil realisation ()
  "manipulate a Nix realisation"
  nil
  t
  (lambda (cmd)
    (let ((args (clingon:command-arguments cmd)))
      (exe "realisation" "info" args)))
  nil
  "Show some information about the realisation of the package `hello'"
  "realisation n#hello")

(define-command nil upgrade-nix (un)
  "upgrade Nix to the latest stable version"
  ""
  nil
  t
  nil
  "Upgrade Nix to the stable version declared in `nixpkgs' flake"
  "upgrade-nix")

(define-command nil collect-garbage (g)
  "run the garbage collector"
  nil
  nil
  (lambda (cmd)
    (let ((args (clingon:command-arguments cmd)))
      (exe! `("nix-collect-garbage" ,@args))))
  nil
  "Garbage collect"
  "g"
  "Gargage collect and delete old versions"
  "g -- -d")

(define-command nil zsh-completions (zsh)
  "generate the Zsh completion script"
  ""
  nil
  (lambda (cmd)
    (let ((parent (clingon:command-parent cmd)))
      (clingon:print-documentation :zsh-completions parent t)))
  nil
  "Generate the Zsh completions of Vix and enable them"
  "zsh-completions > ~/.zsh-completions/_vix
cat >>! ~/.zshenv << EOF
fpath=(~/.zsh-completions $fpath)
autoload -U compinit
compinit
EOF")

(define-command nil print-doc (doc pd)
  "print the documentation"
  ""
  nil
  (lambda (cmd)
    (clingon:print-documentation :markdown (clingon:command-parent cmd) t))
  nil
  "Generate the Markdown documentation of Vix and save it to README.md"
  "print-doc > README.md")

(define-command nil where (w)
  "show the files of a package."
  "<package>"
  nil
  (lambda (cmd)
    (let* ((args (clingon:command-arguments cmd))
           (args-prefixed (prefix-nixpkgs args))
           (paths (loop :for input :in args-prefixed
                        :collect (string-trim '(#\Space #\Tab #\Newline)
                                              (eze "build" input "--print-out-paths" "--no-link")))))
      (loop :for path :in paths :do (exe! `("find" ,path)))))
  nil
  "Show the files that were installed by the `hello' package"
  "w hello"
  "Show the files that were installed by the `hello' and `firefox' packages"
  "w hello firefox")
