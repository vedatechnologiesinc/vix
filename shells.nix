{
  nixpkgs,
  pkgs,
  ...
}:
with pkgs;
let
  comDeps = [
    which
  ];
  extraLibs = [
    zstd
  ];
  addComDeps = list: list ++ comDeps;
in
rec {
  lisp = mkShell {
    buildInputs =
      addComDeps [
        sbcl
        libfixposix
      ]
      ++ extraLibs;
    LD_LIBRARY_PATH = nixpkgs.lib.strings.makeLibraryPath extraLibs;
  };
  cl = lisp;
  default = lisp;
}
