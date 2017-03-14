let
  default_nixpkgs = (import <nixpkgs> {}).fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "68cc97d306d3187c142cfb2378852f28d47bc098";
    sha256 = "07zxbk4g4d51hf7dhsj6h7jy5c2iccm2lwaashj36inkhh9lrqa3";
  };
in

{ nixpkgs ? default_nixpkgs }:

let

  hsOverlay = self: super: {
    haskellPackages = super.haskellPackages.override {
      overrides = self': super': {
        llvm-hs-pure = super'.callPackage ./llvm-hs-pure {};
        llvm-hs = super'.callPackage ./llvm-hs {
          llvm-config = self.llvm_4;
        };
      };
    };
  };

  orig_pkgs = import nixpkgs {};
  pkgs = import orig_pkgs.path { overlays = [ hsOverlay ]; };

  env =
    let
      # Check that a package is not part of llvm-hs.
      notLlvmHs = p:
        p.pname or "" != "llvm-hs-pure" && p.pname or "" != "llvm-hs"
      ;
      # Determine if a package is a Haskell package or not.  Stolen from:
      # <nixpkgs/pkgs/development/haskell-modules/generic-builder.nix>
      isHaskellPkg = x: (x ? pname) && (x ? version) && (x ? env);
      isSystemPkg = x: !isHaskellPkg x;

      allDependencies =
        let inherit (pkgs.haskellPackages) llvm-hs-pure llvm-hs; in
        builtins.concatLists [
          llvm-hs-pure.nativeBuildInputs
          llvm-hs-pure.propagatedNativeBuildInputs
          llvm-hs.nativeBuildInputs
          llvm-hs.propagatedNativeBuildInputs
        ]
      ;
      haskellDependencies = builtins.filter (x: isHaskellPkg x && notLlvmHs x)
        allDependencies
      ;
      systemDependencies = builtins.filter isSystemPkg allDependencies;

      ghc = pkgs.haskellPackages.ghcWithPackages
        (ps: with ps; [ cabal-install ] ++ haskellDependencies)
      ;
    in
    pkgs.stdenv.mkDerivation {
      name = "llvm-hs-env";
      buildInputs = [ ghc ] ++ systemDependencies;
      shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
    }
  ;

in

env
