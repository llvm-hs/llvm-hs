let
  default_nixpkgs = (import <nixpkgs> {}).fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "d587092e9e7df9786495c19f710cf6469d72eecb";
    sha256 = "1ygabmi2lmgy93a1zlmd7hw4ky83rjb6hn6ji40pj8flb437b8c4";
  };
in

{ nixpkgs ? default_nixpkgs
, compiler ? "ghc881" }:
let
  hsOverlay = self: super: {
    llvm_9 = (super.llvm_9.override { debugVersion = true; }).overrideAttrs(_: { doCheck = false; });
    haskell = super.haskell // {
      packages = super.haskell.packages // {
        "${compiler}" = super.haskell.packages."${compiler}".override {
          overrides = haskellSelf: haskellSuper: {
            llvm-hs = haskellSuper.callCabal2nix "llvm-hs" ./llvm-hs { llvm-config = self.llvm_9; };
            llvm-hs-pure = haskellSuper.callCabal2nix "llvm-hs-pure" ./llvm-hs-pure {};
          };
        };
      };
    };
  };

  orig_pkgs = import nixpkgs {};
  pkgs = import orig_pkgs.path { overlays = [ hsOverlay ]; };
in
pkgs.haskell.packages."${compiler}".shellFor {
  packages = pkgs: with pkgs; [llvm-hs llvm-hs-pure];
  nativeBuildInputs = with pkgs; [ llvm_9 gdb lldb ];
}
