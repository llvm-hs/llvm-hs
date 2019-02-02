let
  default_nixpkgs = (import <nixpkgs> {}).fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "5acbe81573523cf3e64d37b03539d7083459ac42";
    sha256 = "0w0i88cdff89spzplhx546cdm5ijyka6q57f67569gk9xk84dcy4";
  };
in

{ nixpkgs ? <nixpkgs>
, compiler ? "ghc863" }:
let
  hsOverlay = self: super: {
    llvm_8 = super.llvm_8.override { debugVersion = true; };
    haskell = super.haskell // {
      packages = super.haskell.packages // {
        "${compiler}" = super.haskell.packages."${compiler}".override {
          overrides = haskellSelf: haskellSuper: {
            llvm-hs = haskellSuper.callCabal2nix "llvm-hs" ./llvm-hs { llvm-config = self.llvm_8; };
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
  nativeBuildInputs = with pkgs; [ llvm_7 gdb lldb ];
}
