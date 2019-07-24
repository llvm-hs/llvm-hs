let
  default_nixpkgs = (import <nixpkgs> {}).fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "2797ddee7ddebbb1292ea7673c42d77bc82b8515";
    sha256 = "1gym77dlfpvflv6l5iq7nq5cqzlxw29pw6gv904s1qd88hw3y5rp";
  };
in

{ nixpkgs ? default_nixpkgs
, compiler ? "ghc865" }:
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
