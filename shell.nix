let
  nixpkgs = (import <nixpkgs> {}).fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "68cc97d306d3187c142cfb2378852f28d47bc098";
    sha256 = "07zxbk4g4d51hf7dhsj6h7jy5c2iccm2lwaashj36inkhh9lrqa3";
  };

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

in

pkgs.haskellPackages.llvm-hs.env
