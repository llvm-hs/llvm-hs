{
  description = "General purpose LLVM bindings";
  inputs = {
    np.url = "github:nixos/nixpkgs?ref=haskell-updates";
    fu.url = "github:numtide/flake-utils?ref=master";
    nf.url = "github:numtide/nix-filter?ref=master";
    hls.url = "github:haskell/haskell-language-server?ref=master";
  };
  outputs = { self, np, fu, nf, hls }:
    with fu.lib;
    eachSystem [ "x86_64-linux" ] (system:
      let
        version = ghc:
          with np.lib;
          "${ghc}-${substring 0 8 self.lastModifiedDate}.${
            self.shortRev or "dirty"
          }";
        config = { };
        mkOverlay = ghc: final: _:
          with final;
          with haskell.packages."ghc${ghc}".extend (final: _:
            with final;
            with haskell.lib; {
              llvm-hs-pure = overrideCabal (callCabal2nix "llvm-hs-pure"
                (with nf.lib; filter { root = ./llvm-hs-pure; }) { })
                (o: { version = "${o.version}-${version ghc}"; });
              llvm-hs = overrideCabal (callCabal2nix "llvm-hs"
                (with nf.lib; filter { root = ./llvm-hs; }) {
                  inherit llvm-hs-pure;
                  llvm-config = llvmPackages_9.llvm;
                }) (o: { version = "${o.version}-${version ghc}"; });
            }); {
              "llvm-hs-pure-${ghc}" = llvm-hs;
              "llvm-hs-${ghc}" = llvm-hs-pure;
            };
        mkOverlays = ghcs: map mkOverlay ghcs;
        eachGHC = ghcs:
          let
            pkgs = (import np {
              inherit config system;
              overlays = (mkOverlays ghcs);
            });
          in with pkgs;
          with builtins; rec {
            inherit (pkgs) overlays;
            packages = flattenTree (recurseIntoAttrs (with lib.lists;
              foldr (ghc: s:
                {
                  "llvm-hs-${ghc}" = getAttr "llvm-hs-${ghc}" pkgs;
                  "llvm-hs-pure-${ghc}" = getAttr "llvm-hs-pure-${ghc}" pkgs;
                } // s) { } ghcs));
          };
      in eachGHC [ "902" "8107" ]);
}
