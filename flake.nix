{
  description = "General purpose LLVM bindings";
  inputs = {
    np.url = "github:nixos/nixpkgs?ref=haskell-updates";
    fu.url = "github:numtide/flake-utils?ref=master";
    nf.url = "github:numtide/nix-filter?ref=master";
    hls.url = "github:haskell/haskell-language-server?ref=master";
    lhc = {
      url = "github:luc-tielen/llvm-hs-combinators";
      flake = false;
    };
    lhp = {
      url = "github:llvm-hs/llvm-hs-pretty";
      flake = false;
    };
  };
  outputs = { self, np, fu, nf, hls, lhc, lhp }:
    with fu.lib;
    with builtins;
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
          with haskell.lib;
          with haskell.packages."ghc${ghc}".extend (final: _: rec { }); {
            "${ghc}" = rec {
              # inherit (haskell.packages."ghc${ghc}")
              #   llvm-hs llvm-hs-pure llvm-hs-pretty;
              llvm-hs-pure = overrideCabal (callCabal2nix "llvm-hs-pure"
                (with nf.lib; filter { root = ./llvm-hs-pure; }) { })
                (o: { version = "${o.version}-${version ghc}"; });
              llvm-hs = overrideCabal (callCabal2nix "llvm-hs"
                (with nf.lib; filter { root = ./llvm-hs; }) {
                  inherit llvm-hs-pure;
                  llvm-config = llvmPackages_9.llvm;
                }) (o: { version = "${o.version}-${version ghc}"; });
              llvm-hs-pretty = dontCheck (overrideCabal (addBuildTools
                (callCabal2nix "llvm-hs-pretty" "${lhp}" {
                  inherit llvm-hs llvm-hs-pure;
                }) [ hpack ]) (o: {
                  version = "${o.version}-${version ghc}";
                  patches = [ ./patches/1-llvm-hs-pretty.patch ];
                }));
              llvm-hs-combinators = dontCheck (overrideCabal (addBuildTools
                (callCabal2nix "llvm-hs-combinators" "${lhc}" {
                  inherit llvm-hs-pure;
                }) [ hpack ])
                (o: { version = "${o.version}-${version ghc}"; }));
            };
          };
        mkOverlays = ghcs: map mkOverlay ghcs;
        eachGHC = ghcs:
          let
            pkgs = (import np {
              inherit config system;
              overlays = (mkOverlays ghcs);
            });
          in with pkgs; rec {
            inherit (pkgs) overlays;
            packages = flattenTree (recurseIntoAttrs (with lib.lists;
              foldr
              (ghc: s: { "ghc${ghc}" = recurseIntoAttrs (pkgs."${ghc}"); } // s)
              { } ghcs));
          };
      in eachGHC [ "902" "8107" ]);
}
