{ mkDerivation, array, attoparsec, base, bytestring, Cabal
, containers, exceptions, llvm-config, llvm-hs-pure, mtl
, pretty-show, QuickCheck, stdenv, tasty, tasty-hunit
, tasty-quickcheck, template-haskell, temporary, transformers
, transformers-compat, utf8-string
}:
mkDerivation {
  pname = "llvm-hs";
  version = "4.1.0.0";
  src = ./.;
  configureFlags = [ "-fshared-llvm" ];
  setupHaskellDepends = [ base Cabal containers ];
  libraryHaskellDepends = [
    array attoparsec base bytestring containers exceptions llvm-hs-pure
    mtl template-haskell transformers transformers-compat utf8-string
  ];
  libraryToolDepends = [ llvm-config ];
  testHaskellDepends = [
    base bytestring containers llvm-hs-pure mtl pretty-show QuickCheck
    tasty tasty-hunit tasty-quickcheck temporary transformers
    transformers-compat
  ];
  homepage = "http://github.com/llvm-hs/llvm-hs/";
  description = "General purpose LLVM bindings";
  license = stdenv.lib.licenses.bsd3;
}
