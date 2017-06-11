{ mkDerivation, attoparsec, base, bytestring, containers, mtl
, stdenv, tasty, tasty-hunit, tasty-quickcheck, template-haskell
, transformers, transformers-compat
}:
mkDerivation {
  pname = "llvm-hs-pure";
  version = "4.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    attoparsec base bytestring containers mtl template-haskell
    transformers transformers-compat
  ];
  testHaskellDepends = [
    base containers mtl tasty tasty-hunit tasty-quickcheck transformers
    transformers-compat
  ];
  homepage = "http://github.com/llvm-hs/llvm-hs/";
  description = "Pure Haskell LLVM functionality (no FFI)";
  license = stdenv.lib.licenses.bsd3;
}
