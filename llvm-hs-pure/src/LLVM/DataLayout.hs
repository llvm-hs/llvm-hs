{-# LANGUAGE OverloadedStrings #-}
module LLVM.DataLayout (
 dataLayoutToString,
 parseDataLayout
 ) where

import LLVM.Prelude

import Control.Monad.Trans.Except

import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8 as ByteString hiding (map, foldr)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

import LLVM.AST.DataLayout
import LLVM.AST.AddrSpace

dataLayoutToString :: DataLayout -> ByteString
dataLayoutToString dl =
  let sAlignmentInfo :: AlignmentInfo -> ByteString
      sAlignmentInfo (AlignmentInfo abi pref) =
        pack (show abi) <>
        if pref /= abi
          then ":" <> pack (show pref)
          else ""
      sTriple :: (Word32, AlignmentInfo) -> ByteString
      sTriple (s, ai) = pack (show s) <> ":" <> sAlignmentInfo ai
      atChar at = case at of
        IntegerAlign -> "i"
        VectorAlign -> "v"
        FloatAlign -> "f"
      manglingChar m = case m of
        ELFMangling -> "e"
        MIPSMangling -> "m"
        MachOMangling -> "o"
        WindowsCOFFMangling -> "w"
      oneOpt f accessor = maybe [] ((:[]) . f) (accessor dl)
      defDl = defaultDataLayout BigEndian
      nonDef :: Eq a => (DataLayout -> [a]) -> [a]
      nonDef f = (f dl) List.\\ (f defDl)
  in
  ByteString.intercalate "-" (
    [case endianness dl of BigEndian -> "E"; LittleEndian -> "e"]
    ++
    (oneOpt (("m:" <>) . manglingChar) mangling)
    ++
    [
      "p" <> (if a == 0 then "" else pack (show a)) <> ":" <> sTriple t
      | (AddrSpace a, t) <- nonDef (Map.toList . pointerLayouts)
    ] ++ [
      atChar at <> sTriple (s, ai)
      | ((at, s), ai) <- nonDef (Map.toList . typeLayouts)
    ] ++ [
      "a:" <> sAlignmentInfo ai | ai <- nonDef (pure . aggregateLayout)
    ] ++
    (oneOpt (("n"<>) . (ByteString.intercalate ":") . map (pack . show) . Set.toList) nativeSizes)
    ++
    (oneOpt (("S"<>) . pack . show) stackAlignment)
  )

-- | Parse a 'DataLayout', given a default Endianness should one not be specified in the
-- string to be parsed. LLVM itself uses BigEndian as the default: thus pass BigEndian to
-- be conformant or LittleEndian to be righteously defiant.
parseDataLayout :: Endianness -> ByteString -> Except String (Maybe DataLayout)
parseDataLayout _ "" = pure Nothing
parseDataLayout defaultEndianness str =
  let
    num :: Parser Word32
    num = read <$> many1 digit
    alignmentInfo :: Parser AlignmentInfo
    alignmentInfo = do
      abi <- num
      pref <- optional $ char ':' *> num
      let pref' = fromMaybe abi pref
      pure $ AlignmentInfo abi pref'
    triple :: Parser (Word32, AlignmentInfo)
    triple = do
      s <- num
      ai <- char ':' *> alignmentInfo
      pure (s, ai)
    parseSpec :: Parser (DataLayout -> DataLayout)
    parseSpec = choice [
      char 'e' *> pure (\dl -> dl { endianness = LittleEndian }),
      char 'E' *> pure (\dl -> dl { endianness = BigEndian }),
      do
        m <- char 'm' *> char ':' *> choice [
              char 'e' *> pure ELFMangling,
              char 'm' *> pure MIPSMangling,
              char 'o' *> pure MachOMangling,
              char 'w' *> pure WindowsCOFFMangling
             ]
        pure $ \dl -> dl { mangling = Just m },
      do
        n <- char 'S' *> num
        pure $ \dl -> dl { stackAlignment = Just n },
      do
        a <- char 'p' *> (AddrSpace <$> option 0 (read <$> many1 digit))
        t <- char ':' *> triple
        pure $ \dl -> dl { pointerLayouts = Map.insert a t (pointerLayouts dl) },
      do
        -- Ignore this obsolete approach to stack alignment.  After the 3.4 release,
        -- this is never generated, still parsed but ignored.  Comments suggest
        -- it will no longer be parsed after 4.0.
        void $ char 's' *> triple
        pure id,
      do
        at <- choice [
               char 'i' *> pure IntegerAlign,
               char 'v' *> pure VectorAlign,
               char 'f' *> pure FloatAlign
              ]
        (sz, ai) <- triple
        pure $ \dl -> dl { typeLayouts = Map.insert (at, sz) ai (typeLayouts dl) },
      do
        ai <- char 'a' *> char ':' *> alignmentInfo
        pure $ \dl -> dl { aggregateLayout = ai },
      do
        ns <- char 'n' *> num `sepBy` (char ':')
        pure $ \dl -> dl { nativeSizes = Just (Set.fromList ns) }
     ]
  in
    case parseOnly (parseSpec `sepBy` (char '-')) str of
      Left _ -> throwE $ "ill formed data layout: " ++ show str
      Right fs -> pure . Just $ foldr ($) (defaultDataLayout defaultEndianness) fs
