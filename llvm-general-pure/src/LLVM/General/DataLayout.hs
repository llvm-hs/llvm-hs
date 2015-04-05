module LLVM.General.DataLayout (
 dataLayoutToString,
 parseDataLayout
 ) where

import LLVM.General.Prelude

import Control.Monad.Trans.Except

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

import Text.ParserCombinators.Parsec hiding (many)

import LLVM.General.AST.DataLayout
import LLVM.General.AST.AddrSpace

dataLayoutToString :: DataLayout -> String
dataLayoutToString dl = 
  let sAlignmentInfo :: AlignmentInfo -> String
      sAlignmentInfo (AlignmentInfo abi pref) = 
        show abi ++ case pref of
                      Just pref | pref /= abi -> ":" ++ show pref
                      _ -> ""
      sTriple :: (Word32, AlignmentInfo) -> String
      sTriple (s, ai) = show s ++ ":" ++ sAlignmentInfo ai
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
  List.intercalate "-" (
    [case endianness dl of BigEndian -> "E"; LittleEndian -> "e"]
    ++
    (oneOpt (("m:" ++) . manglingChar) mangling)
    ++
    [
      "p" ++ (if a == 0 then "" else show a) ++ ":" ++ sTriple t 
      | (AddrSpace a, t) <- nonDef (Map.toList . pointerLayouts)
    ] ++ [
      atChar at ++ sTriple (s, ai)
      | ((at, s), ai) <- nonDef (Map.toList . typeLayouts)
    ] ++ [
      "a:" ++ sAlignmentInfo ai | ai <- nonDef (return . aggregateLayout)
    ] ++
    (oneOpt (("n"++) . (List.intercalate ":") . (map show) . Set.toList) nativeSizes)
    ++
    (oneOpt (("S"++) . show) stackAlignment)
  )

-- | Parse a 'DataLayout', given a default Endianness should one not be specified in the
-- string to be parsed. LLVM itself uses BigEndian as the default: thus pass BigEndian to
-- be conformant or LittleEndian to be righteously defiant.
parseDataLayout :: Endianness -> String -> Except String (Maybe DataLayout)
parseDataLayout _ "" = return Nothing
parseDataLayout defaultEndianness s = 
  let
    num :: Parser Word32
    num = read <$> many1 digit
    alignmentInfo :: Parser AlignmentInfo
    alignmentInfo = do
      abi <- num
      pref <- optionMaybe $ do
                char ':'
                num
      return $ AlignmentInfo abi pref
    triple :: Parser (Word32, AlignmentInfo)
    triple = do
      s <- num
      char ':'
      ai <- alignmentInfo
      return (s, ai)
    parseSpec :: Parser (DataLayout -> DataLayout)
    parseSpec = choice [
      do
        char 'e'
        return $ \dl -> dl { endianness = LittleEndian },
      do
        char 'E' 
        return $ \dl -> dl { endianness = BigEndian },
      do
        char 'm'
        char ':'
        m <- choice [
              char 'e' >> return ELFMangling,
              char 'm' >> return MIPSMangling,
              char 'o' >> return MachOMangling,
              char 'w' >> return WindowsCOFFMangling
             ]
        return $ \dl -> dl { mangling = Just m },
      do
        char 'S'
        n <- num
        return $ \dl -> dl { stackAlignment = Just n },
      do
        char 'p'
        a <- AddrSpace <$> option 0 (read <$> many1 digit)
        char ':'
        t <- triple
        return $ \dl -> dl { pointerLayouts = Map.insert a t (pointerLayouts dl) },
      do
        char 's' -- Ignore this obsolete approach to stack alignment.  After the 3.4 release,
                 -- this is never generated, still parsed but ignored.  Comments suggest
                 -- it will no longer be parsed after 4.0.
        triple
        return id,
      do
        at <- choice [
               char 'i' >> return IntegerAlign,
               char 'v' >> return VectorAlign,
               char 'f' >> return FloatAlign
              ]
        (sz, ai) <- triple
        return $ \dl -> dl { typeLayouts = Map.insert (at, sz) ai (typeLayouts dl) },
      do
        char 'a'
        char ':'
        ai <- alignmentInfo
        return $ \dl -> dl { aggregateLayout = ai },
      do 
        char 'n'
        ns <- num `sepBy` (char ':')
        return $ \dl -> dl { nativeSizes = Just (Set.fromList ns) }
     ]
  in 
    case parse (parseSpec `sepBy` (char '-')) "" s of
      Left _ -> throwE $ "ill formed data layout: " ++ show s
      Right fs -> return . Just $ foldr ($) (defaultDataLayout defaultEndianness) fs


