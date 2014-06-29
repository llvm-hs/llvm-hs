module LLVM.General.DataLayout (
 dataLayoutToString,
 parseDataLayout
 ) where

import Control.Applicative

import Data.Word

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

import Text.ParserCombinators.Parsec hiding (many)

import LLVM.General.AST.DataLayout
import LLVM.General.AST.AddrSpace

dataLayoutToString :: DataLayout -> String
dataLayoutToString dl = 
  let sTriple :: (Word32, AlignmentInfo) -> String
      sTriple (s, ai) = show s ++ ":" ++ show (abiAlignment ai) ++ (maybe "" (\p -> ":" ++ show p) (preferredAlignment ai))
      atChar at = case at of
        IntegerAlign -> "i"
        VectorAlign -> "v"
        FloatAlign -> "f"
        AggregateAlign -> "a"
      manglingChar m = case m of
        ELFMangling -> "e"
        MIPSMangling -> "m"
        MachOMangling -> "o"
        WindowsCOFFMangling -> "w"
      oneOpt f accessor = maybe [] ((:[]) . f) (accessor dl)
  in
  List.intercalate "-" (
    (oneOpt (\e -> case e of BigEndian -> "E"; LittleEndian -> "e") endianness)
    ++
    (oneOpt (("m:" ++) . manglingChar) mangling)
    ++
    (oneOpt (("S"++) . show) stackAlignment)
    ++
    [ "p" ++ (if a == 0 then "" else show a) ++ ":" ++ sTriple t | (AddrSpace a, t) <- Map.toList . pointerLayouts $ dl]
    ++
    [ atChar at ++ sTriple (s, ai) | ((at, s), ai) <- Map.toList . typeLayouts $ dl ]
    ++ 
    (oneOpt (("n"++) . (List.intercalate ":") . (map show) . Set.toList) nativeSizes)
  )

parseDataLayout :: String -> Maybe DataLayout
parseDataLayout "" = Nothing
parseDataLayout s = 
  let
    num :: Parser Word32
    num = read <$> many1 digit
    triple :: Parser (Word32, AlignmentInfo)
    triple = do
      s <- num
      char ':'
      abi <- num
      pref <- optionMaybe $ do
                char ':'
                num
      return (s, (AlignmentInfo abi pref))
    parseSpec :: Parser (DataLayout -> DataLayout)
    parseSpec = choice [
      do
        char 'e'
        return $ \dl -> dl { endianness = Just LittleEndian },
      do
        char 'E' 
        return $ \dl -> dl { endianness = Just BigEndian },
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
               char 'f' >> return FloatAlign,
               char 'a' >> return AggregateAlign
              ]
        (sz,ai) <- triple
        return $ \dl -> dl { typeLayouts = Map.insert (at,sz) ai (typeLayouts dl) },
      do 
        char 'n'
        ns <- num `sepBy` (char ':')
        return $ \dl -> dl { nativeSizes = Just (Set.fromList ns) }
     ]
  in 
    case parse (parseSpec `sepBy` (char '-')) "" s of
      Left _ -> error $ "ill formed data layout: " ++ show s
      Right fs -> Just $ foldr ($) defaultDataLayout fs


