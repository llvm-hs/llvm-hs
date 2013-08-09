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
                    StackAlign -> "s"
  in
  List.intercalate "-" (
    (case endianness dl of Just BigEndian -> ["E"]; Just LittleEndian -> ["e"]; _ -> [])
    ++
    (maybe [] (\s -> ["S" ++ show s]) (stackAlignment dl))
    ++
    [ "p" ++ (if a == 0 then "" else show a) ++ ":" ++ sTriple t | (AddrSpace a, t) <- Map.toList . pointerLayouts $ dl]
    ++
    [ atChar at ++ sTriple (s, ai) | ((at, s), ai) <- Map.toList . typeLayouts $ dl ]
    ++ 
    (maybe [] (\ns -> ["n" ++ (List.intercalate ":" (map show . Set.toList $ ns))]) (nativeSizes dl))
  )

parseDataLayout :: String -> Maybe DataLayout
parseDataLayout "" = Nothing
parseDataLayout s = 
  let
    num :: Parser Word32
    num = read <$> many digit
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
        at <- choice [
               char 'i' >> return IntegerAlign,
               char 'v' >> return VectorAlign,
               char 'f' >> return FloatAlign,
               char 'a' >> return AggregateAlign,
               char 's' >> return StackAlign
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


