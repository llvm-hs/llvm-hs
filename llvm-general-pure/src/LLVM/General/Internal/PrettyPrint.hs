{-# LANGUAGE
  TemplateHaskell,
  QuasiQuotes,
  ViewPatterns,
  OverloadedStrings
  #-}
module LLVM.General.Internal.PrettyPrint where

import LLVM.General.Prelude

import Language.Haskell.TH 
import Language.Haskell.TH.Quote

import Data.Monoid
import Data.String
import Data.Maybe

import Data.List (intercalate)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Reader hiding (sequence, mapM)

data Branch
  = Fixed String
  | Variable String String
  | IndentGroup Tree
  deriving (Eq, Ord, Show)

type Tree = [Branch]

data PrettyShowEnv = PrettyShowEnv {
   prefixes :: Map String (Maybe String),
   precedence :: Int
 }
 deriving (Show)

defaultPrettyShowEnv :: PrettyShowEnv
defaultPrettyShowEnv = PrettyShowEnv {
    prefixes = Map.empty,
    precedence = 0
  }

type Qual a = Reader PrettyShowEnv a 

prec :: Int -> Qual a -> Qual a
prec p = local (\env -> env { precedence = p })

type QTree = Qual Tree

variable :: String -> String -> QTree
variable t f = return [Variable t f]

indentGroup :: QTree -> QTree
indentGroup = fmap (return . IndentGroup)

instance IsString QTree where
  fromString = return . return . Fixed

instance Monoid QTree where
  mempty = return mempty
  mappend a b = mappend <$> a <*> b

renderEx :: Int -> String -> PrettyShowEnv -> QTree -> String
renderEx threshold indent env ts =
  (\(l, t, f) -> if (l < threshold) then t else f) $ fit 0 (runReader ts env)
  where
    ind i = concat $ replicate i indent
    fit i branches = (sum ls, concat ts, concat fs)
      where
        bit (Fixed s) = (length s, s, s)
        bit (Variable t f) = (length f, f, concat [ s:(if s == '\n' then ind i else "") | s <- t ])
        bit (IndentGroup tree) = 
          let (l, t, f) = fit (i+1) tree
          in (l, t, if (l < threshold) then t else "\n" ++ ind (i+1) ++ f ++ "\n" ++ ind i)
        (ls, ts, fs) = unzip3 . map bit $ branches
            
render = renderEx 80 "  " defaultPrettyShowEnv

comma = "," <> variable "\n" " "
a <+> b = a <> " " <> b

punctuate :: QTree -> [QTree] -> QTree
punctuate a as = intercalate <$> a <*> sequence as

gParens o c content = o <> prec 0 (indentGroup content) <> c
parens = gParens "(" ")"
brackets = gParens "[" "]"
braces = gParens ("{" <> variable "" " ") (variable "" " " <> "}")

record :: QTree -> [(QTree,QTree)] -> QTree
record name fields = do
  name <+> braces (punctuate comma [ n <+> "=" <+> v | (n,v) <- fields ])

ctor :: QTree -> [QTree] -> QTree
ctor name fields = do
  p <- asks precedence
  parensIfNeeded appPrec (foldl (<+>) name fields)

-- | a class for simple pretty-printing with indentation a function only of syntactic depth.
class Show a => PrettyShow a where
  prettyShow :: a -> QTree
  prettyShowList :: [a] -> QTree

  prettyShow = fromString . show
  prettyShowList = brackets . punctuate comma . map prettyShow

instance PrettyShow a => PrettyShow [a] where
  prettyShow = prettyShowList


appPrec = 10
appPrec1 = 11

parensIfNeeded p' b = do
  p <- asks precedence
  let b' = prec (p'+1) b
  if (p > p') then parens b' else b'

instance PrettyShow Int
instance PrettyShow Bool
instance PrettyShow Integer
instance PrettyShow Double
instance PrettyShow Float
instance PrettyShow Word
instance PrettyShow Word16
instance PrettyShow Word32
instance PrettyShow Word64

instance PrettyShow Char where
  prettyShowList = fromString . show

instance PrettyShow a => PrettyShow (Set a) where
  prettyShow s = ctor "Set.fromList" [prettyShow (Set.toList s)]

instance (PrettyShow a, PrettyShow b) => PrettyShow (a, b) where
  prettyShow (a,b) = parens (prec appPrec1 (prettyShow a <> comma <> prettyShow b))

instance (PrettyShow k, PrettyShow a) => PrettyShow (Map k a) where
  prettyShow m = ctor "Map.fromList" [prettyShow (Map.toList m)]

data SimpleName = SimpleName (Maybe String) String
  deriving (Eq, Ord, Read, Show)

instance PrettyShow SimpleName where
  prettyShow (SimpleName mMod n) = do
    prs <- asks prefixes
    fromString $ fromMaybe n $ do
      mod <- mMod
      pr <- Map.findWithDefault (Just mod) mod prs
      return $ pr ++ "." ++ n

simpleName :: Name -> ExpQ
simpleName n = do
  let d :: Data d => d -> ExpQ
      d = dataToExpQ (const Nothing)
  [| prettyShow (SimpleName $(d (nameModule n)) $(d (nameBase n))) |]

makePrettyShowInstance :: Name -> DecsQ
makePrettyShowInstance n = do
  info <- reify n
  let (tvb, cons) = 
        case info of
          TyConI (DataD _ _ tvb cons _) -> (tvb, cons)
          TyConI (NewtypeD _ _ tvb con _) -> (tvb, [con])
          x -> error $ "unexpected info: " ++ show x
  cs <- mapM (const $ newName "a") tvb
  let cvs = map varT cs
  sequence . return $ instanceD (cxt [classP (mkName "PrettyShow") [cv] | cv <- cvs]) [t| PrettyShow $(foldl appT (conT n) cvs) |] [
    funD (mkName "prettyShow") [
       clause
         [varP (mkName "a")] (
           normalB $ caseE (dyn "a") $ flip map cons $ \con -> do
             case con of
               RecC conName (unzip3 -> (ns, _, _)) -> do
                 pvs <- mapM (const $ newName "f") ns
                 let ss = [| record $(simpleName conName) $(listE [[|($(simpleName n), prettyShow $(varE pv))|] | (n, pv) <- zip ns pvs]) |]
                 match 
                   (conP conName (map varP pvs))
                   (normalB ss)
                   []
               NormalC conName fs -> do
                 pvs <- mapM (const $ newName "f") fs
                 let ss = [| ctor $(simpleName conName) $(listE [[| prettyShow $(varE pv)|] | pv <- pvs]) |]
                 match 
                   (conP conName (map varP pvs))
                   (normalB ss)
                   []
               InfixC (_, n0) conName (_, n1) -> do
                 DataConI _ _ _ (Fixity prec _) <- reify conName
                 let ns = [n0, n1]
                 [p0,p1] <- mapM (const $ newName "f") ns
                 let ss = [| parensIfNeeded prec (prettyShow $(varE p0) <+> $(simpleName conName) <+> prettyShow $(varE p1)) |]
                 match
                   (uInfixP (varP p0) conName (varP p1))
                   (normalB ss)
                   []
               x -> error $ "unexpected constructor pattern: " ++ show x
         )
         []
     ]
   ]

  

