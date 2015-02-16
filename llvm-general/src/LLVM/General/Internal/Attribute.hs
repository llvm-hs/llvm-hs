{-# LANGUAGE
  TemplateHaskell,
  MultiParamTypeClasses,
  ConstraintKinds
  #-}
module LLVM.General.Internal.Attribute where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

import Data.Data
import Data.Either
import Data.List (genericSplitAt)
import Data.Bits

import qualified LLVM.General.Internal.FFI.LLVMCTypes as FFI

import qualified LLVM.General.AST.Attribute as A.A

import LLVM.General.Internal.Coding
import LLVM.General.Internal.EncodeAST

$(do
  let
    -- build an instance of the Coding class for lists of some kind of attribute
    genAttributeListCoding :: (Data a, Bits a) => TypeQ -> Name -> [(a, String)] -> DecsQ
    genAttributeListCoding type' ctn attributeData = do
      let
         lowZeroes :: Bits b => b -> Int
         lowZeroes b | testBit b 0 = 0
         lowZeroes b = 1 + lowZeroes (shiftR b 1)
         field :: Bits b => b -> (Int, Int)
         field b = 
             let s = lowZeroes b
                 w = lowZeroes (complement (shiftR b s))
             in 
               (s,w)
         attributeData' = [(mkName n, b, s,w) | (b,n) <- attributeData, let (s,w) = field b ]
      let m = varT . mkName $ "m"
      TyConI (NewtypeD _ _ _ (NormalC ctcn _) _) <- reify ctn
      let zero = [| $(conE ctcn) 0 |]
      sequence [
        instanceD (sequence [classP ''Monad [m]]) [t| EncodeM $(m) [$(type')] $(conT ctn) |] [
          funD (mkName "encodeM") [
            clause [] (normalB [| return . (
              let
                 encodeAlignment a | popCount a == 1 = 1 + popCount (a - 1)
                 encodeAlignment _ = error "Cannot encode alignment which is not a power of two"
                 encodeAttribute a = $(
                  caseE [| a |] $ flip map attributeData' $ \(n, b, s, w) ->
                    let bQ = (dataToExpQ (const Nothing) b)
                    in
                      case w of
                        1 -> match (conP n []) (normalB bQ) []
                        _ -> do 
                          a <- newName "a"
                          match 
                           (conP n [varP a])
                           (normalB [| ($(conE ctcn) (fromIntegral (encodeAlignment $(varE a)) `shiftL` s)) .&. $(bQ) |])
                           []
                  )
              in
                foldl (.|.) $(zero) . map encodeAttribute
             ) |]) []
           ]
         ],
 
        -- build a decoder which uses bit masking for multiple fields at once
        -- to eliminate multiple absent attributes in fewer tests
        instanceD (sequence [classP ''Monad [m]]) [t| DecodeM $(m) [$(type')] $(conT ctn) |] [
          funD (mkName "decodeM") [
            do
              bits <- newName "bits"
              clause [varP bits] (normalB 
                (let
                    code :: (Data a, Bits a) => [ (Name, a, Int, Int) ] -- attrs to handle
                         -> Int -- length (attrs), passed in to avoid recomputation
                         -> (a, ExpQ) -- (<bitmask for all the given attrs>, <code to decode the given attrs>)
                    code [(n, b, s, w)] _ = (
                       b, 
                       case w of
                         1 -> [| [ $(conE n) | testBit $(varE bits) s ] |]
                         _-> [| 
                               [ 
                                 $(do
                                    i' <- newName "i'"
                                    letE 
                                     [valD (conP ctcn [varP i']) (normalB [| i |]) []]
                                     [| $(conE n) (bit ((fromIntegral $(varE i')) - 1)) |])
                                 | let i = ($(varE bits) .&. $(dataToExpQ (const Nothing) b)) `shiftR` s, 
                                   i /= $(zero)
                               ]
                             |]
                      )
                    code attrs nAttrs =
                      let (nEarlyAttrs, nLateAttrs) = (nAttrs `div` 2, nAttrs - nEarlyAttrs)
                          (earlyAttrs, lateAttrs) = genericSplitAt nEarlyAttrs attrs
                          (earlyAttrBits, earlyAttrCode) = code earlyAttrs nEarlyAttrs
                          (lateAttrBits, lateAttrCode) = code lateAttrs nLateAttrs
                          attrBits = earlyAttrBits .|. lateAttrBits
                      in
                        (
                         attrBits, 
                         [| 
                            if ($(varE bits) .&. $(dataToExpQ (const Nothing) attrBits) /= $(zero)) then
                              ($earlyAttrCode ++ $lateAttrCode) 
                            else
                              []
                          |]
                        )
                in
                  [| return $(snd $ code attributeData' (length attributeData')) |]
                )
               ) []
            ]
          ]
        ]

  pi <- genAttributeListCoding [t| A.A.ParameterAttribute |] ''FFI.ParamAttr [
    (FFI.paramAttrZExt, "A.A.ZeroExt"),
    (FFI.paramAttrSExt, "A.A.SignExt"),
    (FFI.paramAttrInReg, "A.A.InReg"),
    (FFI.paramAttrStructRet, "A.A.SRet"),
    (FFI.paramAttrAlignment, "A.A.Alignment"),
    (FFI.paramAttrNoAlias, "A.A.NoAlias"),
    (FFI.paramAttrByVal, "A.A.ByVal"),
    (FFI.paramAttrNoCapture, "A.A.NoCapture"),
    (FFI.paramAttrNest, "A.A.Nest")
   ]

  fi <- genAttributeListCoding [t| A.A.FunctionAttribute |] ''FFI.FunctionAttr [
    (FFI.functionAttrNoReturn, "A.A.NoReturn"),
    (FFI.functionAttrNoUnwind, "A.A.NoUnwind"),
    (FFI.functionAttrReadNone, "A.A.ReadNone"),
    (FFI.functionAttrReadOnly, "A.A.ReadOnly"),
    (FFI.functionAttrNoInline, "A.A.NoInline"),
    (FFI.functionAttrAlwaysInline, "A.A.AlwaysInline"),
    (FFI.functionAttrOptimizeForSize, "A.A.OptimizeForSize"),
    (FFI.functionAttrStackProtect, "A.A.StackProtect"),
    (FFI.functionAttrStackProtectReq, "A.A.StackProtectReq"),
    (FFI.functionAttrNoRedZone, "A.A.NoRedZone"),
    (FFI.functionAttrNoImplicitFloat, "A.A.NoImplicitFloat"),
    (FFI.functionAttrNaked, "A.A.Naked"),
    (FFI.functionAttrInlineHint, "A.A.InlineHint"),
    (FFI.functionAttrStackAlignment, "A.A.StackAlignment"),
    (FFI.functionAttrReturnsTwice, "A.A.ReturnsTwice"),
    (FFI.functionAttrUWTable, "A.A.UWTable"),
    (FFI.functionAttrNonLazyBind, "A.A.NonLazyBind")
   ]
  return (pi ++ fi)
 )

instance EncodeM EncodeAST [Either A.A.GroupID A.A.FunctionAttribute] FFI.FunctionAttr where
  encodeM fas = do
    let (gids, as) = partitionEithers fas
    as <- encodeM as
    gs <- mapM referAttributeGroup gids
    return $ foldl (.|.) as gs
