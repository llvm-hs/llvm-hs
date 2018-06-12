module LLVM.OrcJIT.LinkingLayer
  ( LinkingLayer(..)
  , disposeLinkingLayer
  , ObjectLinkingLayer(..)
  , newObjectLinkingLayer
  , withObjectLinkingLayer
  , addObjectFile
  , findSymbol
  , findSymbolIn
  ) where

import LLVM.Internal.OrcJIT.LinkingLayer
