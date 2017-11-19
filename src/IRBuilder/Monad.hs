{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module IRBuilder.Monad where

import Prelude hiding (and, or)

import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State.Strict

import Data.Bifunctor
import Data.Monoid
import Data.String
import Data.ByteString.Short as BS
import Data.HashSet(HashSet)
import qualified Data.HashSet as HS

import LLVM.AST

import Util.SnocList

-- | This provides a uniform API for creating instructions and inserting them
-- into a basic block: either at the end of a BasicBlock, or at a specific
-- location in a block.
newtype IRBuilderT m a = IRBuilderT { unIRBuilderT :: StateT IRBuilderState m a }
  deriving (Functor, Applicative, Monad, MonadFix, MonadTrans, MonadState IRBuilderState)

type IRBuilder = IRBuilderT Identity
type MonadIRBuilder = MonadState IRBuilderState

-- | A partially constructed block as a sequence of instructions
data PartialBlock = PartialBlock
  { partialBlockName :: !Name
  , partialBlockInstrs :: SnocList (Named Instruction)
  , partialBlockTerm :: Maybe (Named Terminator)
  }

emptyPartialBlock :: Name -> PartialBlock
emptyPartialBlock nm = PartialBlock nm mempty Nothing

-- | Builder monad state
data IRBuilderState = IRBuilderState
  { builderSupply :: !Word
  , builderUsedNames :: !(HashSet ShortByteString)
  , builderNameSuggestion :: !(Maybe ShortByteString)
  , builderBlocks :: SnocList BasicBlock
  , builderBlock :: !(Maybe PartialBlock)
  }

emptyIRBuilder :: IRBuilderState
emptyIRBuilder = IRBuilderState
  { builderSupply = 0
  , builderUsedNames = mempty
  , builderNameSuggestion = Nothing
  , builderBlocks = mempty
  , builderBlock = Nothing
  }

-- | Evaluate IRBuilder to a result and a list of basic blocks
runIRBuilder :: IRBuilderState -> IRBuilder a -> (a, [BasicBlock])
runIRBuilder s m = runIdentity $ runIRBuilderT s m

-- | Evaluate IRBuilderT to a result and a list of basic blocks
runIRBuilderT :: Monad m => IRBuilderState -> IRBuilderT m a -> m (a, [BasicBlock])
runIRBuilderT s m
  = second (getSnocList . builderBlocks)
  <$> runStateT (unIRBuilderT $ m <* block) s

-- | Evaluate IRBuilder to a list of basic blocks
execIRBuilder :: IRBuilderState -> IRBuilder a -> [BasicBlock]
execIRBuilder s m = snd $ runIRBuilder s m

-- | Evaluate IRBuilderT to a list of basic blocks
execIRBuilderT :: Monad m => IRBuilderState -> IRBuilderT m a -> m [BasicBlock]
execIRBuilderT s m = snd <$> runIRBuilderT s m

-------------------------------------------------------------------------------
-- * Low-level functionality
-------------------------------------------------------------------------------

modifyBlock
  :: MonadIRBuilder m
  => (PartialBlock -> PartialBlock)
  -> m ()
modifyBlock f = do
  mbb <- gets builderBlock
  case mbb of
    Nothing -> do
      nm <- freshUnName
      modify $ \s -> s { builderBlock = Just $! f $ emptyPartialBlock nm }
    Just bb ->
      modify $ \s -> s { builderBlock = Just $! f bb }

-- | Generate fresh name
fresh :: MonadIRBuilder m => m Name
fresh = do
  msuggestion <- gets builderNameSuggestion
  case msuggestion of
    Nothing -> freshUnName
    Just suggestion -> do
      usedNames <- gets builderUsedNames
      let
        candidates = suggestion : [suggestion <> fromString (show n) | n <- [(1 :: Int)..]]
        (unusedName:_) = filter (not . (`HS.member` usedNames)) candidates
      modify $ \s -> s { builderUsedNames = HS.insert unusedName $ builderUsedNames s }
      return $ Name unusedName

-- | Generate a fresh numbered name
freshUnName :: MonadIRBuilder m => m Name
freshUnName = do
  n <- gets builderSupply
  modify $ \s -> s { builderSupply = 1 + n }
  pure $ UnName n

-- | Emit instruction
emitInstr
  :: MonadIRBuilder m
  => Type -- ^ Return type
  -> Instruction
  -> m Operand
emitInstr retty instr = do
  nm <- fresh
  modifyBlock $ \bb -> bb
    { partialBlockInstrs = partialBlockInstrs bb `snoc` (nm := instr)
    }
  pure (LocalReference retty nm)

-- | Emit terminator
emitTerm
  :: MonadIRBuilder m
  => Terminator
  -> m ()
emitTerm term = modifyBlock $ \bb -> bb
  { partialBlockTerm = Just (Do term)
  }

-------------------------------------------------------------------------------
-- * High-level functionality
-------------------------------------------------------------------------------

-- | Starts a new block and ends the previous one
block
  :: MonadIRBuilder m
  => m Name
block = do
  mbb <- gets builderBlock
  case mbb of
    Nothing -> return ()
    Just bb -> do
      let
        instrs = getSnocList $ partialBlockInstrs bb
        newBb = case partialBlockTerm bb of
          Nothing   -> BasicBlock (partialBlockName bb) instrs (Do (Ret Nothing []))
          Just term -> BasicBlock (partialBlockName bb) instrs term
      modify $ \s -> s
        { builderBlocks = builderBlocks s `snoc` newBb
        }
  nm <- fresh
  modify $ \s -> s { builderBlock = Just $ emptyPartialBlock nm }
  pure nm

-- | @ir `named` name@ executes the 'IRBuilder' @ir@ using @name@ as the base
-- name whenever a fresh local name is generated. Collisions are avoided by
-- appending numbers (first @"name"@, then @"name1"@, @"name2"@, and so on).
named
  :: MonadIRBuilder m
  => m r
  -> ShortByteString
  -> m r
named ir name = do
  before <- gets builderNameSuggestion
  modify $ \s -> s { builderNameSuggestion = Just name }
  result <- ir
  modify $ \s -> s { builderNameSuggestion = before }
  return result
