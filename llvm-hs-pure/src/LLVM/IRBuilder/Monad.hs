{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-} -- For MonadState s (ModuleBuilderT m) instance
{-# LANGUAGE CPP #-}

module LLVM.IRBuilder.Monad where

import LLVM.Prelude

import Control.Monad.Cont
import Control.Monad.Except
import qualified Control.Monad.Fail as Fail
import Control.Monad.Identity
import qualified Control.Monad.Writer.Lazy as Lazy
import qualified Control.Monad.Writer.Strict as Strict
import Control.Monad.Writer (MonadWriter)
import Control.Monad.Reader
import qualified Control.Monad.RWS.Lazy as Lazy
import qualified Control.Monad.RWS.Strict as Strict
import qualified Control.Monad.State.Lazy as Lazy
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe
#if !(MIN_VERSION_mtl(2,2,2))
import Control.Monad.Trans.Identity
#endif

import Data.Bifunctor
import Data.Monoid (First(..))
import Data.String
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as M
import GHC.Stack

import LLVM.AST

import LLVM.IRBuilder.Internal.SnocList

-- | This provides a uniform API for creating instructions and inserting them
-- into a basic block: either at the end of a BasicBlock, or at a specific
-- location in a block.
newtype IRBuilderT m a = IRBuilderT { unIRBuilderT :: StateT IRBuilderState m a }
  deriving
    ( Functor, Alternative, Applicative, Monad, MonadCont, MonadError e
    , MonadFix, MonadIO, MonadPlus, MonadReader r, MonadTrans, MonadWriter w
    )

instance MonadFail m => MonadFail (IRBuilderT m) where
    fail str = IRBuilderT (StateT $ \ _ -> Fail.fail str)

type IRBuilder = IRBuilderT Identity

class Monad m => MonadIRBuilder m where
  liftIRState :: State IRBuilderState a -> m a

  default liftIRState
    :: (MonadTrans t, MonadIRBuilder m1, m ~ t m1)
    => State IRBuilderState a
    -> m a
  liftIRState = lift . liftIRState

instance Monad m => MonadIRBuilder (IRBuilderT m) where
  liftIRState (StateT s) = IRBuilderT $ StateT $ pure . runIdentity . s

-- | A partially constructed block as a sequence of instructions
data PartialBlock = PartialBlock
  { partialBlockName :: !Name
  , partialBlockInstrs :: SnocList (Named Instruction)
  , partialBlockTerm :: First (Named Terminator)
  }

emptyPartialBlock :: Name -> PartialBlock
emptyPartialBlock nm = PartialBlock nm mempty (First Nothing)

-- | Builder monad state
data IRBuilderState = IRBuilderState
  { builderSupply :: !Word
  , builderUsedNames :: !(Map ShortByteString Word)
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

-- | If no partial block exists, create a new block with a fresh label.
--
-- This is useful if you want to ensure that the label for the block
-- is assigned before another label which is not possible with
-- `modifyBlock`.
ensureBlock :: MonadIRBuilder m => m ()
ensureBlock = do
  mbb <- liftIRState $ gets builderBlock
  case mbb of
    Nothing -> do
      nm <- freshUnName
      liftIRState $ modify $ \s -> s { builderBlock = Just $! emptyPartialBlock nm }
    Just _ -> pure ()

modifyBlock
  :: MonadIRBuilder m
  => (PartialBlock -> PartialBlock)
  -> m ()
modifyBlock f = do
  mbb <- liftIRState $ gets builderBlock
  case mbb of
    Nothing -> do
      nm <- freshUnName
      liftIRState $ modify $ \s -> s { builderBlock = Just $! f $ emptyPartialBlock nm }
    Just bb ->
      liftIRState $ modify $ \s -> s { builderBlock = Just $! f bb }

-- | Generate a fresh name. The resulting name is numbered or
-- based on the name suggested with 'named' if that's used.
fresh :: MonadIRBuilder m => m Name
fresh = do
  msuggestion <- liftIRState $ gets builderNameSuggestion
  maybe freshUnName freshName msuggestion

-- | Generate a fresh name from a name suggestion
freshName :: MonadIRBuilder m => ShortByteString -> m Name
freshName suggestion = do
  usedNames <- liftIRState $ gets builderUsedNames
  let
    nameCount = fromMaybe 0 $ M.lookup suggestion usedNames
    unusedName = suggestion <> fromString ("_" <> show nameCount)
    updatedUsedNames = M.insert suggestion (nameCount + 1) usedNames
  liftIRState $ modify $ \s -> s { builderUsedNames = updatedUsedNames }
  return $ Name unusedName

-- | Generate a fresh numbered name
freshUnName :: MonadIRBuilder m => m Name
freshUnName = liftIRState $ do
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
  -- Ensure that the fresh identifier for the block is assigned before the identifier for the instruction.
  ensureBlock
  nm <- fresh
  modifyBlock $ \bb -> bb
    { partialBlockInstrs = partialBlockInstrs bb `snoc` (nm := instr)
    }
  pure (LocalReference retty nm)

-- | Emit instruction that returns void
emitInstrVoid
  :: MonadIRBuilder m
  => Instruction
  -> m ()
emitInstrVoid instr = do
  modifyBlock $ \bb -> bb
    { partialBlockInstrs = partialBlockInstrs bb `snoc` (Do instr)
    }
  pure ()

-- | Emit terminator
emitTerm
  :: MonadIRBuilder m
  => Terminator
  -> m ()
emitTerm term = modifyBlock $ \bb -> bb
  { partialBlockTerm = partialBlockTerm bb <> First (Just (Do term))
  }

-- | Starts a new block labelled using the given name and ends the previous
-- one. The name is assumed to be fresh.
emitBlockStart
  :: MonadIRBuilder m
  => Name
  -> m ()
emitBlockStart nm = do
  mbb <- liftIRState $ gets builderBlock
  case mbb of
    Nothing -> return ()
    Just bb -> do
      let
        instrs = getSnocList $ partialBlockInstrs bb
        newBb = case getFirst (partialBlockTerm bb) of
          Nothing   -> BasicBlock (partialBlockName bb) instrs (Do (Ret Nothing []))
          Just term -> BasicBlock (partialBlockName bb) instrs term
      liftIRState $ modify $ \s -> s
        { builderBlocks = builderBlocks s `snoc` newBb
        }
  liftIRState $ modify $ \s -> s { builderBlock = Just $ emptyPartialBlock nm }

-------------------------------------------------------------------------------
-- * High-level functionality
-------------------------------------------------------------------------------

-- | Starts a new block and ends the previous one
block
  :: MonadIRBuilder m
  => m Name
block = do
  nm <- fresh
  emitBlockStart nm
  return nm

-- | @ir `named` name@ executes the 'IRBuilder' @ir@ using @name@ as the base
-- name whenever a fresh local name is generated. Collisions are avoided by
-- appending numbers (first @"name"@, then @"name1"@, @"name2"@, and so on).
named
  :: MonadIRBuilder m
  => m r
  -> ShortByteString
  -> m r
named ir name = do
  before <- liftIRState $ gets builderNameSuggestion
  liftIRState $ modify $ \s -> s { builderNameSuggestion = Just name }
  result <- ir
  liftIRState $ modify $ \s -> s { builderNameSuggestion = before }
  return result

-- | Get the name of the currently active block.
--
-- This function will throw an error if there is no active block. The
-- only situation in which this can occur is if it is called before
-- any call to `block` and before emitting any instructions.
currentBlock :: HasCallStack => MonadIRBuilder m => m Name
currentBlock = liftIRState $ do
  name <- gets (fmap partialBlockName . builderBlock)
  case name of
    Just n -> pure n
    Nothing -> error "Called currentBlock when no block was active"

-- | Find out if the currently active block has a terminator.
--
-- This function will fail under the same condition as @currentBlock@
hasTerminator :: HasCallStack => MonadIRBuilder m => m Bool
hasTerminator = do
  current <- liftIRState $ gets builderBlock
  case current of
    Nothing    -> error "Called hasTerminator when no block was active"
    Just blk -> case getFirst (partialBlockTerm blk) of
      Nothing  -> return False
      Just _   -> return True

-------------------------------------------------------------------------------
-- mtl instances
-------------------------------------------------------------------------------

instance MonadState s m => MonadState s (IRBuilderT m) where
  state = lift . state

instance MonadIRBuilder m => MonadIRBuilder (ContT r m)
instance MonadIRBuilder m => MonadIRBuilder (ExceptT e m)
instance MonadIRBuilder m => MonadIRBuilder (IdentityT m)
instance MonadIRBuilder m => MonadIRBuilder (MaybeT m)
instance MonadIRBuilder m => MonadIRBuilder (ReaderT r m)
instance (MonadIRBuilder m, Monoid w) => MonadIRBuilder (Strict.RWST r w s m)
instance (MonadIRBuilder m, Monoid w) => MonadIRBuilder (Lazy.RWST r w s m)
instance MonadIRBuilder m => MonadIRBuilder (StateT s m)
instance MonadIRBuilder m => MonadIRBuilder (Lazy.StateT s m)
instance (Monoid w, MonadIRBuilder m) => MonadIRBuilder (Strict.WriterT w m)
instance (Monoid w, MonadIRBuilder m) => MonadIRBuilder (Lazy.WriterT w m)
