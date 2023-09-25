module Compiler.Typechecker.Monads where

import Compiler.Typechecker.Types

import qualified Data.Set as S
import qualified Data.Map as M

-- global fns are put into a "global::" virtual namespace
data GlobalEnv = GlobalEnv { globalNamespaces :: M.Map VarID NamespaceValue }

class (MonadFail m) => GatherMonad m where
  getGlobalEnv :: m GlobalEnv
  getProc :: m Proc
  getAllVars :: m (S.Set VarID)
  getVarTypevarAt :: StmtID -> VarID -> m VarID
  pushEqConstraint :: Type -> Type -> m ()
  pushGeqConstraint :: Type -> Type -> m () -- a | b >= a
  pushMemberTypeConstraint :: Type -> RecordMember -> Type -> m () -- pushMemberTypeConstraint a b c: a.b has type c
  pushNumericConstraint :: Type -> m ()
  pushIntConstraint :: Type -> m ()
  newTypevar :: m VarID

getVarTypeAt :: (GatherMonad m) => StmtID -> VarID -> m Type
getVarTypeAt s v = VarType <$> getVarTypevarAt s v

newType :: (GatherMonad m) => m Type
newType = VarType <$> newTypevar
