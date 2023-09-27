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
  getVarTypevar :: VarID -> m VarID
  -- in the order that theyll eventually get resolved by unify:
  pushEqConstraint :: Type -> Type -> m ()
  pushMemberTypeConstraint :: Type -> RecordMember -> Type -> m () -- pushMemberTypeConstraint a b c: a.b has type c
  pushGeqConstraint :: Type -> Type -> m () -- a | b >= a
  pushNumericConstraint :: Type -> m ()
  pushIntConstraint :: Type -> m ()
  newTypevar :: m VarID

getVarType :: (GatherMonad m) => VarID -> m Type
getVarType = fmap VarType . getVarTypevar

newType :: (GatherMonad m) => m Type
newType = VarType <$> newTypevar

class (MonadFail m) => UnifyMonad m where
  lookupVar :: VarID -> m (Maybe Type)
  -- no double-assignments: assignVar v t >> assignVar v s should fail even if t == s
  -- assignVar v t should fail if varIsTemplateType v returns True
  assignVar :: VarID -> Type -> m ()
  varIsTemplateType :: VarID -> m Bool
  varAssumedNumeric :: VarID -> m Bool
  varAssumedInt :: VarID -> m Bool
  anythingHappened :: m () -> m Bool
