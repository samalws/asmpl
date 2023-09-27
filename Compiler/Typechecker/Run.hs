{-# LANGUAGE OverloadedRecordDot #-}

module Compiler.Typechecker.Run where

import Compiler.Typechecker.Types
import Compiler.Typechecker.Monads
import Compiler.Typechecker.Gather
import Compiler.Typechecker.Unify

import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad (when, unless)
import Control.Monad.Reader (ReaderT, runReaderT, asks)
import Control.Monad.State (StateT, runStateT, gets, modify)
import Data.Has (Has, getter, modifier)
import Data.Maybe (maybe, isJust)

newtype EitherString a = EitherString { runEitherString :: Either String a } deriving (Show, Eq, Ord, Functor, Applicative, Monad)

instance MonadFail EitherString where
  fail s = EitherString (Left s)

data EqConstr = EqConstr Type Type
data MemberTypeConstr = MemberTypeConstr Type RecordMember Type
data GeqConstr = GeqConstr Type Type
data NumericConstr = NumericConstr Type
data IntConstr = IntConstr Type

type GatherMonadImpl = ReaderT (GlobalEnv, Proc, S.Set VarID, M.Map VarID VarID) (StateT ([EqConstr],[MemberTypeConstr],[GeqConstr],[NumericConstr],[IntConstr],VarID) EitherString)

instance GatherMonad GatherMonadImpl where
  getGlobalEnv = asks getter
  getProc = asks getter
  getAllVars = asks getter
  getVarTypevar v = asks (M.lookup v . getter) >>= maybe (fail "Typevar lookup failed") pure
  pushEqConstraint a b = modify . modifier $ (EqConstr a b :)
  pushMemberTypeConstraint a b c = modify . modifier $ (MemberTypeConstr a b c :)
  pushGeqConstraint a b = modify . modifier $ (GeqConstr a b :)
  pushNumericConstraint a = modify . modifier $ (NumericConstr a :)
  pushIntConstraint a = modify . modifier $ (IntConstr a :)
  newTypevar = modify (modifier (succ :: VarID -> VarID)) >> gets getter

runGatherMonadImpl :: GlobalEnv -> Proc -> S.Set VarID -> M.Map VarID VarID -> VarID -> GatherMonadImpl t -> EitherString (t, ([EqConstr],[MemberTypeConstr],[GeqConstr],[NumericConstr],[IntConstr]))
runGatherMonadImpl globalEnv proc varIDs varMap maxVarID = fmap modifyResult . flip runStateT ([],[],[],[],[],maxVarID) . flip runReaderT (globalEnv, proc, varIDs, varMap) where
  modifyResult (retVal, (ca,cb,cc,cd,ce,_)) = (retVal,(ca,cb,cc,cd,ce))

newtype TemplateVarSet = TemplateVarSet { runTemplateVarSet :: S.Set VarID }
newtype NumericVarSet = NumericVarSet { runNumericVarSet :: S.Set VarID }
newtype IntVarSet = IntVarSet { runIntVarSet :: S.Set VarID }

makeTemplateVarSet :: Proc -> TemplateVarSet
makeTemplateVarSet = TemplateVarSet . getTypeVarSetProc

makeNumericVarSet :: Proc -> NumericVarSet
makeNumericVarSet = NumericVarSet . (.procType.procTypeTemplate.numericConstraints)

makeIntVarSet :: Proc -> IntVarSet
makeIntVarSet = IntVarSet . (.procType.procTypeTemplate.intConstraints)

type UnifyMonadImpl = ReaderT (TemplateVarSet, NumericVarSet, IntVarSet) (StateT (M.Map VarID Type) EitherString)

instance UnifyMonad UnifyMonadImpl where
  lookupVar = gets . M.lookup
  assignVar v t = do
    isAssigned <- isJust <$> lookupVar v
    isTmp <- varIsTemplateType v
    when isAssigned $ fail "Tried to reassign typevar"
    when isTmp $ fail "Tried to assign template typevar"
    modify (M.insert v t)
  varIsTemplateType v = asks (S.member v . runTemplateVarSet . getter)
  varAssumedNumeric v = asks (S.member v . runNumericVarSet . getter)
  varAssumedInt v = asks (S.member v . runIntVarSet . getter)
  anythingHappened m = do
    s <- gets M.size
    m
    s' <- gets M.size
    pure (s /= s')

runUnifyMonadImpl :: TemplateVarSet -> NumericVarSet -> IntVarSet -> UnifyMonadImpl t -> EitherString (t, M.Map VarID Type)
runUnifyMonadImpl t n i = flip runStateT M.empty . flip runReaderT (t,n,i)

makeVarMap :: VarID -> S.Set VarID -> M.Map VarID VarID
makeVarMap startID varset = M.fromList $ S.toList varset `zip` [startID..]

applySnd :: (Functor f) => (b -> f c) -> (a, b) -> f (a, c)
applySnd f (a, b) = (a,) <$> f b

mapMSnd :: (Monad m, Traversable t) => (b -> m c) -> t (a, b) -> m (t (a, c))
mapMSnd = mapM . applySnd

runGatherGlobal :: GlobalEnv -> EitherString (M.Map VarID (M.Map String (M.Map VarID VarID, ([EqConstr],[MemberTypeConstr],[GeqConstr],[NumericConstr],[IntConstr]))))
runGatherGlobal ge = fmap M.fromList $ mapMSnd f $ M.toList ge.globalNamespaces where
  f ns = fmap M.fromList $ mapMSnd g $ M.toList ns.fns
  g proc =
    let
      varset = getVarSetProc proc
      startID0 = succ $ getHighestVarIDProc proc
      varmap = makeVarMap startID0 varset
      startID = succ $ startID0 + VarID (M.size varmap)
    in runGatherMonadImpl ge proc varset varmap startID (gatherProc >> pure varmap)

sequenceMap :: (Monad m, Ord k) => M.Map k (m v) -> m (M.Map k v)
sequenceMap = fmap M.fromList . mapM (\(k,mv) -> (k,) <$> mv) . M.toList

mMapMapIndexed :: (Monad m, Ord k) => (k -> v -> m w) -> M.Map k v -> m (M.Map k w)
mMapMapIndexed f = sequenceMap . M.mapWithKey f

resolveMemberTypeConstrs :: (UnifyMonad m) => [MemberTypeConstr] -> m ()
resolveMemberTypeConstrs [] = pure ()
resolveMemberTypeConstrs cs = do
  changes <- mapM (\(MemberTypeConstr a b c) -> anythingHappened $ resolveMemberTypeConstraint a b c) cs
  unless (or changes) $ fail "Failed to resolve member type constraints (all are typevars)"
  let cs' = fmap fst $ filter snd $ cs `zip` changes
  resolveMemberTypeConstrs cs'

resolveConstrs :: (UnifyMonad m) => ([EqConstr],[MemberTypeConstr],[GeqConstr],[NumericConstr],[IntConstr]) -> M.Map VarID VarID -> m (M.Map VarID Type)
resolveConstrs (ca,cb,cc,cd,ce) varmap = do
  mapM_ (\(EqConstr a b) -> resolveEqConstraint a b) ca
  resolveMemberTypeConstrs cb
  mapM_ (\(GeqConstr a b) -> resolveGeqConstraint a b) cc
  mapM_ (\(NumericConstr a) -> resolveNumericConstraint a) cd
  mapM_ (\(IntConstr a) -> resolveIntConstraint a) ce
  mMapMapIndexed (const $ simplifyType . VarType) varmap

runUnifyGlobal :: GlobalEnv -> M.Map VarID (M.Map String (M.Map VarID VarID, ([EqConstr],[MemberTypeConstr],[GeqConstr],[NumericConstr],[IntConstr]))) -> EitherString (M.Map VarID (M.Map String (M.Map VarID Type)))
runUnifyGlobal ge = mMapMapIndexed (mMapMapIndexed . f) where
  f nsId procName (varmap, cs) =
    let proc = getProc nsId procName
    in fst <$> runUnifyMonadImpl (makeTemplateVarSet proc) (makeNumericVarSet proc) (makeIntVarSet proc) (resolveConstrs cs varmap)
  getProc nsId procName = (ge.globalNamespaces M.! nsId).fns M.! procName

runTypechecker :: GlobalEnv -> Either String (M.Map VarID (M.Map String (M.Map VarID Type)))
runTypechecker ge = runEitherString $ runGatherGlobal ge >>= runUnifyGlobal ge
