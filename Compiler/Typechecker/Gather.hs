{-# LANGUAGE OverloadedRecordDot #-}

module Compiler.Typechecker.Gather where

import Compiler.Typechecker.Types
import Compiler.Typechecker.Monads

import qualified Data.Set as S
import qualified Data.Map as M
import Control.Monad (unless, zipWithM_)
import Data.Maybe (isJust, fromJust)

-- ptb cant have any constraints that pta doesnt have
pushPtsGeq :: (GatherMonad m) => ProcType -> ProcType -> m ()
pushPtsGeq pta ptb = do
  let tmpa = pta.procTypeTemplate
  let tmpb = ptb.procTypeTemplate
  unless (length tmpa.typeArgs == length tmpb.typeArgs && length tmpa.nsArgs == length tmpb.nsArgs && length pta.procTypeArgs == length ptb.procTypeArgs) $ fail "mismatched procs: wrong template/arg length"

  let typesPaired = M.fromList $ tmpa.typeArgs `zip` (VarType <$> tmpb.typeArgs)
  let nssPaired = M.fromList $ tmpa.nsArgs `zip` tmpb.nsArgs
  let argsPaired = (snd <$> pta.procTypeArgs) `zip` (snd <$> ptb.procTypeArgs)

  let typeApplyRewriteHere = typeApplyRewrite typesPaired
  let nsApplyRewriteHere = nsApplyRewrite nssPaired
  let procTypeApplyRewriteHere = procTypeApplyRewrite typesPaired nssPaired

  mapM_ (\(ta, tb) -> typeApplyRewriteHere ta `pushEqConstraint` tb) argsPaired

  let numConstrsA' = S.map (typeApplyRewriteHere . VarType) tmpa.numericConstraints
  let numConstrsB' = S.map VarType tmpb.numericConstraints
  let intConstrsA' = S.map (typeApplyRewriteHere . VarType) tmpa.intConstraints
  let intConstrsB' = S.map VarType tmpb.intConstraints
  let
    checkConstrB (nb,fb,tb) = do
      let [(_,_,ta)] = filter (\(na,fa,_) -> (nsApplyRewriteHere na,fa) == (nb,fb)) $ S.toList tmpa.nsConstraints
      unless (procTypeApplyRewriteHere ta `procTypesEquivalent` tb) $ fail "mismatched procs: namespace constraints not matched"

  unless (S.null $ S.difference numConstrsB' numConstrsA') $ fail "mismatched procs: numeric constraints not matched"
  unless (S.null $ S.difference intConstrsB' intConstrsA') $ fail "mismatched procs: int constraints not matched"
  mapM_ checkConstrB $ S.toList tmpb.nsConstraints

gatherNSConstraint :: (GatherMonad m) => (VarID, String, ProcType) -> m ()
gatherNSConstraint (ns, fn, pt) = pushPtsGeq pt =<< getProcType ns fn

getProcType :: (GatherMonad m) => VarID -> String -> m ProcType
getProcType ns fn = do
  globalNsVal <- M.lookup ns . globalNamespaces <$> getGlobalEnv
  let isGlobal = isJust globalNsVal

  thisProcTemplate <- procTypeTemplate . procType <$> getProc
  let localNsConstrs = thisProcTemplate.nsConstraints
  let isLocal = ns `elem` thisProcTemplate.nsArgs

  if isGlobal then do
    let Just p = fn `M.lookup` fns (fromJust globalNsVal)
    pure p.procType
  else if isLocal then do
    let [(_,_,t)] = filter (\(n,f,_) -> n == ns && f == fn) $ S.toList localNsConstrs -- TODO MonadFail handles this correctly?
    pure t
  else fail "ProcCall with nonexistent namespace"

gatherProcCall :: (GatherMonad m) => Stmt -> ProcType -> m () 
gatherProcCall pc@(ProcCall{}) pt = do
  unless (length pc.callTypeArgs == length pt.procTypeTemplate.typeArgs && length pc.callNSArgs == length pt.procTypeTemplate.nsArgs && length pc.callArgs == length pt.procTypeArgs) $ fail "incorrect number of proc arguments"
  let typesPaired = M.fromList $ pt.procTypeTemplate.typeArgs `zip` pc.callTypeArgs
  let nssPaired = M.fromList $ pt.procTypeTemplate.nsArgs `zip` pc.callNSArgs

  let typeApplyRewriteHere = typeApplyRewrite typesPaired
  let nsApplyRewriteHere = nsApplyRewrite nssPaired
  let procTypeApplyRewriteHere = procTypeApplyRewrite typesPaired nssPaired

  let expectedArgTypes = typeApplyRewriteHere . snd <$> pt.procTypeArgs
  realArgTypes <- mapM getVarType pc.callArgs

  zipWithM_ pushEqConstraint realArgTypes expectedArgTypes

  mapM_ (pushNumericConstraint . typeApplyRewriteHere . VarType) $ S.toList pt.procTypeTemplate.numericConstraints
  mapM_ (pushIntConstraint . typeApplyRewriteHere . VarType) $ S.toList pt.procTypeTemplate.intConstraints
  mapM_ (gatherNSConstraint . (\(n,f,t) -> (nsApplyRewriteHere n, f, procTypeApplyRewriteHere t))) $ S.toList pt.procTypeTemplate.nsConstraints

gatherProcCall _ _ = error "gatherProcCall called with non ProcCall value"

gatherStmt :: (GatherMonad m) => Stmt -> m ()
gatherStmt (AssignVar va vb _) = do
  ta <- getVarType va
  tb <- getVarType vb
  ta `pushEqConstraint` tb
gatherStmt (AssignVarGeq va vb ta' _) = do
  ta <- getVarType va
  tb <- getVarType vb
  maybe (pure ()) (pushEqConstraint ta) ta'
  ta `pushGeqConstraint` tb
gatherStmt (AssignVarLeq va vb ta' _) = do
  ta <- getVarType va
  tb <- getVarType vb
  maybe (pure ()) (pushEqConstraint ta) ta'
  tb `pushGeqConstraint` ta
gatherStmt (AssignLit v l _) = do
  tv <- getVarType v
  pushEqConstraint tv (litType l)
gatherStmt (AssignMember v r m _) = do
  tv <- getVarType v
  tr <- getVarType r
  tm <- newType
  pushMemberTypeConstraint tr m tm
  pushEqConstraint tv tm
gatherStmt pc@(ProcCall{}) = do
  pt <- getProcType pc.callNS pc.callFn
  gatherProcCall pc pt
gatherStmt (JNZ v _ _) = do
  tv <- getVarType v
  pushIntConstraint tv
gatherStmt (Nop _) = pure ()
gatherStmt Unreachable = pure ()
gatherStmt Return = pure ()

gatherProc :: (GatherMonad m) => m ()
gatherProc = do
  proc <- getProc
  mapM_ (\(v, t) -> pushEqConstraint t =<< getVarType v) proc.procType.procTypeArgs
  mapM_ gatherStmt (M.elems proc.procStmts)
