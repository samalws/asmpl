{-# LANGUAGE OverloadedRecordDot #-}

module Compiler.Typechecker.Gather where

import Compiler.Typechecker.Types
import Compiler.Typechecker.Monads

import qualified Data.Set as S
import qualified Data.Map as M
import Control.Monad (when)
import Data.Maybe (isJust, fromJust)

-- ptb cant have any constraints that pta doesnt have
pushPtsGeq :: (GatherMonad m) => ProcType -> ProcType -> m ()
pushPtsGeq pta ptb = do
  let tmpa = pta.procTypeTemplate
  let tmpb = ptb.procTypeTemplate
  when (length tmpa.typeArgs /= length tmpb.typeArgs || length tmpa.nsArgs /= length tmpb.nsArgs || length pta.procTypeArgs /= length ptb.procTypeArgs) $ fail "mismatched procs: wrong template/arg length"

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
    modifyNsConstr :: (VarID, String, ProcType) -> (VarID, String, Template, [Type])
    modifyNsConstr (n,f,t) = (n,f,t.procTypeTemplate,snd <$> t.procTypeArgs) -- we want to ignore type names when checking equality
  let nsConstrsA' = S.map (modifyNsConstr . (\(n,f,t) -> (nsApplyRewriteHere n, f, procTypeApplyRewriteHere t))) tmpa.nsConstraints
  let nsConstrsB' = S.map modifyNsConstr tmpb.nsConstraints

  when (not $ S.null $ S.difference numConstrsB' numConstrsA') $ fail "mismatched procs: numeric constraints not matched"
  when (not $ S.null $ S.difference intConstrsB' intConstrsA') $ fail "mismatched procs: int constraints not matched"
  when (not $ S.null $ S.difference nsConstrsB' nsConstrsA') $ fail "mismatched procs: namespace constraints not matched"

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

gatherProcCall :: (GatherMonad m) => StmtID -> Stmt -> ProcType -> m () 
gatherProcCall sid pc@(ProcCall{}) pt = do
  when (length pc.callTypeArgs /= length pt.procTypeTemplate.typeArgs || length pc.callNSArgs /= length pt.procTypeTemplate.nsArgs || length pc.callArgs /= length pt.procTypeArgs) $ fail "incorrect number of proc arguments"
  let typesPaired = M.fromList $ pt.procTypeTemplate.typeArgs `zip` pc.callTypeArgs
  let nssPaired = M.fromList $ pt.procTypeTemplate.nsArgs `zip` pc.callNSArgs

  let typeApplyRewriteHere = typeApplyRewrite typesPaired
  let nsApplyRewriteHere = nsApplyRewrite nssPaired
  let procTypeApplyRewriteHere = procTypeApplyRewrite typesPaired nssPaired

  let expectedArgTypes = typeApplyRewriteHere . snd <$> pt.procTypeArgs
  realArgTypes <- mapM (getVarTypeAt sid) pc.callArgs

  sequence_ $ zipWith pushEqConstraint realArgTypes expectedArgTypes

  mapM_ (pushNumericConstraint . typeApplyRewriteHere . VarType) $ S.toList pt.procTypeTemplate.numericConstraints
  mapM_ (pushIntConstraint . typeApplyRewriteHere . VarType) $ S.toList pt.procTypeTemplate.intConstraints
  mapM_ (gatherNSConstraint . (\(n,f,t) -> (nsApplyRewriteHere n, f, procTypeApplyRewriteHere t))) $ S.toList pt.procTypeTemplate.nsConstraints

gatherProcCall _ _ _ = error "gatherProcCall called with non ProcCall value"

gatherStmt_ :: (GatherMonad m) => StmtID -> Stmt -> m ()
gatherStmt_ sid (AssignVar va vb _) = do
  ta <- getVarTypeAt sid va
  tb <- getVarTypeAt sid vb
  ta `pushEqConstraint` tb -- we can assign a|b <- a
gatherStmt_ sid (AssignLit v l _) = do
  tv <- getVarTypeAt sid v
  pushEqConstraint tv (litType l)
gatherStmt_ sid (AssignMember v r m _) = do
  tv <- getVarTypeAt sid v
  tr <- getVarTypeAt sid r
  tm <- newType
  pushMemberTypeConstraint tr m tm
  pushEqConstraint tv tm
gatherStmt_ sid pc@(ProcCall{}) = do
  pt <- getProcType pc.callNS pc.callFn
  gatherProcCall sid pc pt
gatherStmt_ sid (JNZ v _ _) = do
  tv <- getVarTypeAt sid v
  pushIntConstraint tv
gatherStmt_ sid (AssertVarType v t _) = do
  tv <- getVarTypeAt sid v
  pushEqConstraint tv t
gatherStmt_ _ Return = pure ()

gatherStmt :: (GatherMonad m) => StmtID -> m ()
gatherStmt sid = do
  proc <- getProc
  let Just thisStmt = sid `M.lookup` proc.procStmts
  gatherStmt_ sid thisStmt

  allVars <- getAllVars
  let
    allVarsList = S.toList allVars
    handleNextStmtVar sid' (geq,v) = do
      tv <- getVarTypeAt sid v
      tv' <- getVarTypeAt sid' v
      let pushConstraint = if geq then pushGeqConstraint else pushEqConstraint
      tv' `pushConstraint` tv -- since we "assign" tv' <- tv
    modifiedVarsList (AssertVarType v' _ _) = map (\v -> ((v == v'), v)) allVarsList
    modifiedVarsList _ = map (False,) allVarsList
    handleNextStmt sid' = do
      let Just nextStmt = sid' `M.lookup` proc.procStmts
      mapM_ (handleNextStmtVar sid') (modifiedVarsList nextStmt)
  mapM_ handleNextStmt (stmtSuccessors thisStmt)
