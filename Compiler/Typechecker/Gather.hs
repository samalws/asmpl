{-# LANGUAGE OverloadedRecordDot #-}

module Compiler.Typechecker.Gather where

import Compiler.Typechecker.Types
import Compiler.Typechecker.Monads

import qualified Data.Set as S
import qualified Data.Map as M
import Control.Monad (when)
import Data.Maybe (isJust, fromJust)

gatherNSConstraint :: (GatherMonad m) => StmtID -> (VarID, String, ProcType) -> m ()
gatherNSConstraint = undefined -- TODO

getProcType :: (GatherMonad m) => StmtID -> Stmt -> m ProcType
getProcType sid pc@(ProcCall{}) = do
  globalNsVal <- M.lookup pc.callNS . globalNamespaces <$> getGlobalEnv
  let isGlobal = isJust globalNsVal

  thisProcTemplate <- procTypeTemplate . procType <$> getProc
  let localNsConstrs = thisProcTemplate.nsConstraints
  let isLocal = pc.callNS `elem` thisProcTemplate.nsArgs

  if isGlobal then do
    let Just p = pc.callFn `M.lookup` fns (fromJust globalNsVal)
    pure p.procType
  else if isLocal then do
    let [(_,_,t)] = filter (\(n,f,_) -> n == pc.callNS && f == pc.callFn) $ S.toList localNsConstrs -- TODO MonadFail handles this correctly?
    pure t
  else fail "ProcCall with nonexistent namespace"

getProcType _ _ = error "getProcType called with non ProcCall value"

gatherProcCall :: (GatherMonad m) => StmtID -> Stmt -> ProcType -> m () 
gatherProcCall sid pc@(ProcCall{}) pt = do
  when (length pc.callTypeArgs /= length pt.procTypeTemplate.typeArgs || length pc.callNSArgs /= length pt.procTypeTemplate.nsArgs || length pc.callArgs /= length pt.procTypeArgs) $ fail "incorrect number of proc arguments"
  let typesPaired = M.fromList $ pt.procTypeTemplate.typeArgs `zip` pc.callTypeArgs
  let nssPaired = M.fromList $ pt.procTypeTemplate.nsArgs `zip` pc.callNSArgs
  let typeApplyTemplateHere = typeApplyTemplate typesPaired nssPaired
  let expectedArgTypes = typeApplyTemplateHere . snd <$> pt.procTypeArgs
  realArgTypes <- mapM (getVarTypeAt sid) pc.callArgs
  sequence_ $ zipWith pushEqConstraint realArgTypes expectedArgTypes
  mapM_ (pushNumericConstraint . typeApplyTemplateHere . VarType) $ S.toList pt.procTypeTemplate.numericConstraints
  mapM_ (pushIntConstraint . typeApplyTemplateHere . VarType) $ S.toList pt.procTypeTemplate.intConstraints
  mapM_ (gatherNSConstraint sid . (\(n,f,t) -> (nsApplyTemplate typesPaired nssPaired n, f, procTypeApplyTemplate typesPaired nssPaired t))) $ S.toList pt.procTypeTemplate.nsConstraints

gatherProcCall _ _ _ = error "gatherProcCall called with non ProcCall value"

gatherStmt_ :: (GatherMonad m) => StmtID -> Stmt -> m ()
gatherStmt_ sid (AssignVar va vb _) = do
  ta <- getVarTypeAt sid va
  tb <- getVarTypeAt sid vb
  ta `pushGeqConstraint` tb -- we can assign a|b <- a
gatherStmt_ sid (AssignLit v l _) = do
  tv <- getVarTypeAt sid v
  pushGeqConstraint tv (litType l)
gatherStmt_ sid (AssignMember v r m _) = do
  tv <- getVarTypeAt sid v
  tr <- getVarTypeAt sid r
  tm <- newType
  pushMemberTypeConstraint tr m tm
  pushGeqConstraint tv tm
gatherStmt_ sid pc@(ProcCall{}) = do
  pt <- getProcType sid pc
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
    handleNextStmtVar sid' v = do
      tv <- getVarTypeAt sid v
      tv' <- getVarTypeAt sid' v
      tv' `pushGeqConstraint` tv -- since we "assign" tv' <- tv
    modifiedVarsList (AssertVarType v _ _) = filter (/= v) allVarsList
    modifiedVarsList _ = allVarsList
    handleNextStmt sid' = do
      let Just nextStmt = sid' `M.lookup` proc.procStmts
      mapM_ (handleNextStmtVar sid') (modifiedVarsList nextStmt)
  mapM_ handleNextStmt (stmtSuccessors thisStmt)
