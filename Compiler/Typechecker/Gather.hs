{-# LANGUAGE OverloadedRecordDot #-}

module Compiler.Typechecker.Gather where

import Compiler.Typechecker.Types
import Compiler.Typechecker.Monads

import qualified Data.Set as S
import qualified Data.Map as M

getProcType :: (GatherMonad m) => StmtID -> Stmt -> m (Maybe ProcType)
getProcType sid pc@(ProcCall{}) = undefined -- TODO
getProcType _ _ = error "getProcType called with non ProcCall value"

gatherProcCall :: (GatherMonad m) => StmtID -> Stmt -> ProcType -> m () 
gatherProcCall sid pc@(ProcCall{}) pt = undefined -- TODO
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
  Just pt <- getProcType sid pc
  gatherProcCall sid pc pt
gatherStmt_ sid (JNZ v _ _) = do
  tv <- getVarTypeAt sid v
  pushIntConstraint tv
gatherStmt_ sid (AssertVarType v t _) = do
  tv <- getVarTypeAt sid v
  pushEqConstraint tv t

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
