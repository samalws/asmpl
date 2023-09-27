{-# LANGUAGE OverloadedRecordDot #-}

module Compiler.Typechecker.Types where

import qualified Data.Set as S
import qualified Data.Map as M

import Data.Foldable (foldMap)

newtype StmtID = StmtID { fromStmtID :: Int } deriving (Show, Eq, Ord, Num, Enum)
newtype VarID = VarID { fromVarID :: Int } deriving (Show, Eq, Ord, Num, Enum)
newtype RecordMember = RecordMember { fromRecordMember :: Int } deriving (Show, Eq, Ord, Num, Enum)
newtype Tag = Tag { fromTag :: String } deriving (Show, Eq, Ord)
data Stmt = AssignVar VarID VarID StmtID
          | AssignVarGeq VarID VarID (Maybe Type) StmtID -- a:t <- b, where type(a) >= type(b)
          | AssignVarLeq VarID VarID (Maybe Type) StmtID -- a:t <- b, where type(a) <= type(b)
          | AssignLit VarID Literal StmtID
          | AssignMember VarID VarID RecordMember StmtID
          | ProcCall { callNS :: VarID, callFn :: String, callTypeArgs :: [Type], callNSArgs :: [VarID], callArgs :: [VarID], callNext :: StmtID }
          | JNZ VarID {- where you go if nonzero: -} StmtID {- where you go if zero: -} StmtID
          | Return
          | Nop StmtID
          | Unreachable
          deriving (Show, Eq, Ord)
data Literal = IntLiteral { litIsSigned :: Bool, litBits :: Int, litIntVal :: Integer }
             | FloatLiteral Float
             | DoubleLiteral Double
             | TagLiteral Tag
             deriving (Show, Eq, Ord)
data Type = IntType { signed :: Bool, bits :: Int }
          | FloatType
          | DoubleType
          | TagType Tag
          | RecordsType [[RecordEntryType]]
          | VarType VarID
          deriving (Show, Eq, Ord)
data Template = Template { typeArgs :: [VarID], nsArgs :: [VarID], numericConstraints :: S.Set VarID, intConstraints :: S.Set VarID, nsConstraints :: S.Set (VarID, String, ProcType) } deriving (Show, Eq, Ord)
-- TODO ideally namespaces should be templates too, but I don't feel like filling in the logic for that
-- (once you do this, have to modify getProcType as well as the defn of NamespaceValue and ProcCall)
data NamespaceValue = NSValue { fns :: M.Map String Proc } deriving (Show, Eq, Ord)
data RecordEntryType = RecordEntryType Type (Maybe RecordEntryConstraint) deriving (Show, Eq, Ord)
data RecordEntryConstraint = EqConstraint Literal | RangeConstraint Literal Literal deriving (Show, Eq, Ord)
data ProcType = ProcType { procTypeTemplate :: Template, procTypeArgs :: [(VarID, Type)] } deriving (Show, Eq, Ord)
data Proc = Proc { procType :: ProcType, procStmts :: M.Map StmtID Stmt } deriving (Show, Eq, Ord) -- 0 is entry point, not that it matters

stmtSuccessors :: Stmt -> S.Set StmtID
stmtSuccessors (AssignVar _ _ a) = S.singleton a
stmtSuccessors (AssignVarGeq _ _ _ a) = S.singleton a
stmtSuccessors (AssignVarLeq _ _ _ a) = S.singleton a
stmtSuccessors (AssignLit _ _ a) = S.singleton a
stmtSuccessors (AssignMember _ _ _ a) = S.singleton a
stmtSuccessors pc@(ProcCall{}) = S.singleton (callNext pc)
stmtSuccessors (JNZ _ a b) = S.fromList [a,b]
stmtSuccessors Return = S.empty
stmtSuccessors (Nop a) = S.singleton a
stmtSuccessors Unreachable = S.empty

litType :: Literal -> Type
litType i@(IntLiteral{}) = IntType { signed = litIsSigned i, bits = litBits i }
litType (FloatLiteral _) = FloatType
litType (DoubleLiteral _) = DoubleType
litType (TagLiteral t) = TagType t

typeApplyRewrite :: M.Map VarID Type -> Type -> Type
typeApplyRewrite tm t@(VarType i) = M.findWithDefault t i tm
typeApplyRewrite tm (RecordsType rs) = RecordsType $ map (map (\(RecordEntryType t c) -> RecordEntryType (typeApplyRewrite tm t) c)) rs
typeApplyRewrite _ t = t

nsApplyRewrite :: M.Map VarID VarID -> VarID -> VarID
nsApplyRewrite nm n = M.findWithDefault n n nm

procTypeApplyRewrite :: M.Map VarID Type -> M.Map VarID VarID -> ProcType -> ProcType
procTypeApplyRewrite tm nm pt = undefined -- TODO UHHHHHHHHHHh

procTypesEquivalent :: ProcType -> ProcType -> Bool
procTypesEquivalent pta ptb = undefined -- TODO

getVarSetStmt :: Stmt -> S.Set VarID
getVarSetStmt (AssignVar a b _) = S.fromList [a,b]
getVarSetStmt (AssignVarGeq a b _ _) = S.fromList [a,b]
getVarSetStmt (AssignVarLeq a b _ _) = S.fromList [a,b]
getVarSetStmt (AssignLit a _ _) = S.singleton a
getVarSetStmt (AssignMember a b _ _) = S.fromList [a,b]
getVarSetStmt pc@(ProcCall{}) = S.fromList pc.callArgs
getVarSetStmt (JNZ a _ _) = S.singleton a
getVarSetStmt (Nop _) = S.empty
getVarSetStmt Unreachable = S.empty
getVarSetStmt Return = S.empty

getVarSetProc :: Proc -> S.Set VarID
getVarSetProc p = S.fromList (fst <$> p.procType.procTypeArgs) `S.union` foldMap getVarSetStmt p.procStmts

getTypeVarSetProc :: Proc -> S.Set VarID
getTypeVarSetProc p = S.fromList p.procType.procTypeTemplate.typeArgs

getNSVarSetProc :: Proc -> S.Set VarID
getNSVarSetProc p = S.fromList p.procType.procTypeTemplate.nsArgs

safeMax :: S.Set VarID -> VarID
safeMax s = maximum (S.singleton 0 `S.union` s)

getHighestVarIDProc :: Proc -> VarID
getHighestVarIDProc p = maximum $ safeMax <$> [getVarSetProc p, getTypeVarSetProc p, getNSVarSetProc p]
