module Compiler.Typechecker.Types where

import qualified Data.Set as S
import qualified Data.Map as M

newtype StmtID = StmtID { fromStmtID :: Int } deriving (Show, Eq, Ord, Num)
newtype VarID = VarID { fromVarID :: Int } deriving (Show, Eq, Ord, Num)
newtype RecordMember = RecordMember { fromRecordMember :: Int } deriving (Show, Eq, Ord, Num)
newtype Tag = Tag { fromTag :: String } deriving (Show, Eq, Ord)
data Stmt = AssignVar VarID VarID StmtID
          | AssignLit VarID Literal StmtID
          | AssignMember VarID VarID RecordMember StmtID
          | ProcCall { callNS :: VarID, callFn :: String, callTypeArgs :: [Type], callNSArgs :: [VarID], callArgs :: [VarID], callNext :: StmtID }
          | JNZ VarID {- where you go if nonzero: -} StmtID {- where you go if zero: -} StmtID
          | AssertVarType VarID Type StmtID
          | Return
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
stmtSuccessors (AssignVar _ _ id) = S.singleton id
stmtSuccessors (AssignLit _ _ id) = S.singleton id
stmtSuccessors (AssignMember _ _ _ id) = S.singleton id
stmtSuccessors pc@(ProcCall{}) = S.singleton (callNext pc)
stmtSuccessors (JNZ _ a b) = S.fromList [a,b]
stmtSuccessors (AssertVarType _ _ id) = S.singleton id
stmtSuccessors Return = S.empty

litType :: Literal -> Type
litType i@(IntLiteral{}) = IntType { signed = litIsSigned i, bits = litBits i }
litType (FloatLiteral _) = FloatType
litType (DoubleLiteral _) = DoubleType
litType (TagLiteral t) = TagType t

typeApplyRewrite :: M.Map VarID Type -> M.Map VarID VarID -> Type -> Type
typeApplyRewrite = undefined -- TODO

nsApplyRewrite :: M.Map VarID Type -> M.Map VarID VarID -> VarID -> VarID
nsApplyRewrite = undefined -- TODO

procTypeApplyRewrite :: M.Map VarID Type -> M.Map VarID VarID -> ProcType -> ProcType
procTypeApplyRewrite = undefined -- TODO
