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
          | ProcCall { callNS :: VarID, callFn :: String, callTypeArgs :: [Type], callNSArgs :: [Namespace], callArgs :: [VarID], callNext :: StmtID }
          | JNZ VarID {- where you go if nonzero: -} StmtID {- where you go if zero: -} StmtID
          | AssertVarType VarID Type StmtID
          deriving (Show, Eq)
data Literal = IntLiteral { litIsSigned :: Bool, litBits :: Int, litIntVal :: Integer }
             | FloatLiteral Float
             | DoubleLiteral Double
             | TagLiteral Tag
             deriving (Show, Eq)
data Type = IntType { signed :: Bool, bits :: Int }
          | FloatType
          | DoubleType
          | TagType Tag
          | RecordsType [[RecordEntryType]]
          | VarType VarID
          deriving (Show, Eq)
data Template = Template { typeArgs :: [VarID], nsArgs :: [VarID], numericConstraints :: S.Set VarID, intConstraints :: S.Set VarID, nsConstraints :: S.Set (VarID, String, ProcType) } deriving (Show, Eq)
data Namespace = Namespace VarID [Type] [Namespace] deriving (Show, Eq)
data NamespaceValue = NSValue { nsTemplate :: Template, fns :: M.Map String Proc } deriving (Show, Eq)
data RecordEntryType = RecordEntryType Type (Maybe RecordEntryConstraint) deriving (Show, Eq)
data RecordEntryConstraint = EqConstraint Literal | RangeConstraint Literal Literal deriving (Show, Eq)
data ProcType = ProcType { procTypeTemplate :: Template, procTypeArgs :: [(VarID, Type)] } deriving (Show, Eq)
data Proc = Proc { procType :: ProcType, procStmts :: M.Map StmtID Stmt } deriving (Show, Eq) -- 0 is entry point, 1 is exit point

stmtSuccessors :: Stmt -> S.Set StmtID
stmtSuccessors (AssignVar _ _ id) = S.singleton id
-- ... TODO

litType :: Literal -> Type
litType i@(IntLiteral{}) = IntType { signed = litIsSigned i, bits = litBits i }
litType (FloatLiteral _) = FloatType
litType (DoubleLiteral _) = DoubleType
litType (TagLiteral t) = TagType t