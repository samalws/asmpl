newtype StmtID = StmtID { fromStmtID :: Int } deriving (Show, Eq, Ord, Num)
newtype VarID = VarID { fromVarID :: Int } deriving (Show, Eq, Ord, Num)
newtype RecordMember = RecordMember { fromRecordMember :: Int } deriving (Show, Eq, Ord, Num)
newtype Tag = Tag { fromTag :: String } deriving (Show, Eq, Ord)
data Stmt = AssignVar VarID VarID StmtID
          | AssignLit VarID Literal StmtID
          | AssignMember VarID VarID RecordMember
          | ProcCall { callNS :: VarID, callFn :: String, callTypeArgs :: [Type], callNSArgs :: [Namespace], callArgs :: [VarID], callNext :: StmtID }
          | JZ VarID StmtID StmtID
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
data RecordEntryType = RecordEntryType Type (Maybe RecordEntryConstraint) deriving (Show, Eq)
data RecordEntryConstraint = EqConstraint Literal | RangeConstraint Literal Literal deriving (Show, Eq)
data ProcType = ProcType { typeArgs :: [VarID], nsArgs :: [VarID], numericConstraints :: S.Set VarID, intConstraints :: S.Set VarID, nsConstraints :: S.Set (VarID, String, ProcType), args :: [(VarID, Type)] }
data Proc = Proc { procType :: ProcType, procStmts :: M.Map StmtID Stmt } -- 0 is entry point, 1 is exit point
