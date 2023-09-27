module Compiler.Typechecker.Test where

import Compiler.Typechecker.Types
import Compiler.Typechecker.Monads
import Compiler.Typechecker.Gather
import Compiler.Typechecker.Unify
import Compiler.Typechecker.Run

import qualified Data.Map as M
import qualified Data.Set as S

emptyTemplate = Template { typeArgs = [], nsArgs = [], numericConstraints = S.empty, intConstraints = S.empty, nsConstraints = S.empty }

exampleProcType = ProcType { procTypeTemplate = emptyTemplate, procTypeArgs = [(0, FloatType), (1, FloatType), (2, DoubleType)] }

makeProcStmts = M.fromList . zip [0..]

exampleProc = Proc { procType = exampleProcType, procStmts = makeProcStmts [AssignVar 0 1 1, AssignVar 1 0 2, Return] }

makeNamespaceVal fs = NSValue { fns = M.fromList $ [show i | i <- [0..]] `zip` fs }

makeGlobalEnv fs = GlobalEnv { globalNamespaces = M.fromList [(0, makeNamespaceVal fs)] }

checkedExampleProc = runTypechecker $ makeGlobalEnv [exampleProc]

testMain = do
  print checkedExampleProc
