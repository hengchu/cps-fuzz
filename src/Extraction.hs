module Extraction where

import HFunctor
import Syntax as Syn
import Control.Lens
import Names
import Control.Monad.Catch

data Bop = Add  | Mult
         | Sub  | Div
         | Pow
         | IDiv | IMod
         | Lt   | Le | Gt | Ge
         | Eq_  | Neq
         | Conj | Disj
         | Is | IsNot
  deriving (Show, Eq, Ord)

data Uop = BNot
  deriving (Show, Eq, Ord)

data Exp = Binary Exp Bop Exp
         | List [Exp]
         | Tuple [Exp]
         | Index Exp Exp
         | Slice Exp (Maybe Exp) (Maybe Exp)
         | Unary Uop Exp
         | Call Exp [Exp]
         | CallBuiltin String [Exp]
         | Var UniqueName
         | Val Literal
         | None
  deriving (Show, Eq, Ord)

data Stmt = ExpStmt Exp
          | Assign  UniqueName Exp
          | Assert  Exp
          | Cond    Exp    [Stmt] [Stmt]
          | While   Exp    [Stmt]
          | Ret     Exp
          | Decl    FuncDecl
          | Skip
  deriving (Show, Eq, Ord)

data FuncDecl = FuncDecl {
  _fdName     :: UniqueName
  , _fdParams :: [UniqueName]
  , _fdBody   :: [Stmt]
  }
  deriving (Show, Eq, Ord)

makeLensesWith abbreviatedFields ''FuncDecl

data Extraction = E {
  _eStatements :: [Stmt],
  _eExpr :: Exp
  }

makeLensesWith abbreviatedFields ''Extraction

extractExprF ::
  (MonadThrowWithStack m, FreshM m) =>
  ExprF (K Extraction) a ->
  m (K Extraction a)
extractExprF (EVarF (Syn.Var bound)) = return . K $
  E [] $ Extraction.Var bound
extractExprF (ELamF (Syn.Var bound) (unK -> body)) = do
  funName <- gfresh "anonymous_fun"
  let fDecl = FuncDecl funName [bound] (body ^. statements ++ [Ret $ body ^. expr])
  return . K $ E [Decl fDecl] $ Extraction.Var funName
extractExprF (EAppF (unK -> f) (unK -> arg)) = do
  return . K $ E (f ^. statements ++ arg ^. statements) (Call (f ^. expr) [arg ^. expr])
extractExprF (ECompF (unK -> g) (unK -> f)) = do
  return . K $
    E (g ^. statements ++ f ^. statements)
      (CallBuiltin "fun_comp" [g ^. expr, f ^. expr])

extractRZControlF ::
  (MonadThrowWithStack m, FreshM m) =>
  ControlF (K Extraction) a ->
  m (K Extraction a)
extractRZControlF (CIfF (unK -> cond) (unK -> a) (unK -> b)) = do
  condResultName <- gfresh "cond_result"
  let stmts = cond ^. statements
              ++ [Cond (cond ^. expr)
                       (a ^. statements ++ [Assign condResultName (a ^. expr)])
                       (b ^. statements ++ [Assign condResultName (b ^. expr)])]
  return . K $
    E stmts (Extraction.Var condResultName)
extractRZControlF (CLoopF (unK -> acc) (unK -> pred) (unK -> iter)) = do
  accResultName <- gfresh "loop_acc"
  let condExpr = Call (pred ^. expr) [Extraction.Var accResultName]
  let iterExpr = Call (iter ^. expr) [Extraction.Var accResultName]
  let stmts = acc ^. statements
              ++ pred ^. statements
              ++ iter ^. statements
              ++ [Assign accResultName (acc ^. expr)]
              ++ [While condExpr [Assign accResultName iterExpr]]
  return . K $
    E stmts (Extraction.Var accResultName)

extractRZPrimF ::
  (MonadThrowWithStack m, FreshM m) =>
  PrimF (K Extraction) a ->
  m (K Extraction a)
extractRZPrimF (PLitF v) = return . K $ E [] (Val . toLiteral $ v)
extractRZPrimF (PAddF (unK -> a) (unK -> b)) = do
  return . K $ E (a ^. statements ++ b ^. statements) (Binary (a ^. expr) Add (b ^. expr))
extractRZPrimF (PSubF (unK -> a) (unK -> b)) = do
  return . K $ E (a ^. statements ++ b ^. statements) (Binary (a ^. expr) Sub (b ^. expr))
extractRZPrimF (PMultF (unK -> a) (unK -> b)) = do
  return . K $ E (a ^. statements ++ b ^. statements) (Binary (a ^. expr) Mult (b ^. expr))
extractRZPrimF (PDivF (unK -> a) (unK -> b)) = do
  return . K $ E (a ^. statements ++ b ^. statements) (Binary (a ^. expr) Div (b ^. expr))
extractRZPrimF (PAbsF (unK -> a)) = do
  return . K $ E (a ^. statements) (CallBuiltin "abs" [a ^. expr])
extractRZPrimF (PSignumF (unK -> a)) = do
  return . K $ E (a ^. statements) (CallBuiltin "signum" [a ^. expr])
extractRZPrimF (PExpF (unK -> a)) = do
  return . K $ E (a ^. statements) (CallBuiltin "math.exp" [a ^. expr])
extractRZPrimF (PSqrtF (unK -> a)) = do
  return . K $ E (a ^. statements) (CallBuiltin "math.sqrt" [a ^. expr])
extractRZPrimF (PLogF (unK -> a)) = do
  return . K $ E (a ^. statements) (CallBuiltin "math.log" [a ^. expr])
extractRZPrimF (PGTF (unK -> a) (unK -> b)) = do
  return . K $ E (a ^. statements) (Binary (a ^. expr) Gt (b ^. expr))
extractRZPrimF (PGEF (unK -> a) (unK -> b)) = do
  return . K $ E (a ^. statements) (Binary (a ^. expr) Ge (b ^. expr))
extractRZPrimF (PLTF (unK -> a) (unK -> b)) = do
  return . K $ E (a ^. statements) (Binary (a ^. expr) Lt (b ^. expr))
extractRZPrimF (PLEF (unK -> a) (unK -> b)) = do
  return . K $ E (a ^. statements) (Binary (a ^. expr) Le (b ^. expr))
extractRZPrimF (PEQF (unK -> a) (unK -> b)) = do
  return . K $ E (a ^. statements) (Binary (a ^. expr) Eq_ (b ^. expr))
extractRZPrimF (PNEQF (unK -> a) (unK -> b)) = do
  return . K $ E (a ^. statements) (Binary (a ^. expr) Neq (b ^. expr))
extractRZPrimF (PAndF (unK -> a) (unK -> b)) = do
  return . K $ E (a ^. statements) (Binary (a ^. expr) Conj (b ^. expr))
extractRZPrimF (POrF (unK -> a) (unK -> b)) = do
  return . K $ E (a ^. statements) (Binary (a ^. expr) Disj (b ^. expr))
extractRZPrimF (PJustF (unK -> a)) = do
  return . K $ E (a ^. statements) (a ^. expr)
extractRZPrimF PNothingF = do
  return . K $ E [] None
extractRZPrimF (PFromJustF (unK -> a)) = do
  return . K $ E (a ^. statements) (a ^. expr)
extractRZPrimF (PIsJustF (unK -> a)) = do
  return . K $ E (a ^. statements) (Binary (a ^. expr) IsNot None)
extractRZPrimF (PPairF (unK -> a) (unK -> b)) = do
  return . K $ E (a ^. statements ++ b ^. statements) (Tuple [a^.expr, b^.expr])
extractRZPrimF (PFstF (unK -> p)) = do
  return . K $ E (p ^. statements) (Index (p ^. expr) (Val (I 0)))
extractRZPrimF (PSndF (unK -> p)) = do
  return . K $ E (p ^. statements) (Index (p ^. expr) (Val (I 1)))
