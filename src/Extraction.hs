{-# LANGUAGE AllowAmbiguousTypes #-}

module Extraction where

import HFunctor
import Syntax as Syn
import Control.Lens
import Names
import Control.Monad.Catch
import Type.Reflection
import Data.Constraint
import Text.Printf

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
         | DotCall Exp String [Exp]
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

data ExtractionError = InternalError String
  deriving (Show, Eq, Ord)

instance Exception ExtractionError

data Zone = Orange | Other

newtype ExtractionFun m a =
  ExtractionFun { runExtraction :: Zone -> m Extraction }

class DefaultMamba ty where
  defaultMamba :: Exp

instance DefaultMamba Int where
  defaultMamba = CallBuiltin "cint" [Val (I 0)]

instance DefaultMamba Number where
  defaultMamba = CallBuiltin "cfloat" [Val (D 0)]

instance DefaultMamba Bool where
  defaultMamba = CallBuiltin "cint" [Val (I 0)]

instance (DefaultMamba a, DefaultMamba b) => DefaultMamba (a, b) where
  defaultMamba = Tuple [defaultMamba @a, defaultMamba @b]

instance DefaultMamba a => DefaultMamba (Distr a) where
  defaultMamba = defaultMamba @a

resolveDefaultMamba :: forall a. Typeable a => Maybe (Dict (DefaultMamba a))
resolveDefaultMamba =
  case eqTypeRep (typeRep @a) (typeRep @Int) of
    Just HRefl -> return Dict
    _ -> case eqTypeRep (typeRep @a) (typeRep @Number) of
      Just HRefl -> return Dict
      _ -> case eqTypeRep (typeRep @a) (typeRep @Bool) of
             Just HRefl -> return Dict
             _ -> case typeRep @a of
                    App tyCon (b :: _ b) ->
                      case tyCon of
                        App (comma :: _ comma) (a1 :: _ a1) ->
                          case eqTypeRep comma (typeRep @(,)) of
                            Just HRefl -> do
                              d1 <- withTypeable a1 $ resolveDefaultMamba @a1
                              d2 <- withTypeable b  $ resolveDefaultMamba @b
                              withDict d1 $
                                withDict d2 $
                                return Dict
                            _ -> Nothing
                        _ -> Nothing
                    _ -> Nothing

extractExprF ::
  (MonadThrowWithStack m, FreshM m) =>
  ExprF (ExtractionFun m) a ->
  ExtractionFun m a
extractExprF (EVarF (Syn.Var bound)) = ExtractionFun . const . return $
  E [] $ Extraction.Var bound
extractExprF (ELamF (Syn.Var bound) (runExtraction -> body)) = ExtractionFun $ \zone -> do
  funName <- gfresh "anonymous_fun"
  bodyExtr <- body zone
  let fDecl = FuncDecl funName [bound] (bodyExtr ^. statements ++ [Ret $ bodyExtr ^. expr])
  return $ E [Decl fDecl] $ Extraction.Var funName
extractExprF (EAppF (runExtraction -> f) (runExtraction -> arg)) = ExtractionFun $ \zone -> do
  fExtr <- f zone
  argExtr <- arg zone
  return $ E (fExtr ^. statements ++ argExtr ^. statements) (Call (fExtr ^. expr) [argExtr ^. expr])
extractExprF (ECompF (runExtraction -> g) (runExtraction -> f)) = ExtractionFun $ \zone -> do
  gExtr <- g zone
  fExtr <- f zone
  return $
    E (gExtr ^. statements ++ fExtr ^. statements)
      (CallBuiltin "fun_comp" [gExtr ^. expr, fExtr ^. expr])

defaultMambaExpr :: forall a m. (Typeable a, MonadThrowWithStack m) => m Exp
defaultMambaExpr =
  case resolveDefaultMamba @a of
    Just d -> withDict d $ return (defaultMamba @a)
    Nothing -> throwM' . InternalError $ printf "expected DefaultMamba for type %s" (show $ typeRep @a)

extractControlF ::
  (MonadThrowWithStack m, FreshM m) =>
  ControlF (ExtractionFun m) a ->
  ExtractionFun m a
extractControlF (CIfF (runExtraction -> cond) ((runExtraction -> a) :: _ r) (runExtraction -> b)) =
  ExtractionFun $ \zone ->
  case zone of
    Other -> do
      condResultName <- gfresh "cond_result"
      condExtr <- cond Other
      aExtr <- a Other
      bExtr <- b Other
      let stmts = condExtr ^. statements
                  ++ [Cond (condExtr ^. expr)
                           (aExtr ^. statements ++ [Assign condResultName (aExtr ^. expr)])
                           (bExtr ^. statements ++ [Assign condResultName (bExtr ^. expr)])]
      return $
        E stmts (Extraction.Var condResultName)
    Orange -> do
      condResultName <- gfresh "cond_result"
      initializeCondResultStmt <-
        (\defExp -> return $ Assign condResultName $
          CallBuiltin "MemValue" [defExp]) =<< defaultMambaExpr @r
      condExtr <- cond Orange
      aExtr <- a Orange
      bExtr <- b Orange
      trueBranchName <- gfresh "if_true"
      falseBranchName <- gfresh "if_false"
      let trueFun =
            FuncDecl trueBranchName [] $
            aExtr ^. statements ++
            [ExpStmt $ DotCall (Extraction.Var condResultName) "write" [aExtr ^. expr]]
      let falseFun =
            FuncDecl trueBranchName [] $
            bExtr ^. statements ++
            [ExpStmt $ DotCall (Extraction.Var condResultName) "write" [bExtr ^. expr]]
      let stmts = condExtr ^. statements
                  ++ aExtr ^. statements
                  ++ bExtr ^. statements
                  ++ [initializeCondResultStmt,
                      Decl trueFun,
                      Decl falseFun,
                      ExpStmt $ CallBuiltin "if_statement" $
                        [condExtr ^. expr, Extraction.Var trueBranchName, Extraction.Var falseBranchName],
                      Assign condResultName (DotCall (Extraction.Var condResultName) "read" [])
                     ]
      return $ E stmts (Extraction.Var condResultName)
extractControlF (CLoopF ((runExtraction -> acc) :: _ acc) (runExtraction -> pred) (runExtraction -> iter)) =
  ExtractionFun $ \zone ->
  case zone of
    Other -> do
      accResultName <- gfresh "loop_acc"
      accExtr  <- acc  Other
      predExtr <- pred Other
      iterExtr <- iter Other
      let condExpr = Call (predExtr ^. expr) [Extraction.Var accResultName]
      let iterExpr = Call (iterExtr ^. expr) [Extraction.Var accResultName]
      let stmts = accExtr ^. statements
                  ++ predExtr ^. statements
                  ++ iterExtr ^. statements
                  ++ [Assign accResultName (accExtr ^. expr)]
                  ++ [While condExpr [Assign accResultName iterExpr]]
      return $
        E stmts (Extraction.Var accResultName)
    Orange -> do
      loopResultName <- gfresh "loop_acc"
      accExtr  <- acc Orange
      predExtr <- pred Orange
      iterExtr <- iter Orange
      let initializeAccStmt =
            Assign loopResultName $
            CallBuiltin "MemValue" [accExtr ^. expr]
      loopBodyName <- gfresh "loop_body"
      let loopResultExpr = Extraction.Var loopResultName
      let iterFun =
            FuncDecl loopBodyName [loopResultName] $
            [ExpStmt $ DotCall loopResultExpr "write" [Call (iterExtr ^. expr) [loopResultExpr]]]
      let stmts = accExtr ^. statements
                  ++ predExtr ^. statements
                  ++ iterExtr ^. statements
                  ++ [initializeAccStmt,
                      Decl iterFun,
                      ExpStmt $
                       CallBuiltin "while_loop" $
                       [Extraction.Var loopBodyName, -- loop body function
                        predExtr ^. expr, -- loop condition function
                        loopResultExpr], -- initial loop accumulator value
                      Assign loopResultName (DotCall loopResultExpr "read" [])
                     ]
      return $
        E stmts (Extraction.Var loopResultName)

{-
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
-}
