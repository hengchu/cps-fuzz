{-# LANGUAGE AllowAmbiguousTypes #-}

module Extraction where

import HFunctor
import Syntax as Syn
import Control.Lens
import Names
import Control.Monad.Catch
import Type.Reflection
import Data.Constraint (withDict, Dict(..))
import Text.Printf
import Compiler (compileM)
import Control.Monad.State.Strict
import qualified Data.Map as M

data Bop = Add   | Mult
         | Sub   | Div
         | Lt    | Le | Gt | Ge
         | Eq_   | Neq
         | Conj  | Disj
         | BConj | BDisj
         | Is    | IsNot
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
  , _fdAnnotations :: M.Map UniqueName String
  }
  deriving (Show, Eq, Ord)

makeLensesWith abbreviatedFields ''FuncDecl

data Extraction = E {
  _eStatements :: [Stmt],
  _eExpr :: Exp
  }
  deriving (Show, Eq, Ord)

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
  defaultMamba = CallBuiltin "cfix" [Val (D 0)]

instance DefaultMamba Bool where
  defaultMamba = CallBuiltin "cint" [Val (I 0)]

instance (DefaultMamba a, DefaultMamba b) => DefaultMamba (a, b) where
  defaultMamba = Tuple [defaultMamba @a, defaultMamba @b]

instance DefaultMamba a => DefaultMamba (Distr a) where
  defaultMamba = defaultMamba @a

lit2Mamba ::  Literal -> Exp
lit2Mamba lit@(I _) = CallBuiltin "cint" [Val lit]
lit2Mamba lit@(D _) = CallBuiltin "cfix" [Val lit]
lit2Mamba (P (a, b)) = Tuple [lit2Mamba a, lit2Mamba b]
lit2Mamba U = Tuple []
lit2Mamba (V (Vec v)) = Extraction.List . map (Val . D) $ v

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
extractExprF (ELamF ((Syn.Var bound) :: _ var) (runExtraction -> body)) = ExtractionFun $ \zone -> do
  funName <- gfresh "anonymous_fun"
  bodyExtr <- body zone
  let fDecl = FuncDecl funName [bound]
                (bodyExtr ^. statements ++ [Ret $ bodyExtr ^. expr])
                (M.fromList [(bound, show $ typeRep @var)])
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
            FuncDecl trueBranchName []
            (aExtr ^. statements ++
             [ExpStmt $ DotCall (Extraction.Var condResultName) "write" [aExtr ^. expr]])
            M.empty
      let falseFun =
            FuncDecl trueBranchName []
            (bExtr ^. statements ++
             [ExpStmt $ DotCall (Extraction.Var condResultName) "write" [bExtr ^. expr]])
            M.empty
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
extractControlF (CLoopPureF (runExtraction -> acc) (runExtraction -> pred) (runExtraction -> iter)) =
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
    Orange -> throwM' . InternalError $ printf "pure loop is not allowed in orange zone code"
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
            FuncDecl loopBodyName [loopResultName]
              [ExpStmt $ DotCall loopResultExpr "write" [Call (iterExtr ^. expr) [loopResultExpr]]]
              (M.fromList [(loopResultName, show $ typeRep @acc)])
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

naturalBase :: Exp
naturalBase = Val (D (Prelude.exp 1))

extractPrimF :: forall a m.
  (MonadThrowWithStack m, FreshM m) =>
  PrimF (ExtractionFun m) a ->
  ExtractionFun m a
extractPrimF (PLitF v) = ExtractionFun $ \zone ->
  case zone of
    Other  -> return $ E [] (Val . toLiteral $ v)
    Orange -> return $ E [] (lit2Mamba . toLiteral $ v)
extractPrimF (PAddF (runExtraction -> a) (runExtraction -> b)) = ExtractionFun $ \zone -> do
  aExtr <- a zone
  bExtr <- b zone
  return $ E (aExtr ^. statements ++ bExtr ^. statements) (Binary (aExtr ^. expr) Add (bExtr ^. expr))
extractPrimF (PSubF (runExtraction -> a) (runExtraction -> b)) = ExtractionFun $ \zone -> do
  aExtr <- a zone
  bExtr <- b zone
  return $ E (aExtr ^. statements ++ bExtr ^. statements) (Binary (aExtr ^. expr) Sub (bExtr ^. expr))
extractPrimF (PMultF (runExtraction -> a) (runExtraction -> b)) = ExtractionFun $ \zone -> do
  aExtr <- a zone
  bExtr <- b zone
  return $ E (aExtr ^. statements ++ bExtr ^. statements) (Binary (aExtr ^. expr) Mult (bExtr ^. expr))
extractPrimF (PDivF (runExtraction -> a) (runExtraction -> b)) = ExtractionFun $ \zone -> do
  aExtr <- a zone
  bExtr <- b zone
  return $ E (aExtr ^. statements ++ bExtr ^. statements) (Binary (aExtr ^. expr) Div (bExtr ^. expr))
extractPrimF (PAbsF (runExtraction -> a)) = ExtractionFun $ \zone -> do
  aExtr <- a zone
  case zone of
    Other -> do
      return $ E (aExtr ^. statements) (CallBuiltin "abs" [aExtr ^. expr])
    Orange -> do
      case eqTypeRep (typeRep @a) (typeRep @Number) of
        Just HRefl -> return $ E (aExtr ^. statements) (CallBuiltin "mpc_math.abs_fx" [aExtr ^. expr])
        Nothing ->
          throwM' . InternalError $
          printf "SCALE-MAMBA does not support abs() for type %s" (show $ typeRep @a)
extractPrimF (PSignumF _) = ExtractionFun . const $
  throwM' . InternalError $ "we do not support signum(), please compute sign explicitly"
extractPrimF (PExpF (runExtraction -> a)) = ExtractionFun $ \zone -> do
  aExtr <- a zone
  case zone of
    Other -> return $ E (aExtr ^. statements) (CallBuiltin "math.exp" [aExtr ^. expr])
    Orange -> do
      case eqTypeRep (typeRep @a) (typeRep @Number) of
        Just HRefl ->
          return $
          E (aExtr ^. statements)
            (CallBuiltin "mpc_math.pow_fx" [CallBuiltin "sint" [naturalBase], aExtr ^. expr])
        _ ->
          throwM' . InternalError $
          printf "SCALE-MAMBA does not support exp() for type %s" (show $ typeRep @a)
extractPrimF (PSqrtF (runExtraction -> a)) = ExtractionFun $ \zone -> do
  aExtr <- a zone
  case zone of
    Other -> return $ E (aExtr ^. statements) (CallBuiltin "math.sqrt" [aExtr ^. expr])
    Orange -> do
      case eqTypeRep (typeRep @a) (typeRep @Number) of
        Just HRefl ->
          return $
          E (aExtr ^. statements)
            (CallBuiltin "mpc_math.sqrt_simplified_fx" [aExtr ^. expr])
        _ -> throwM' . InternalError $
             printf "SCALE-MAMBA does not support sqrt() for type %s" (show $ typeRep @a)
extractPrimF (PLogF (runExtraction -> a)) = ExtractionFun $ \zone -> do
  aExtr <- a zone
  case zone of
    Other -> return $ E (aExtr ^. statements) (CallBuiltin "math.log" [aExtr ^. expr])
    Orange -> do
      case eqTypeRep (typeRep @a) (typeRep @Number) of
        Just HRefl ->
          return $
          E (aExtr ^. statements)
            (CallBuiltin "mpc_math.log_fx" [aExtr ^. expr])
        _ -> throwM' . InternalError $
             printf "SCALE-MAMBA does not support log() for type %s" (show $ typeRep @a)
extractPrimF (PGTF (runExtraction -> a) (runExtraction -> b)) = ExtractionFun $ \zone -> do
  aExtr <- a zone
  bExtr <- b zone
  return $ E (aExtr ^. statements ++ bExtr ^. statements) (Binary (aExtr ^. expr) Gt (bExtr ^. expr))
extractPrimF (PGEF (runExtraction -> a) (runExtraction -> b)) = ExtractionFun $ \zone -> do
  aExtr <- a zone
  bExtr <- b zone
  return $ E (aExtr ^. statements ++ bExtr ^. statements) (Binary (aExtr ^. expr) Ge (bExtr ^. expr))
extractPrimF (PLTF (runExtraction -> a) (runExtraction -> b)) = ExtractionFun $ \zone -> do
  aExtr <- a zone
  bExtr <- b zone
  return $ E (aExtr ^. statements ++ bExtr ^. statements) (Binary (aExtr ^. expr) Lt (bExtr ^. expr))
extractPrimF (PLEF (runExtraction -> a) (runExtraction -> b)) = ExtractionFun $ \zone -> do
  aExtr <- a zone
  bExtr <- b zone
  return $ E (aExtr ^. statements ++ bExtr ^. statements) (Binary (aExtr ^. expr) Le (bExtr ^. expr))
extractPrimF (PEQF (runExtraction -> a) (runExtraction -> b)) = ExtractionFun $ \zone -> do
  aExtr <- a zone
  bExtr <- b zone
  return $ E (aExtr ^. statements ++ bExtr ^. statements) (Binary (aExtr ^. expr) Eq_ (bExtr ^. expr))
extractPrimF (PNEQF (runExtraction -> a) (runExtraction -> b)) = ExtractionFun $ \zone -> do
  aExtr <- a zone
  bExtr <- b zone
  return $ E (aExtr ^. statements ++ bExtr ^. statements) (Binary (aExtr ^. expr) Neq (bExtr ^. expr))
extractPrimF (PAndF (runExtraction -> a) (runExtraction -> b)) = ExtractionFun $ \zone -> do
  aExtr <- a zone
  bExtr <- b zone
  case zone of
    Other ->
      return $ E (aExtr ^. statements ++ bExtr ^. statements) (Binary (aExtr ^. expr) Conj (bExtr ^. expr))
    Orange ->
      return $ E (aExtr ^. statements ++ bExtr ^. statements) (Binary (aExtr ^. expr) BConj (bExtr ^. expr))
extractPrimF (POrF (runExtraction -> a) (runExtraction -> b)) = ExtractionFun $ \zone -> do
  aExtr <- a zone
  bExtr <- b zone
  case zone of
    Other ->
      return $ E (aExtr ^. statements ++ bExtr ^. statements) (Binary (aExtr ^. expr) Disj (bExtr ^. expr))
    Orange ->
      return $ E (aExtr ^. statements ++ bExtr ^. statements) (Binary (aExtr ^. expr) BDisj (bExtr ^. expr))
extractPrimF (PJustF (runExtraction -> a)) = ExtractionFun $ \zone -> do
  aExtr <- a zone
  case zone of
    Other -> return aExtr
    Orange ->
      throwM' . InternalError $
      "SCALE-MAMBA does not support just(), please remove Maybe values from orange zone computation"
extractPrimF PNothingF = ExtractionFun $ \zone -> do
  case zone of
    Other -> return $ E [] None
    Orange ->
      throwM' . InternalError $
      "SCALE-MAMBA does not support nothing value, please remove Maybe values from orange zone computation"
extractPrimF (PFromJustF (runExtraction -> a)) = ExtractionFun $ \zone -> do
  aExtr <- a zone
  case zone of
    Other -> return aExtr
    Orange ->
      throwM' . InternalError $
      "SCALE-MAMBA does not support fromJust(), please remove Maybe values from orange zone computation"
extractPrimF (PIsJustF (runExtraction -> a)) = ExtractionFun $ \zone -> do
  aExtr <- a zone
  case zone of
    Other -> return $ E (aExtr ^. statements) (Binary (aExtr ^. expr) IsNot None)
    Orange ->
      throwM' . InternalError $
      "SCALE-MAMBA does not support isJust(), please remove Maybe values from orange zone computation"
extractPrimF (PPairF (runExtraction -> a) (runExtraction -> b)) = ExtractionFun $ \zone -> do
  aExtr <- a zone
  bExtr <- b zone
  return $ E (aExtr ^. statements ++ bExtr ^. statements) (Tuple [aExtr^.expr, bExtr^.expr])
extractPrimF (PFstF (runExtraction -> a)) = ExtractionFun $ \zone -> do
  aExtr <- a zone
  return $ E (aExtr ^. statements) (Index (aExtr ^. expr) (Val (I 0)))
extractPrimF (PSndF (runExtraction -> a)) = ExtractionFun $ \zone -> do
  aExtr <- a zone
  return $ E (aExtr ^. statements) (Index (aExtr ^. expr) (Val (I 1)))
extractPrimF (PLengthF (runExtraction -> a)) = ExtractionFun $ \zone -> do
  aExtr <- a zone
  return $ E (aExtr ^. statements) (CallBuiltin "len" [aExtr ^. expr])
extractPrimF (PIndexF (runExtraction -> a) (runExtraction -> idx)) = ExtractionFun $ \zone -> do
  aExtr <- a zone
  idxExtr <- idx zone
  return $ E (aExtr ^. statements ++ idxExtr ^. statements) (Index (aExtr ^. expr) (idxExtr ^. expr))
extractPrimF (PSliceF (runExtraction -> a) (runExtraction -> start) (runExtraction -> end)) = ExtractionFun $ \zone -> do
  aExtr <- a zone
  startExtr <- start zone
  endExtr <- end zone
  return $ E (aExtr ^. statements ++ startExtr ^. statements ++ endExtr ^. statements) (Slice (aExtr ^. expr) (Just $ startExtr ^. expr) (Just $ endExtr ^. expr))
extractPrimF (PVecLitF (map runExtraction -> as)) = ExtractionFun $ \zone -> do
  asExtrs <- traverse ($ zone) as
  return $ E (foldr (++) [] (map (^. statements) asExtrs)) (Extraction.List $ map (^. expr) asExtrs)
extractPrimF (PConcatF (runExtraction -> a) (runExtraction -> b)) = ExtractionFun $ \zone -> do
  aExtr <- a zone
  bExtr <- b zone
  return $ E (aExtr ^. statements ++ bExtr ^. statements) (Binary (aExtr ^. expr) Add (bExtr ^. expr))

extractExprMonadF ::
  MonadThrowWithStack m =>
  ExprMonadF (ExtractionFun m) a ->
  ExtractionFun m a
extractExprMonadF (EParF (runExtraction -> a) (runExtraction -> b)) = ExtractionFun $ \zone -> do
  aExtr <- a zone
  bExtr <- b zone
  return $ E (aExtr ^. statements ++ bExtr ^. statements) (Tuple [aExtr^.expr, bExtr^.expr])
extractExprMonadF (ELaplaceF w (runExtraction -> a)) = ExtractionFun $ \zone -> do
  aExtr <- a zone
  case zone of
    Other -> do
      return $ E (aExtr ^. statements) (CallBuiltin "laplace" [Val (D w), aExtr ^. expr])
    Orange -> do
      return $ E (aExtr ^. statements) (CallBuiltin "laplace_fx" [lit2Mamba (D w), aExtr ^. expr])
extractExprMonadF (EReturnF (runExtraction -> a)) = ExtractionFun $ \zone -> do
  aExtr <- a zone
  return aExtr
extractExprMonadF (EBindF (runExtraction -> m) (Syn.Var bound) (runExtraction -> k)) = ExtractionFun $ \zone -> do
  mExtr <- m zone
  kExtr <- k zone
  let stmts = mExtr ^. statements
              ++ [Assign bound (mExtr ^. expr)]
              ++ kExtr ^. statements
  return $ E stmts (kExtr ^. expr)

extractBmcsF ::
  MonadThrowWithStack m =>
  BmcsF (ExtractionFun m) a ->
  ExtractionFun m a
extractBmcsF (BRunF reprSize (Vec clip) (runExtraction -> mstate) (runExtraction -> mf) (runExtraction -> rstate) (runExtraction -> rf)) = ExtractionFun $ \_ -> do
  mstateExtr <- mstate Other
  mfExtr     <- mf Other
  rstateExtr <- rstate Other
  rfExtr     <- rf Orange
  let stmts = mstateExtr ^. statements
              ++ mfExtr ^. statements
              ++ rstateExtr ^. statements
              ++ rfExtr ^. statements
  return $ E stmts (CallBuiltin "bmcs" [Val (I reprSize),
                                        Extraction.List $ map (Val . D) clip,
                                        mstateExtr ^. expr,
                                        mfExtr ^. expr,
                                        rstateExtr ^. expr,
                                        rfExtr ^. expr])

extract' :: (MonadThrowWithStack m, FreshM m) => HFix NBmcsF a -> ExtractionFun m a
extract' = hcata' (extractBmcsF
                   `sumAlg` extractExprMonadF
                   `sumAlg` extractExprF
                   `sumAlg` extractControlF
                   `sumAlg` extractPrimF)

extractM :: (MonadThrowWithStack m, FreshM m) => HFix NBmcsF a -> m Extraction
extractM prog = runExtraction (extract' prog) Other

compileAndExtract ::
  (Typeable row, Typeable a) =>
  String ->
  (forall f. HXFix CPSFuzzF f (Bag row) -> HXFix CPSFuzzF f (Distr a)) ->
  Either SomeException Extraction
compileAndExtract db prog = flip evalStateT (nameState [UniqueName db 0]) $ do
  bmcsProg <- compileM db prog
  extractM bmcsProg
