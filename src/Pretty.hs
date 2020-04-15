module Pretty where

import Lib
import Names

import Type.Reflection
import Control.Monad.State
import Text.PrettyPrint.ANSI.Leijen

-- |The pretty-printing monad.
newtype P a = P { runP_ :: State NameState a }
  deriving (Functor, Applicative, Monad, MonadState NameState)
  via (State NameState)

associativityTable :: String -> Int
associativityTable "App" = 1
associativityTable "."   = -1
associativityTable "+"   = 1
associativityTable "-"   = 1
associativityTable "*"   = 1
associativityTable "/"   = 1
associativityTable "&&"  = -1
associativityTable "||"  = -1
associativityTable "=="  = 0
associativityTable "/="  = 0
associativityTable "<"   = 0
associativityTable "<="  = 0
associativityTable ">"   = 0
associativityTable ">="  = 0
associativityTable x     = error $ "associativityTable: unknown symbol " ++ x


precedenceTable :: String -> Int
precedenceTable "App" = 900
precedenceTable "."   = 900
precedenceTable "*"   = 700
precedenceTable "/"   = 700
precedenceTable "+"   = 600
precedenceTable "-"   = 600
precedenceTable "=="  = 400
precedenceTable "/="  = 400
precedenceTable "<"   = 400
precedenceTable "<="  = 400
precedenceTable ">"   = 400
precedenceTable ">="  = 400
precedenceTable "&&"  = 300
precedenceTable "||"  = 200
precedenceTable x     = error $ "precedenceTable: unknown symbol " ++ x

runP :: P a -> a
runP = flip evalState emptyNameState . runP_

prettyExpr :: forall a. Int -> Expr a -> P Doc
prettyExpr _ (EVar x) = return $ text x
prettyExpr _ (ELam (f :: Expr arg -> Expr ret)) = do
  lpush
  x <- lfresh "x"
  body <- prettyExpr 0 (f (EVar x))
  lpop
  return $
    parens $
    text "\\" <>
    (parens $ text x <+> text "::" <+> text (show $ typeRep @arg)) <>
    dot <+>
    body
prettyExpr p (EApp f a) = do
  f' <- prettyExpr (precedenceTable "App") f
  a' <- prettyExpr (p+associativityTable "App") a
  return $ parensIf (p > precedenceTable "App") (f' <+> a')
prettyExpr p (EComp g f) = do
  g' <- prettyExpr (precedenceTable ".") g
  f' <- prettyExpr (p+associativityTable ".") f
  return $ parensIf (p > precedenceTable ".") (g' <+> dot <+> f')
prettyExpr p (EIf c a b) = do
  c' <- prettyExpr (precedenceTable "App") c
  a' <- prettyExpr (precedenceTable "App") a
  b' <- prettyExpr (precedenceTable "App") b
  return $ parensIf (p > precedenceTable "App")
    (text "if" <+> c' <+> (softbreak <> a') <+> (softbreak <> b'))
prettyExpr _ (EIntLit x) = return $ int x
prettyExpr _ (ENumLit x) = return $ double x
prettyExpr p (EBoolToNum a) = do
  a' <- prettyExpr (precedenceTable "App") a
  return $ parensIf (p > precedenceTable "App")
    (text "b2n" <+> a')
prettyExpr p (EFocus start end) = do
  start' <- prettyExpr (precedenceTable "App") start
  end'   <- prettyExpr (precedenceTable "App" + associativityTable "App") end
  return $ parensIf (p > precedenceTable "App") $ text "focus" <+> start' <+> end'
prettyExpr _ EVecSum =
  return $ text "vec_sum"
prettyExpr p (EVecExtend w) = do
  w' <- prettyExpr (precedenceTable "App") w
  return $ parensIf (p > precedenceTable "App") $ text "vec_extend" <+> w'
prettyExpr p (EVecStore rStart rEnd wStart wEnd f) = do
  rStart' <- prettyExpr (precedenceTable "App") rStart
  rEnd'   <- prettyExpr (precedenceTable "App"+associativityTable "App") rEnd
  wStart' <- prettyExpr (precedenceTable "App"+associativityTable "App"*2) wStart
  wEnd'   <- prettyExpr (precedenceTable "App"+associativityTable "App"*3) wEnd
  f'      <- prettyExpr (precedenceTable "App"+associativityTable "App"*4) f
  return $
    parensIf (p > precedenceTable "App") $
    text "vec_store" <+> rStart' <+> rEnd' <+> wStart' <+> wEnd' <+> f'
prettyExpr p (EVecZeros w) = do
  w' <- prettyExpr (precedenceTable "App") w
  return $
    parensIf (p > precedenceTable "App") $
    text "vec_zeros" <+> w'
prettyExpr p (EAsVec (a :: Expr arg)) = do
  a' <- prettyExpr (precedenceTable "App") a
  return $
    parensIf (p > precedenceTable "App") $
    text "as_vec" <+> text "@" <> (text $ show (typeRep @arg)) <+> a'
prettyExpr p (EFromVec a) = do
  a' <- prettyExpr (precedenceTable "App") a
  return $
    parensIf (p > precedenceTable "App") $
    text "from_vec" <+> text "@" <> (text $ show (typeRep @a)) <+> a'
prettyExpr p (EAdd   a b) = prettyBinop p "+" a b
prettyExpr p (EMinus a b) = prettyBinop p "-" a b
prettyExpr p (EMult  a b) = prettyBinop p "*" a b
prettyExpr p (EDiv   a b) = prettyBinop p "/" a b
prettyExpr p (EAbs a) = do
  a' <- prettyExpr (precedenceTable "App") a
  return $
    parensIf (p > precedenceTable "App") $
    text "abs" <+> a'
prettyExpr p (EGT a b)  = prettyBinop p ">"  a b
prettyExpr p (EGE a b)  = prettyBinop p ">=" a b
prettyExpr p (ELT a b)  = prettyBinop p "<"  a b
prettyExpr p (ELE a b)  = prettyBinop p "<=" a b
prettyExpr p (EEQ a b)  = prettyBinop p "==" a b
prettyExpr p (ENEQ a b) = prettyBinop p "/=" a b

prettyBinop :: forall a. Int -> String -> Expr a -> Expr a -> P Doc
prettyBinop p op a b = do
  a' <- prettyExpr (precedenceTable op) a
  b' <- prettyExpr (precedenceTable op+associativityTable op) b
  return $
    parensIf (p > precedenceTable op) $
    a' <+> text op <+> b'

prettyCPSFuzz :: Int -> CPSFuzz a -> P Doc
prettyCPSFuzz = undefined

prettyBMCS :: Int -> BMCS a -> P Doc
prettyBMCS = undefined

parensIf :: Bool -> Doc -> Doc
parensIf cond = if cond then parens else id

instance FreshM P where
  getNameState = get
  modifyNameState = modify
