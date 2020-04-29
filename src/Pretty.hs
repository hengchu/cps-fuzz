module Pretty where

import Control.Monad.State
import Lib
import Names
import Text.PrettyPrint.ANSI.Leijen
import Type.Reflection

-- | The pretty-printing monad.
newtype P a = P {runP_ :: State NameState a}
  deriving
    (Functor, Applicative, Monad, MonadState NameState)
    via (State NameState)

associativityTable :: String -> Int
associativityTable "App" = 1
associativityTable "." = -1
associativityTable "+" = 1
associativityTable "-" = 1
associativityTable "*" = 1
associativityTable "/" = 1
associativityTable "&&" = -1
associativityTable "||" = -1
associativityTable "==" = 0
associativityTable "/=" = 0
associativityTable "<" = 0
associativityTable "<=" = 0
associativityTable ">" = 0
associativityTable ">=" = 0
associativityTable x = error $ "associativityTable: unknown symbol " ++ x

precedenceTable :: String -> Int
precedenceTable "App" = 900
precedenceTable "." = 800
precedenceTable "*" = 700
precedenceTable "/" = 700
precedenceTable "+" = 600
precedenceTable "-" = 600
precedenceTable "==" = 400
precedenceTable "/=" = 400
precedenceTable "<" = 400
precedenceTable "<=" = 400
precedenceTable ">" = 400
precedenceTable ">=" = 400
precedenceTable "&&" = 300
precedenceTable "||" = 200
precedenceTable x = error $ "precedenceTable: unknown symbol " ++ x

runP :: P a -> a
runP = flip evalState emptyNameState . runP_

prettyExpr :: forall a. Int -> Expr a -> P Doc
prettyExpr _ (EVar x) = return $ text x
prettyExpr _ (ELam (f :: Expr arg -> Expr ret)) = do
  lpush
  x <- lfresh "x"
  body <- prettyExpr 0 (f (EVar x))
  lpop
  return
    $ parens
    $ text "\\"
      <> (parens $ text x <+> text "::" <+> text (show $ typeRep @arg))
      <+> text "->"
      <+> body
prettyExpr p (EApp f a) = do
  f' <- prettyExpr (precedenceTable "App") f
  a' <- prettyExpr (p + associativityTable "App") a
  return $ parensIf (p > precedenceTable "App") (f' <+> a')
prettyExpr p (EComp g f) = do
  g' <- prettyExpr (precedenceTable ".") g
  f' <- prettyExpr (precedenceTable "." + associativityTable ".") f
  return $ parensIf (p > precedenceTable ".") (g' </> dot <+> f')
prettyExpr p (EIf c a b) = do
  c' <- prettyExpr (precedenceTable "App") c
  a' <- prettyExpr (precedenceTable "App" + associativityTable "App") a
  b' <- prettyExpr (precedenceTable "App" + associativityTable "App" * 2) b
  return $
    parensIf
      (p > precedenceTable "App")
      (text "if" <+> c' <+> a' <+> b')
prettyExpr _ (EIntLit x) = return $ int x
prettyExpr _ (ENumLit x) = return $ double x
prettyExpr p (EAdd a b) = prettyBinop p "+" prettyExpr a b
prettyExpr p (EMinus a b) = prettyBinop p "-" prettyExpr a b
prettyExpr p (EMult a b) = prettyBinop p "*" prettyExpr a b
prettyExpr p (EDiv a b) = prettyBinop p "/" prettyExpr a b
prettyExpr p (EAbs a) = do
  a' <- prettyExpr (precedenceTable "App") a
  return
    $ parensIf (p > precedenceTable "App")
    $ text "abs" <+> a'
prettyExpr p (EGT a b) = prettyBinop p ">" prettyExpr a b
prettyExpr p (EGE a b) = prettyBinop p ">=" prettyExpr a b
prettyExpr p (ELT a b) = prettyBinop p "<" prettyExpr a b
prettyExpr p (ELE a b) = prettyBinop p "<=" prettyExpr a b
prettyExpr p (EEQ a b) = prettyBinop p "==" prettyExpr a b
prettyExpr p (ENEQ a b) = prettyBinop p "/=" prettyExpr a b

prettyBinop :: forall term. Int -> String -> (Int -> term -> P Doc) -> term -> term -> P Doc
prettyBinop p op printer a b = do
  a' <- printer (precedenceTable op) a
  b' <- printer (precedenceTable op + associativityTable op) b
  return
    $ parensIf (p > precedenceTable op)
    $ a' <+> text op <+> b'

prettyCPSFuzz :: Int -> CPSFuzz a -> P Doc
prettyCPSFuzz _ (CVar x) = return $ text x
prettyCPSFuzz _ (CNumLit x) = return $ double x
prettyCPSFuzz p (CAdd a b) = do
  prettyBinop p "+" prettyCPSFuzz a b
prettyCPSFuzz p (CMinus a b) = do
  prettyBinop p "-" prettyCPSFuzz a b
prettyCPSFuzz p (CMult a b) = do
  prettyBinop p "*" prettyCPSFuzz a b
prettyCPSFuzz p (CDiv a b) = do
  prettyBinop p "/" prettyCPSFuzz a b
prettyCPSFuzz p (CAbs a) = do
  a' <- prettyCPSFuzz (precedenceTable "App") a
  return
    $ parensIf (p > precedenceTable "App")
    $ text "abs" <+> a'
prettyCPSFuzz p (CGT a b) =
  prettyBinop p ">" prettyCPSFuzz a b
prettyCPSFuzz p (CGE a b) =
  prettyBinop p ">=" prettyCPSFuzz a b
prettyCPSFuzz p (CLT a b) =
  prettyBinop p "<" prettyCPSFuzz a b
prettyCPSFuzz p (CLE a b) =
  prettyBinop p "<=" prettyCPSFuzz a b
prettyCPSFuzz p (CEQ a b) =
  prettyBinop p "==" prettyCPSFuzz a b
prettyCPSFuzz p (CNEQ a b) =
  prettyBinop p "/=" prettyCPSFuzz a b
prettyCPSFuzz p (BMap f db k) = do
  f' <- prettyExpr (precedenceTable "App") f
  db' <- prettyCPSFuzz (precedenceTable "App" + associativityTable "App") db
  lpush
  dbName <- lfresh "db"
  k' <- prettyCPSKont dbName k
  lpop
  return
    $ parensIf (p > precedenceTable "App")
    $ text "bmap" <+> f' <+> db' <+> k'
{-
prettyCPSFuzz p (BFilter f db k) = do
  f' <- prettyExpr (precedenceTable "App") f
  db' <- prettyCPSFuzz (precedenceTable "App" + associativityTable "App") db
  lpush
  dbName <- lfresh "db"
  k' <- prettyCPSKont dbName k
  lpop
  return
    $ parensIf (p > precedenceTable "App")
    $ text "bfilter" <+> f' <+> db' <+> k'
-}
prettyCPSFuzz p (BSum clip db k) = do
  db' <- prettyCPSFuzz (precedenceTable "App" + associativityTable "App") db
  lpush
  dbName <- lfresh "sum"
  k' <- prettyCPSKont dbName k
  lpop
  return
    $ parensIf (p > precedenceTable "App")
    $ text "bsum" <+> text (show clip) <+> db' <+> k'

prettyCPSKont :: Typeable a => String -> CPSKont a b -> P Doc
prettyCPSKont freshArgName k = do
  body' <- prettyCPSFuzz (precedenceTable "App") (k (CVar freshArgName))
  return $ parens $ text "\\" <> text freshArgName <+> text "->" <+> body'

prettyBMCS :: Int -> BMCS a -> P Doc
--prettyBMCS _ (BVar x) = return $ text x
--prettyBMCS _ (BNumLit x) = return $ double x
prettyBMCS p (Run reprSize bound mf rf) = do
  mf' <- prettyExpr (precedenceTable "App" + associativityTable "App" * 2) mf
  rf' <- prettyExpr (precedenceTable "App" + associativityTable "App" * 3) rf
  return
    $ parensIf (p > precedenceTable "App")
    $ text "bmcs" <+> int reprSize <> (hardline <> (red mf')) <> (hardline <> (dullyellow rf'))

parensIf :: Bool -> Doc -> Doc
parensIf cond = if cond then parens else id

instance FreshM P where
  getNameState = get
  modifyNameState = modify
