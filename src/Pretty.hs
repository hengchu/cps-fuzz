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
  x <- gfresh "x"
  body <- prettyExpr 0 (f (EVar x))
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
prettyExpr p (EJust a) = do
  a' <- prettyExpr (precedenceTable "App") a
  return $ parensIf (p > precedenceTable "App") (text "Just" <+> a')
prettyExpr _ ENothing = return $ text "Nothing"
prettyExpr p (EIsJust a) = do
  a' <- prettyExpr (precedenceTable "App") a
  return $ parensIf (p > precedenceTable "App") (text "isJust" <+> a')
prettyExpr p (EFromJust a) = do
  a' <- prettyExpr (precedenceTable "App" + associativityTable "App") a
  return $ parensIf (p > precedenceTable "App") (text "fromJust" <+> a')
prettyExpr _ (EPair a b) = do
  a' <- prettyExpr 0 a
  b' <- prettyExpr 0 b
  return $ parens (a' <> comma <+> b')
prettyExpr p (EFst a) = do
  a' <- prettyExpr (precedenceTable "App") a
  return $ parens {-parensIf (p > precedenceTable "App")-} (text "fst" <+> a')
prettyExpr p (ESnd a) = do
  a' <- prettyExpr (precedenceTable "App") a
  return $ parens {-parensIf (p > precedenceTable "App")-} (text "snd" <+> a')
prettyExpr _ (EShare a f) = do
  x <- gfresh "x"
  f' <- prettyExpr 0 (f (EVar x))
  {-
    let x = a in f
  -}
  a' <- prettyExpr 0 a
  let letIn = text "let" <+> text x <+> text "=" <+> a' <+> text "in" <+> f'
  return letIn
prettyExpr p (ELap c w) = do
  w' <- prettyExpr (precedenceTable "App" + associativityTable "App") w
  return $ parensIf (p > precedenceTable "App") (text "lap" <+> double c <+> w')
prettyExpr p (EReturn w) = do
  w' <- prettyExpr (precedenceTable "App" + associativityTable "App") w
  return $ parensIf (p > precedenceTable "App") (text "return" <+> w')
prettyExpr _ (EBind m f) = do
  m' <- prettyExpr 0 m
  x <- gfresh "x"
  f' <- prettyExpr 0 (f (EVar x))
  return $ vcat [text x <+> text "<-" <+> m', f']

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
  dbName <- gfresh "db"
  k' <- prettyCPSKont dbName k
  return
    $ parensIf (p > precedenceTable "App")
    $ text "bmap" <+> f' <+> db' <+> k'
prettyCPSFuzz p (BSum clip db k) = do
  db' <- prettyCPSFuzz (precedenceTable "App" + associativityTable "App") db
  dbName <- gfresh "sum"
  k' <- prettyCPSKont dbName k
  return
    $ parensIf (p > precedenceTable "App")
    $ text "bsum" <+> text (show clip) <+> db' <+> k'
prettyCPSFuzz _ (CShare a f) = do
  x <- gfresh "x"
  f' <- prettyCPSFuzz 0 (f (CVar x))
  {-
    let x = a in f
  -}
  a' <- prettyCPSFuzz 0 a
  let letIn = text "let" <+> text x <+> text "=" <+> a' <+> text "in" <+> f'
  return letIn
prettyCPSFuzz p (CLap c w) = do
  w' <- prettyCPSFuzz (precedenceTable "App" + associativityTable "App") w
  return $ parensIf (p > precedenceTable "App") (text "lap" <+> double c <+> w')
prettyCPSFuzz p (CReturn w) = do
  w' <- prettyCPSFuzz (precedenceTable "App" + associativityTable "App") w
  return $ parensIf (p > precedenceTable "App") (text "return" <+> w')
prettyCPSFuzz _ (CBind m f) = do
  m' <- prettyCPSFuzz 0 m
  x <- gfresh "x"
  f' <- prettyCPSFuzz 0 (f (CVar x))
  return $ vcat [text x <+> text "<-" <+> m', f']

prettyCPSKont :: Typeable a => String -> CPSKont a b -> P Doc
prettyCPSKont freshArgName k = do
  body' <- prettyCPSFuzz (precedenceTable "App") (k (CVar freshArgName))
  return $ parens $ text "\\" <> text freshArgName <+> text "->" <+> body'

prettyBMCS :: Int -> BMCS a -> P Doc
prettyBMCS _ (BVar x) = return $ text x
prettyBMCS _ (BNumLit x) = return $ double x
prettyBMCS p (BReturn w) = do
  w' <- prettyBMCS (precedenceTable "App" + associativityTable "App") w
  return $ parensIf (p > precedenceTable "App") (text "return" <+> w')
prettyBMCS _ (BBind m f) = do
  m' <- prettyBMCS 0 m
  x <- gfresh "x"
  f' <- prettyBMCS 0 (f (BVar x))
  return $ vcat [text x <+> text "<-" <+> m', f']
prettyBMCS p (Run reprSize bound mf rf) = do
  mf' <- prettyExpr (precedenceTable "App" + associativityTable "App" * 2) mf
  rf' <- prettyExpr (precedenceTable "App" + associativityTable "App" * 3) rf
  return
    $ parensIf (p > precedenceTable "App")
    $ vcat [text "bmcs" <+> int reprSize <+> text (show bound),
            indent 2 (red mf'),
            indent 2 (dullyellow rf')]
prettyBMCS p (BAdd a b) = do
  prettyBinop p "+" prettyBMCS a b
prettyBMCS p (BMinus a b) = do
  prettyBinop p "-" prettyBMCS a b
prettyBMCS p (BMult a b) = do
  prettyBinop p "*" prettyBMCS a b
prettyBMCS p (BDiv a b) = do
  prettyBinop p "/" prettyBMCS a b
prettyBMCS p (BAbs a) = do
  a' <- prettyBMCS (precedenceTable "App") a
  return
    $ parensIf (p > precedenceTable "App")
    $ text "abs" <+> a'
prettyBMCS p (BGT a b) =
  prettyBinop p ">" prettyBMCS a b
prettyBMCS p (BGE a b) =
  prettyBinop p ">=" prettyBMCS a b
prettyBMCS p (BLT a b) =
  prettyBinop p "<" prettyBMCS a b
prettyBMCS p (BLE a b) =
  prettyBinop p "<=" prettyBMCS a b
prettyBMCS p (BEQ a b) =
  prettyBinop p "==" prettyBMCS a b
prettyBMCS p (BNEQ a b) =
  prettyBinop p "/=" prettyBMCS a b
prettyBMCS p (BLap c w) = do
  w' <- prettyBMCS (precedenceTable "App" + associativityTable "App") w
  return $ parensIf (p > precedenceTable "App") (text "lap" <+> double c <+> w')


parensIf :: Bool -> Doc -> Doc
parensIf cond = if cond then parens else id

instance FreshM P where
  getNameState = get
  modifyNameState = modify
