module Pretty where

import Lib
import Names

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
precedenceTable "App" = 90
precedenceTable "."   = 90
precedenceTable "*"   = 70
precedenceTable "/"   = 70
precedenceTable "+"   = 60
precedenceTable "-"   = 60
precedenceTable "=="  = 40
precedenceTable "/="  = 40
precedenceTable "<"   = 40
precedenceTable "<="  = 40
precedenceTable ">"   = 40
precedenceTable ">="  = 40
precedenceTable "&&"  = 30
precedenceTable "||"  = 20
precedenceTable x     = error $ "precedenceTable: unknown symbol " ++ x

runP :: P a -> a
runP = flip evalState emptyNameState . runP_

prettyExpr :: Int -> Expr a -> P Doc
prettyExpr _ (EVar x) = return $ text x
prettyExpr _ (ELam f) = do
  lpush
  x <- lfresh "x"
  body <- prettyExpr 0 (f (EVar x))
  return $ parens $ text "\\" <> text x <> dot <+> body
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

prettyCPSFuzz :: Int -> CPSFuzz a -> P Doc
prettyCPSFuzz = undefined

prettyBMCS :: Int -> BMCS a -> P Doc
prettyBMCS = undefined

parensIf :: Bool -> Doc -> Doc
parensIf cond = if cond then parens else id

instance FreshM P where
  getNameState = get
  modifyNameState = modify
