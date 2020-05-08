{-# LANGUAGE AllowAmbiguousTypes #-}

module Pretty where

import Control.Monad.State.Strict
import Syntax
import Text.PrettyPrint.ANSI.Leijen
import Type.Reflection
import HFunctor

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

newtype P a = P { runPretty :: Int -> Doc }

parensIf :: Bool -> Doc -> Doc
parensIf True = parens
parensIf False = id

pBagOpF :: BagOpF P a -> P a
pBagOpF (BMapF mf inputDb kont) = P $ \prec ->
  let mfDoc = runPretty mf (precedenceTable "App")
      inputDbDoc = runPretty inputDb (precedenceTable "App" + associativityTable "App")
      kontDoc = runPretty kont 0
  in parensIf (prec > precedenceTable "App") $
  string "bmap" <+> mfDoc <+> inputDbDoc <+> string "$" <$$> nest 2 kontDoc
pBagOpF (BSumF clip inputDb kont) = P $ \prec ->
  let clipDoc = parens . text $ show clip
      inputDbDoc = runPretty inputDb (precedenceTable "App" + associativityTable "App")
      kontDoc = runPretty kont 0
  in parensIf (prec > precedenceTable "App") $
  string "bsum" <+> clipDoc <+> inputDbDoc <+> string "$" <$$> nest 2 kontDoc

pExprMonadF :: ExprMonadF P a -> P a
pExprMonadF (ELaplaceF w c) = P $ \prec ->
  let wDoc = double w
      cDoc = runPretty c (precedenceTable "App" + associativityTable "App")
  in string "lap" <+> wDoc <+> cDoc
pExprMonadF (EBindF m (Var bound) f) = P $ \prec ->
  let mDoc = runPretty m 0
      fDoc = runPretty f 0
  in text bound <+> string "<-" <+> mDoc <$$> fDoc
pExprMonadF (EReturnF m) = P $ \prec ->
  let mDoc = runPretty m (precedenceTable "App")
  in string "return" <+> mDoc

showTypeRep :: forall a. Typeable a => Doc
showTypeRep = text . show $ (typeRep @a)

pExprF :: ExprF P a -> P a
pExprF (EVarF (Var x)) = P $ const (text x)
pExprF (ELamF (Var bound :: _ t) body) = P $ \prec ->
  let bodyDoc = runPretty body 0
  in parens $
     string "\\" <> (parens $ text bound <+> string "::" <+> showTypeRep @t)
     <+> string "->"
     </> bodyDoc
pExprF (EAppF f arg) = P $ \prec ->
  let fDoc = runPretty f (precedenceTable "App")
      argDoc = runPretty arg (precedenceTable "App" + associativityTable "App")
  in parensIf (prec > precedenceTable "App") $
     fDoc <+> argDoc
pExprF (ECompF f g) = P $ \prec ->
  let fDoc = runPretty f (precedenceTable ".")
      gDoc = runPretty g (precedenceTable "." + associativityTable ".")
  in parensIf (prec > precedenceTable ".") $
     fDoc <+> string "." <+> gDoc

pControlF :: ControlF P a -> P a
pControlF (CIfF cond a b) = P $ \prec ->
  let condDoc = runPretty cond (precedenceTable "App")
      aDoc = runPretty a (precedenceTable "App")
      bDoc = runPretty b (precedenceTable "App")
  in parensIf (prec > precedenceTable "App") $
     string "if" <+> condDoc
     </> string "then" <+> aDoc
     </> string "else" <+> bDoc

pPrimF :: PrimF P a -> P a
pPrimF (PLitF x) = P $ const . parens . text . show $ x
pPrimF (PAddF a b) = P $ \prec ->
  let aDoc = runPretty a (precedenceTable "+")
      bDoc = runPretty b (precedenceTable "+" + associativityTable "+")
  in parensIf (prec > precedenceTable "+") $
     aDoc <+> string "+" <+> bDoc
pPrimF (PSubF a b) = P $ \prec ->
  let aDoc = runPretty a (precedenceTable "-")
      bDoc = runPretty b (precedenceTable "-" + associativityTable "-")
  in parensIf (prec > precedenceTable "-") $
     aDoc <+> string "-" <+> bDoc
pPrimF (PMultF a b) = P $ \prec ->
  let aDoc = runPretty a (precedenceTable "*")
      bDoc = runPretty b (precedenceTable "*" + associativityTable "*")
  in parensIf (prec > precedenceTable "*") $
     aDoc <+> string "*" <+> bDoc
pPrimF (PDivF a b) = P $ \prec ->
  let aDoc = runPretty a (precedenceTable "/")
      bDoc = runPretty b (precedenceTable "/" + associativityTable "/")
  in parensIf (prec > precedenceTable "/") $
     aDoc <+> string "/" <+> bDoc
pPrimF (PAbsF a) = P $ \prec ->
  let aDoc = runPretty a (precedenceTable "App")
  in parensIf (prec > precedenceTable "App") $
     string "abs" <+> aDoc
pPrimF (PSignumF a) = P $ \prec ->
  let aDoc = runPretty a (precedenceTable "App")
  in parensIf (prec > precedenceTable "App") $
     string "signum" <+> aDoc
pPrimF (PExpF a) = P $ \prec ->
  let aDoc = runPretty a (precedenceTable "App")
  in parensIf (prec > precedenceTable "App") $
     string "exp" <+> aDoc
pPrimF (PSqrtF a) = P $ \prec ->
  let aDoc = runPretty a (precedenceTable "App")
  in parensIf (prec > precedenceTable "App") $
     string "sqrt" <+> aDoc
pPrimF (PLogF a) = P $ \prec ->
  let aDoc = runPretty a (precedenceTable "App")
  in parensIf (prec > precedenceTable "App") $
     string "log" <+> aDoc
pPrimF (PGTF a b) = P $ \prec ->
  let aDoc = runPretty a (precedenceTable ">")
      bDoc = runPretty b (precedenceTable ">" + associativityTable ">")
  in parensIf (prec > precedenceTable ">") $
     aDoc <+> string ">" <+> bDoc
pPrimF (PGEF a b) = P $ \prec ->
  let aDoc = runPretty a (precedenceTable ">=")
      bDoc = runPretty b (precedenceTable ">=" + associativityTable ">=")
  in parensIf (prec > precedenceTable ">=") $
     aDoc <+> string ">=" <+> bDoc
pPrimF (PLTF a b) = P $ \prec ->
  let aDoc = runPretty a (precedenceTable "<")
      bDoc = runPretty b (precedenceTable "<" + associativityTable "<")
  in parensIf (prec > precedenceTable "<") $
     aDoc <+> string "<" <+> bDoc
pPrimF (PLEF a b) = P $ \prec ->
  let aDoc = runPretty a (precedenceTable "<=")
      bDoc = runPretty b (precedenceTable "<=" + associativityTable "<=")
  in parensIf (prec > precedenceTable "<=") $
     aDoc <+> string "<=" <+> bDoc
pPrimF (PEQF a b) = P $ \prec ->
  let aDoc = runPretty a (precedenceTable "==")
      bDoc = runPretty b (precedenceTable "==" + associativityTable "==")
  in parensIf (prec > precedenceTable "==") $
     aDoc <+> string "==" <+> bDoc
pPrimF (PNEQF a b) = P $ \prec ->
  let aDoc = runPretty a (precedenceTable "/=")
      bDoc = runPretty b (precedenceTable "/=" + associativityTable "/=")
  in parensIf (prec > precedenceTable "/=") $
     aDoc <+> string "/=" <+> bDoc
pPrimF (PJustF a) = P $ \prec ->
  let aDoc = runPretty a (precedenceTable "App")
  in parensIf (prec > precedenceTable "App") $
     string "Just" <+> aDoc
pPrimF PNothingF = P $ const $ string "Nothing"
pPrimF (PFromJustF a) = P $ \prec ->
  let aDoc = runPretty a (precedenceTable "App")
  in parensIf (prec > precedenceTable "App") $
     string "fromJust" <+> aDoc
pPrimF (PIsJustF a) = P $ \prec ->
  let aDoc = runPretty a (precedenceTable "App")
  in parensIf (prec > precedenceTable "App") $
     string "isJust" <+> aDoc
pPrimF (PPairF a b) = P $ \prec ->
  let aDoc = runPretty a 0
      bDoc = runPretty b 0
  in parens $ aDoc <> comma <+> bDoc
pPrimF (PFstF a) = P $ \prec ->
  let aDoc = runPretty a (precedenceTable "App")
  in parensIf (prec > precedenceTable "App") $
     string "fst" <+> aDoc
pPrimF (PSndF a) = P $ \prec ->
  let aDoc = runPretty a (precedenceTable "App")
  in parensIf (prec > precedenceTable "App") $
     string "snd" <+> aDoc

pMainF :: MainF P a -> P a
pMainF =
  pExprMonadF
  `sumAlg` pExprF
  `sumAlg` pControlF
  `sumAlg` pPrimF

pNCPSFuzzF :: NCPSFuzzF P a -> P a
pNCPSFuzzF = pBagOpF `sumAlg` pMainF

pNCPSFuzz :: HFix NCPSFuzzF a -> Doc
pNCPSFuzz = flip runPretty 0 . (hcata' pNCPSFuzzF)
