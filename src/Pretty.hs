{-# LANGUAGE AllowAmbiguousTypes #-}

module Pretty where

import Control.Monad.State.Strict
import HFunctor
import Syntax hiding (Literal(..))
import Text.PrettyPrint.ANSI.Leijen
import Type.Reflection
import Prelude hiding ((<$>))

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

newtype P a = P {runPretty :: Int -> Doc}

parensIf :: Bool -> Doc -> Doc
parensIf True = parens
parensIf False = id

pBagOpF :: BagOpF P a -> P a
pBagOpF (BMapF mf inputDb kont) = P $ \prec ->
  let mfDoc = runPretty mf (precedenceTable "App")
      inputDbDoc = runPretty inputDb (precedenceTable "App" + associativityTable "App")
      kontDoc = runPretty kont 0
   in parensIf (prec >= precedenceTable "App") $
        string "bmap" <+> mfDoc <+> inputDbDoc <+> string "$" <$$> nest 2 kontDoc
pBagOpF (BSumF clip inputDb kont) = P $ \prec ->
  let clipDoc = parens . text $ show clip
      inputDbDoc = runPretty inputDb (precedenceTable "App" + associativityTable "App")
      kontDoc = runPretty kont 0
   in parensIf (prec >= precedenceTable "App") $
        string "bsum" <+> clipDoc <+> inputDbDoc <+> string "$" <$$> nest 2 kontDoc

pFlatBagOpF :: FlatBagOpF P a -> P a
pFlatBagOpF (FBMapF mf (Var inputDb) kont) = P $ \prec ->
  let mfDoc = runPretty mf (precedenceTable "App")
      inputDbDoc = string . show $ inputDb
      kontDoc = runPretty kont 0
   in parensIf (prec >= precedenceTable "App") $
        string "bmap" <+> mfDoc <+> inputDbDoc <+> string "$" <$$> nest 2 kontDoc
pFlatBagOpF (FBSumF clip (Var inputDb) kont) = P $ \prec ->
  let clipDoc = parens . text $ show clip
      inputDbDoc = string . show $ inputDb
      kontDoc = runPretty kont 0
   in parensIf (prec >= precedenceTable "App") $
        string "bsum" <+> clipDoc <+> inputDbDoc <+> string "$" <$$> nest 2 kontDoc

pExprMonadF :: ExprMonadF P a -> P a
pExprMonadF (EParF a b) = P $ \prec ->
  let aDoc = runPretty a (precedenceTable "App")
      bDoc = runPretty b (precedenceTable "App" + associativityTable "App")
   in parensIf (prec >= precedenceTable "App") $ string "par" <+> aDoc <+> bDoc
pExprMonadF (ELaplaceF w c) = P $ \prec ->
  let wDoc = double w
      cDoc = runPretty c (precedenceTable "App" + associativityTable "App")
   in string "lap" <+> wDoc <+> cDoc
pExprMonadF (EBindF m (Var bound) f) = P $ \prec ->
  let mDoc = runPretty m 0
      fDoc = runPretty f 0
   in (string (show bound)) <+> string "<-" <+> mDoc <$$> fDoc
pExprMonadF (EReturnF m) = P $ \prec ->
  let mDoc = runPretty m (precedenceTable "App")
   in string "return" <+> mDoc

showTypeRep :: forall a. Typeable a => Doc
showTypeRep = text . show $ (typeRep @a)

pExprF :: ExprF P a -> P a
pExprF (EVarF (Var x)) = P $ const (string (show x))
pExprF (ELamF (Var bound :: _ t) body) = P $ \prec ->
  let bodyDoc = runPretty body 0
      flatDoc =
        string "\\" <> (parens $ (string (show bound)) <+> string "::" <+> showTypeRep @t)
          <+> string "->"
          <+> bodyDoc
      multilineDoc =
        string "\\" <> (parens $ (string (show bound)) <+> string "::" <+> showTypeRep @t)
          <+> string "->"
          <$$> nest 2 bodyDoc
   in parens $
        flatAlt flatDoc multilineDoc
pExprF (EAppF f arg) = P $ \prec ->
  let fDoc = runPretty f (precedenceTable "App")
      argDoc = runPretty arg (precedenceTable "App" + associativityTable "App")
   in parensIf (prec >= precedenceTable "App") $
        fDoc <+> argDoc
pExprF (ECompF f g) = P $ \prec ->
  let fDoc = runPretty f (precedenceTable ".")
      gDoc = runPretty g (precedenceTable "." + associativityTable ".")
   in parensIf (prec >= precedenceTable ".") $
        fDoc <+> string "." <+> gDoc

pControlF :: ControlF P a -> P a
pControlF (CIfF cond a b) = P $ \prec ->
  let condDoc = runPretty cond (precedenceTable "App")
      aDoc = runPretty a (precedenceTable "App")
      bDoc = runPretty b (precedenceTable "App")
   in parensIf (prec >= precedenceTable "App") $
        string "if" <+> condDoc
          <+> string "then"
          <+> aDoc
          <+> string "else"
          <+> bDoc
pControlF (CLoopF acc cond iter) = P $ \prec ->
  let accDoc = runPretty acc (precedenceTable "App")
      condDoc = runPretty cond (precedenceTable "App" + associativityTable "App")
      iterDoc = runPretty iter (precedenceTable "App" + associativityTable "App" * 2)
   in parensIf (prec >= precedenceTable "App") $
        string "loop" <+> accDoc
          <$$> nest 2 condDoc
          <$$> nest 2 iterDoc

pPrimF :: PrimF P a -> P a
pPrimF (PLitF x) = P $ const . parens . text . show $ x
pPrimF (PAddF a b) = P $ \prec ->
  let aDoc = runPretty a (precedenceTable "+")
      bDoc = runPretty b (precedenceTable "+" + associativityTable "+")
   in parensIf (prec >= precedenceTable "+") $
        aDoc <+> string "+" <+> bDoc
pPrimF (PSubF a b) = P $ \prec ->
  let aDoc = runPretty a (precedenceTable "-")
      bDoc = runPretty b (precedenceTable "-" + associativityTable "-")
   in parensIf (prec >= precedenceTable "-") $
        aDoc <+> string "-" <+> bDoc
pPrimF (PMultF a b) = P $ \prec ->
  let aDoc = runPretty a (precedenceTable "*")
      bDoc = runPretty b (precedenceTable "*" + associativityTable "*")
   in parensIf (prec >= precedenceTable "*") $
        aDoc <+> string "*" <+> bDoc
pPrimF (PDivF a b) = P $ \prec ->
  let aDoc = runPretty a (precedenceTable "/")
      bDoc = runPretty b (precedenceTable "/" + associativityTable "/")
   in parensIf (prec >= precedenceTable "/") $
        aDoc <+> string "/" <+> bDoc
pPrimF (PAbsF a) = P $ \prec ->
  let aDoc = runPretty a (precedenceTable "App")
   in parensIf (prec >= precedenceTable "App") $
        string "abs" <+> aDoc
pPrimF (PSignumF a) = P $ \prec ->
  let aDoc = runPretty a (precedenceTable "App")
   in parensIf (prec >= precedenceTable "App") $
        string "signum" <+> aDoc
pPrimF (PExpF a) = P $ \prec ->
  let aDoc = runPretty a (precedenceTable "App")
   in parensIf (prec >= precedenceTable "App") $
        string "exp" <+> aDoc
pPrimF (PSqrtF a) = P $ \prec ->
  let aDoc = runPretty a (precedenceTable "App")
   in parensIf (prec >= precedenceTable "App") $
        string "sqrt" <+> aDoc
pPrimF (PLogF a) = P $ \prec ->
  let aDoc = runPretty a (precedenceTable "App")
   in parensIf (prec >= precedenceTable "App") $
        string "log" <+> aDoc
pPrimF (PGTF a b) = P $ \prec ->
  let aDoc = runPretty a (precedenceTable ">")
      bDoc = runPretty b (precedenceTable ">" + associativityTable ">")
   in parensIf (prec >= precedenceTable ">") $
        aDoc <+> string ">" <+> bDoc
pPrimF (PGEF a b) = P $ \prec ->
  let aDoc = runPretty a (precedenceTable ">=")
      bDoc = runPretty b (precedenceTable ">=" + associativityTable ">=")
   in parensIf (prec >= precedenceTable ">=") $
        aDoc <+> string ">=" <+> bDoc
pPrimF (PLTF a b) = P $ \prec ->
  let aDoc = runPretty a (precedenceTable "<")
      bDoc = runPretty b (precedenceTable "<" + associativityTable "<")
   in parensIf (prec >= precedenceTable "<") $
        aDoc <+> string "<" <+> bDoc
pPrimF (PLEF a b) = P $ \prec ->
  let aDoc = runPretty a (precedenceTable "<=")
      bDoc = runPretty b (precedenceTable "<=" + associativityTable "<=")
   in parensIf (prec >= precedenceTable "<=") $
        aDoc <+> string "<=" <+> bDoc
pPrimF (PEQF a b) = P $ \prec ->
  let aDoc = runPretty a (precedenceTable "==")
      bDoc = runPretty b (precedenceTable "==" + associativityTable "==")
   in parensIf (prec >= precedenceTable "==") $
        aDoc <+> string "==" <+> bDoc
pPrimF (PNEQF a b) = P $ \prec ->
  let aDoc = runPretty a (precedenceTable "/=")
      bDoc = runPretty b (precedenceTable "/=" + associativityTable "/=")
   in parensIf (prec >= precedenceTable "/=") $
        aDoc <+> string "/=" <+> bDoc
pPrimF (PAndF a b) = P $ \prec ->
  let aDoc = runPretty a (precedenceTable "&&")
      bDoc = runPretty b (precedenceTable "&&" + associativityTable "&&")
  in parensIf (prec >= precedenceTable "&&") $
       aDoc <+> string "&&" <+> bDoc
pPrimF (POrF a b) = P $ \prec ->
  let aDoc = runPretty a (precedenceTable "||")
      bDoc = runPretty b (precedenceTable "||" + associativityTable "||")
  in parensIf (prec >= precedenceTable "||") $
       aDoc <+> string "||" <+> bDoc
pPrimF (PJustF a) = P $ \prec ->
  let aDoc = runPretty a (precedenceTable "App")
   in parensIf (prec >= precedenceTable "App") $
        string "Just" <+> aDoc
pPrimF PNothingF = P $ const $ string "Nothing"
pPrimF (PFromJustF a) = P $ \prec ->
  let aDoc = runPretty a (precedenceTable "App")
   in parensIf (prec >= precedenceTable "App") $
        string "fromJust" <+> aDoc
pPrimF (PIsJustF a) = P $ \prec ->
  let aDoc = runPretty a (precedenceTable "App")
   in parensIf (prec >= precedenceTable "App") $
        string "isJust" <+> aDoc
pPrimF (PPairF a b) = P $ \prec ->
  let aDoc = runPretty a 0
      bDoc = runPretty b 0
   in parens $ aDoc <> comma <+> bDoc
pPrimF (PFstF a) = P $ \prec ->
  let aDoc = runPretty a (precedenceTable "App")
   in parensIf (prec >= precedenceTable "App") $
        string "fst" <+> aDoc
pPrimF (PSndF a) = P $ \prec ->
  let aDoc = runPretty a (precedenceTable "App")
   in parensIf (prec >= precedenceTable "App") $
        string "snd" <+> aDoc
pPrimF (PLengthF a) = P $ \prec ->
  let aDoc = runPretty a (precedenceTable "App")
  in parensIf (prec >= precedenceTable "App") $
     string "length" <+> aDoc
pPrimF (PIndexF a idx) = P $ \prec ->
  let aDoc = runPretty a (precedenceTable "App")
      idxDoc = runPretty idx (precedenceTable "App" + associativityTable "App")
  in parensIf (prec >= precedenceTable "App") $
     string "index" <+> aDoc <+> idxDoc
pPrimF (PSliceF a start end) = P $ \prec ->
  let aDoc = runPretty a (precedenceTable "App")
      startDoc = runPretty start (precedenceTable "App" + associativityTable "App")
      endDoc = runPretty end (precedenceTable "App" + associativityTable "App" * 2)
  in string "slice" <+> aDoc <+> startDoc <+> endDoc

pMcsF :: McsF P a -> P a
pMcsF (MRunF reprSize clip mf rf) = P $ \prec ->
  let reprSizeDoc = int reprSize
      clipDoc = parens . string . show $ clip
      mfDoc = runPretty mf (precedenceTable "App" + associativityTable "App" * 2)
      rfDoc = runPretty rf (precedenceTable "App" + associativityTable "App" * 3)
      parts =
        vcat
          [ mempty,
            string "reprSize" <+> equals <+> reprSizeDoc <> comma,
            string "clip" <+> equals <+> clipDoc <> comma,
            string "map" <+> equals <+> mfDoc <> comma,
            string "release" <+> equals <+> rfDoc <> comma
          ]
   in string "bmcs" <+> braces (nest 2 parts)

pBmcsF :: BmcsF P a -> P a
pBmcsF (BRunF reprSize clip mstate mf rstate rf) = P $ \prec ->
  let reprSizeDoc = int reprSize
      clipDoc = parens . string . show $ clip
      mstateDoc = runPretty mstate (precedenceTable "App" + associativityTable "App" * 2)
      mfDoc = runPretty mf (precedenceTable "App" + associativityTable "App" * 3)
      rstateDoc = runPretty rstate (precedenceTable "App" + associativityTable "App" * 4)
      rfDoc = runPretty rf (precedenceTable "App" + associativityTable "App" * 5)
      parts =
        vcat
          [ mempty,
            string "reprSize" <+> equals <+> reprSizeDoc <> comma,
            string "clip" <+> equals <+> clipDoc <> comma,
            string "map_state" <+> equals <+> mstateDoc <> comma,
            string "map" <+> equals <+> mfDoc <> comma,
            string "release_state" <+> equals <+> rstateDoc <> comma,
            string "release" <+> equals <+> rfDoc <> comma
          ]
   in string "bmcs" <+> braces (nest 2 parts)

pMainF :: MainF P a -> P a
pMainF =
  pExprMonadF
    `sumAlg` pExprF
    `sumAlg` pControlF
    `sumAlg` pPrimF

pNCPSFuzzF :: NCPSFuzzF P a -> P a
pNCPSFuzzF = pBagOpF `sumAlg` pMainF

pNNormalizedF :: NNormalizedF P a -> P a
pNNormalizedF = pFlatBagOpF `sumAlg` pMainF

pNMcsF :: NMcsF P a -> P a
pNMcsF = pMcsF `sumAlg` pMainF

pNBmcsF :: NBmcsF P a -> P a
pNBmcsF = pBmcsF `sumAlg` pMainF

pMain :: HFix MainF a -> Doc
pMain = flip runPretty 0 . (hcata' pMainF)

pNNormalized :: HFix NNormalizedF a -> Doc
pNNormalized = flip runPretty 0 . (hcata' pNNormalizedF)

pNCPSFuzz :: HFix NCPSFuzzF a -> Doc
pNCPSFuzz = flip runPretty 0 . (hcata' pNCPSFuzzF)

pNMcs :: HFix NMcsF a -> Doc
pNMcs = flip runPretty 0 . (hcata' pNMcsF)

pRedZoneF :: NRedZoneF P a -> P a
pRedZoneF =
  pExprF `sumAlg` pControlF `sumAlg` pPrimF

pRedZone :: HFix NRedZoneF a -> Doc
pRedZone = flip runPretty 0 . (hcata' pRedZoneF)

pNBmcs :: HFix NBmcsF a -> Doc
pNBmcs = flip runPretty 0 . (hcata' pNBmcsF)

instance Show (HFix NCPSFuzzF a) where
  show prog = displayS (renderPretty 0.6 80 (pNCPSFuzz prog)) ""

instance Show (HFix NMcsF a) where
  show prog = displayS (renderPretty 0.6 80 (pNMcs prog)) ""

instance Show (HFix NNormalizedF a) where
  show prog = displayS (renderPretty 0.6 80 (pNNormalized prog)) ""

instance Show (HFix NBmcsF a) where
  show prog = displayS (renderPretty 0.6 80 (pNBmcs prog)) ""
