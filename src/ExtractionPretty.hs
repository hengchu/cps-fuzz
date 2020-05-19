module ExtractionPretty where

import Extraction
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import Text.Printf
import Data.Maybe
import Syntax (Literal(..), Vec(..))
import Control.Lens ((^.))

prec :: String -> Int
prec "call"  = 12000
prec "index" = 11000
prec "~" = 10000
prec "*" = 9000
prec "/" = 9000
prec "+" = 8000
prec "-" = 8000
prec "&" = 7000
prec "^" = 6000
prec "|" = 5000
prec "is not" = 4000
prec "is" = 4000
prec "<" = 4000
prec "<=" = 4000
prec ">" = 4000
prec ">=" = 4000
prec "!=" = 4000
prec "==" = 4000
prec "not" = 3000
prec "and" = 2000
prec "or" = 1000
prec op = error $ printf "unknown operator %s" op

assoc :: String -> Int
assoc "call" = 0
assoc "index" = 0
assoc "~" = 0
assoc "*" = 1
assoc "/" = 1
assoc "+" = 1
assoc "-" = 1
assoc "&" = 1
assoc "^" = 1
assoc "|" = 1
assoc "is not" = 0
assoc "is" = 0
assoc "<" = 0
assoc "<=" = 0
assoc ">" = 0
assoc ">=" = 0
assoc "!=" = 0
assoc "==" = 0
assoc "not" = 0
assoc "and" = 1
assoc "or" = 1
assoc op = error $ printf "unknown operator %s" op

bop2str :: Bop -> String
bop2str = \case
  Add -> "+"
  Mult -> "*"
  Sub -> "-"
  Div -> "/"
  Lt -> "<"
  Le -> "<="
  Gt -> ">"
  Ge -> ">="
  Eq_ -> "=="
  Neq -> "!="
  Conj -> "and"
  Disj -> "or"
  BConj -> "&"
  BDisj -> "|"
  Is -> "is"
  IsNot -> "is not"

uop2str :: Uop -> String
uop2str BNot = "~"

parensIf :: Bool -> Doc -> Doc
parensIf True = parens
parensIf False = id

pLiteral :: Literal -> Doc
pLiteral (I i) = int i
pLiteral (D d) = double d
pLiteral (P (a, b)) = encloseSep lparen rparen comma [pLiteral a, pLiteral b]
pLiteral U = string "()"
pLiteral (V (Vec v)) = encloseSep lbracket rbracket comma $ map double v

pExp :: Int -> Exp -> Doc
pExp p (Binary a op b) =
  let opStr = bop2str op
      opPrec = prec opStr
      opAssoc = assoc opStr
      aDoc = pExp opPrec a
      bDoc = pExp (opPrec+opAssoc) b
  in parensIf (p >= opPrec) (aDoc <+> string opStr <+> bDoc)
pExp _ (List exps) =
  let expDocs = map (pExp 0) exps
  in encloseSep lbracket rbracket comma expDocs
pExp _ (Tuple exps) =
  let expDocs = map (pExp 0) exps
  in encloseSep lparen rparen comma expDocs
pExp p (Index e i) =
  let indexPrec = prec "index"
      eDoc = pExp indexPrec e
      iDoc = pExp 0 i
  in parensIf (p >= indexPrec) $ eDoc <> brackets iDoc
pExp p (Slice e from to) =
  let indexPrec = prec "index"
      eDoc = pExp indexPrec e
      fromDoc = fromMaybe mempty $ pExp 0 <$> from
      toDoc = fromMaybe mempty $ pExp 0 <$> to
  in parensIf (p >= indexPrec) $ eDoc <> brackets (fromDoc <> colon <> toDoc)
pExp p (Unary uop e) =
  let opStr = uop2str uop
      opPrec = prec opStr
      eDoc = pExp opPrec e
  in parensIf (p >= opPrec) $ string opStr <> eDoc
pExp p (Call f args) =
  let opPrec = prec "call"
      fDoc = pExp opPrec f
      argDocs = map (pExp 0) args
  in parensIf (p >= opPrec) $ fDoc <> encloseSep lparen rparen comma argDocs
pExp p (DotCall e funName args) =
  let opPrec = prec "call"
      eDoc = pExp opPrec e
      argDocs = map (pExp 0) args
  in parensIf (p >= opPrec) $ eDoc <> dot <> string funName <> encloseSep lparen rparen comma argDocs
pExp p (CallBuiltin funName args) =
  let opPrec = prec "call"
      argDocs = map (pExp 0) args
  in parensIf (p >= opPrec) $ string funName <> encloseSep lparen rparen comma argDocs
pExp _ (Var name) = string (show name)
pExp _ (Val lit) = pLiteral lit
pExp _ None = string "None"

pFuncDecl :: FuncDecl -> Doc
pFuncDecl (FuncDecl funName funParams funBody) =
  vsep [string "def"
         <+> string (show funName)
         <> encloseSep lparen rparen comma (map (string . show) funParams)
         <> colon,
         indent 2 . pStmts $ funBody]

pStmt :: Stmt -> Doc
pStmt (ExpStmt e) = pExp 0 e
pStmt (Assign x e) =
  string (show x) <+> equals <+> pExp 0 e
pStmt (Assert e) =
  string "assert" <+> pExp 0 e
pStmt (Cond e a b) =
  vsep [string "if" <+> pExp 0 e <> colon,
        indent 2 . pStmts $ a,
        string "else" <> colon,
        indent 2 . pStmts $ b]
pStmt (While cond body) =
  vsep [string "while" <+> pExp 0 cond <> colon,
        indent 2 . pStmts $ body]
pStmt (Ret e) =
  string "return" <+> pExp 0 e
pStmt (Decl fun) = pFuncDecl fun
pStmt Skip = string "skip"

pStmts :: [Stmt] -> Doc
pStmts = vsep . map pStmt

funCompDef :: Doc
funCompDef = vsep [
  string "def fun_comp(g, f):",
  indent 2 $ vsep [string "def inner(x):",
                   indent 2 $ string "return g(f(x))",
                   string "return inner"]
  ]

pExtraction :: Extraction -> Doc
pExtraction extr =
  vsep ["import math",
        "import mpc_math",
        funCompDef,
        hardline,
        pStmts $ extr ^. statements,
        pExp 0 $ extr ^. expr]

pExtractionStr :: Extraction -> String
pExtractionStr = flip displayS "" . renderPretty 1.0 1000 . pExtraction
