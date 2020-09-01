{-|
Module: ExtractionPretty
Description: Pretty printer of extracted code
-}
module ExtractionPretty where

import Extraction
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import Text.Printf
import Data.Maybe
import Syntax (Literal(..), Vec(..))
import Control.Lens ((^.))
import qualified Data.Map as M

-- |The precedence table of all operators.
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

-- |The associativity table of all operators.
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

-- |Pretty print a binary operator.
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

-- |Pretty print a unary operator.
uop2str :: Uop -> String
uop2str BNot = "~"

-- |Wrap the pretty printed document in parenthesis if the given condition is
-- 'True'.
parensIf :: Bool -> Doc -> Doc
parensIf True = parens
parensIf False = id

-- |Pretty print a literal value.
pLiteral :: Literal -> Doc
pLiteral (I i) = int i
pLiteral (D d) = double d
pLiteral (P (a, b)) = encloseSep lparen rparen comma [pLiteral a, pLiteral b]
pLiteral U = string "()"
pLiteral (V (Vec v)) = encloseSep lbracket rbracket comma $ map double v

-- |Pretty print an expression.
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
pFuncDecl (FuncDecl funName funParams funBody funAnnotations) =
  let comments = string "\"\"\"" <> hardline <> commentsInner <> hardline <> string "\"\"\""
      commentsInner = vsep . map (\(name, ann) -> string (show name) <> colon <+> string ann) . M.toList $ funAnnotations
  in if M.null funAnnotations
     then
       vsep [string "def"
              <+> string (show funName)
              <> encloseSep lparen rparen comma (map (string . show) funParams)
              <> colon,
              indent 2 . pStmts $ funBody]
     else
       vsep [string "def"
              <+> string (show funName)
              <> encloseSep lparen rparen comma (map (string . show) funParams)
              <> colon,
              indent 2 comments,
              indent 2 . pStmts $ funBody]

-- |Pretty print a statement.
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

-- |Pretty print a list of statements.
pStmts :: [Stmt] -> Doc
pStmts = vsep . map pStmt

-- |A prologue that contains definitions of function composition.
funCompDef :: Doc
funCompDef = vsep [
  string "def fun_comp(g, f):",
  indent 2 $ vsep [string "def inner(x):",
                   indent 2 $ string "return g(f(x))",
                   string "return inner"]
  ]

-- |Convert extracted code into a pretty printed document.
pExtraction :: Extraction -> Doc
pExtraction extr =
  vsep ["import math",
        "import mpc_math",
        funCompDef,
        hardline,
        pStmts $ extr ^. statements,
        pExp 0 $ extr ^. expr]

-- |Same as 'pExtraction', but directly outputs a string.
pExtractionStr :: Extraction -> String
pExtractionStr = flip displayS "" . renderPretty 1.0 5000 . pExtraction
