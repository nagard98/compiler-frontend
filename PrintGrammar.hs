-- File generated by the BNF Converter (bnfc 2.9.4.1).

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
#if __GLASGOW_HASKELL__ <= 708
{-# LANGUAGE OverlappingInstances #-}
#endif

-- | Pretty-printer for PrintGrammar.

module PrintGrammar where

import Prelude
  ( ($), (.)
  , Bool(..), (==), (<)
  , Int, Integer, Double, (+), (-), (*)
  , String, (++)
  , ShowS, showChar, showString
  , all, elem, foldr, id, map, null, replicate, shows, span
  )
import Data.Char ( Char, isSpace )
import qualified AbsGrammar

-- | The top-level printing method.

printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 False (map ($ "") $ d []) ""
  where
  rend
    :: Int        -- ^ Indentation level.
    -> Bool       -- ^ Pending indentation to be output before next character?
    -> [String]
    -> ShowS
  rend i p = \case
      "["      :ts -> char '[' . rend i False ts
      "("      :ts -> char '(' . rend i False ts
      "{"      :ts -> onNewLine i     p . showChar   '{'  . new (i+1) ts
      "}" : ";":ts -> onNewLine (i-1) p . showString "};" . new (i-1) ts
      "}"      :ts -> onNewLine (i-1) p . showChar   '}'  . new (i-1) ts
      [";"]        -> char ';'
      ";"      :ts -> char ';' . new i ts
      t  : ts@(s:_) | closingOrPunctuation s
                   -> pending . showString t . rend i False ts
      t        :ts -> pending . space t      . rend i False ts
      []           -> id
    where
    -- Output character after pending indentation.
    char :: Char -> ShowS
    char c = pending . showChar c

    -- Output pending indentation.
    pending :: ShowS
    pending = if p then indent i else id

  -- Indentation (spaces) for given indentation level.
  indent :: Int -> ShowS
  indent i = replicateS (2*i) (showChar ' ')

  -- Continue rendering in new line with new indentation.
  new :: Int -> [String] -> ShowS
  new j ts = showChar '\n' . rend j True ts

  -- Make sure we are on a fresh line.
  onNewLine :: Int -> Bool -> ShowS
  onNewLine i p = (if p then id else showChar '\n') . indent i

  -- Separate given string from following text by a space (if needed).
  space :: String -> ShowS
  space t s =
    case (all isSpace t', null spc, null rest) of
      (True , _   , True ) -> []              -- remove trailing space
      (False, _   , True ) -> t'              -- remove trailing space
      (False, True, False) -> t' ++ ' ' : s   -- add space if none
      _                    -> t' ++ s
    where
      t'          = showString t []
      (spc, rest) = span isSpace s

  closingOrPunctuation :: String -> Bool
  closingOrPunctuation [c] = c `elem` closerOrPunct
  closingOrPunctuation _   = False

  closerOrPunct :: String
  closerOrPunct = ")],;"

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- | The printer class does the job.

class Print a where
  prt :: Int -> a -> Doc

instance {-# OVERLAPPABLE #-} Print a => Print [a] where
  prt i = concatD . map (prt i)

instance Print Char where
  prt _ c = doc (showChar '\'' . mkEsc '\'' c . showChar '\'')

instance Print String where
  prt _ = printString

printString :: String -> Doc
printString s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q = \case
  s | s == q -> showChar '\\' . showChar s
  '\\' -> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  s -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j < i then parenth else id

instance Print Integer where
  prt _ x = doc (shows x)

instance Print Double where
  prt _ x = doc (shows x)

instance Print AbsGrammar.TokIdent where
  prt _ (AbsGrammar.TokIdent (_,i)) = doc $ showString i
instance Print AbsGrammar.TokChar where
  prt _ (AbsGrammar.TokChar (_,i)) = doc $ showString i
instance Print AbsGrammar.TokDouble where
  prt _ (AbsGrammar.TokDouble (_,i)) = doc $ showString i
instance Print AbsGrammar.TokInteger where
  prt _ (AbsGrammar.TokInteger (_,i)) = doc $ showString i
instance Print AbsGrammar.TokString where
  prt _ (AbsGrammar.TokString (_,i)) = doc $ showString i
instance Print AbsGrammar.TokBoolean where
  prt _ (AbsGrammar.TokBoolean (_,i)) = doc $ showString i

--instance Print AbsGrammar.Ident where
--  prt _ (AbsGrammar.Ident i) = doc $ showString i
instance Print (AbsGrammar.P env infType) where
  prt i = \case
    AbsGrammar.Prog pblock dclblocks beblock globEnv -> prPrec i 0 (concatD [prt 0 pblock, prt 0 dclblocks, prt 0 beblock, doc (showString ".")])

instance Print AbsGrammar.PBlock where
  prt i = \case
    AbsGrammar.ProgBlock id_ -> prPrec i 0 (concatD [doc (showString "program"), prt 0 id_, doc (showString ";")])

instance Print (AbsGrammar.BEBlock env infType) where
  prt i = \case
    AbsGrammar.BegEndBlock stmts env -> prPrec i 0 (concatD [doc (showString "begin"), prt 0 stmts, doc (showString "end")])

instance Print (AbsGrammar.Stmt env infType) where
  prt i = \case
    AbsGrammar.StmtDecl dclblock -> prPrec i 0 (concatD [prt 0 dclblock])
    AbsGrammar.StmtComp beblock -> prPrec i 0 (concatD [prt 0 beblock])
    AbsGrammar.StmtAssign expr1 expr2 -> prPrec i 0 (concatD [prt 0 expr1, doc (showString ":="), prt 0 expr2])
    AbsGrammar.StmtCall call -> prPrec i 0 (concatD [prt 0 call])
    AbsGrammar.StmtSelect selstmt -> prPrec i 0 (concatD [prt 0 selstmt])
    AbsGrammar.StmtIter iterstmt -> prPrec i 0 (concatD [prt 0 iterstmt])
    AbsGrammar.StmtReturn return -> prPrec i 0 (concatD [prt 0 return])
    AbsGrammar.StmtBreak -> prPrec i 0 (concatD [doc (showString "break")])
    AbsGrammar.StmtContinue -> prPrec i 0 (concatD [doc (showString "continue")])

instance Print [AbsGrammar.Stmt env infType] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ";"), prt 0 xs]

instance Print (AbsGrammar.SelStmt env infType) where
  prt i = \case
    AbsGrammar.StmtIf expr stmt -> prPrec i 0 (concatD [doc (showString "if"), prt 0 expr, doc (showString "then"), prt 0 stmt])
    AbsGrammar.StmtIfElse expr stmt1 stmt2 -> prPrec i 0 (concatD [doc (showString "if"), prt 0 expr, doc (showString "then"), prt 0 stmt1, doc (showString "else"), prt 0 stmt2])

instance Print (AbsGrammar.IterStmt env infType) where
  prt i = \case
    AbsGrammar.StmtWhileDo expr stmt -> prPrec i 0 (concatD [doc (showString "while"), prt 0 expr, doc (showString "do"), prt 0 stmt])
    AbsGrammar.StmtRepeat stmt expr -> prPrec i 0 (concatD [doc (showString "repeat"), prt 0 stmt, doc (showString "until"), prt 0 expr])
    AbsGrammar.StmtFor stmt1 fordirection expr stmt2 -> prPrec i 0 (concatD [doc (showString "for"), prt 0 stmt1, prt 0 fordirection, prt 0 expr, doc (showString "do"), prt 0 stmt2])

instance Print AbsGrammar.ForDirection where
  prt i = \case
    AbsGrammar.ForDirection_to -> prPrec i 0 (concatD [doc (showString "to")])
    AbsGrammar.ForDirection_downto -> prPrec i 0 (concatD [doc (showString "downto")])

instance Print (AbsGrammar.Return infType) where
  prt i = \case
    AbsGrammar.Ret expr -> prPrec i 0 (concatD [doc (showString "return"), prt 0 expr])

instance Print (AbsGrammar.DclBlock env infType) where
  prt i = \case
    AbsGrammar.DclBlockPcBlock pcblock -> prPrec i 0 (concatD [prt 0 pcblock])
    AbsGrammar.DclBlockVrBlock vrblock -> prPrec i 0 (concatD [prt 0 vrblock])
    AbsGrammar.DclBlockFcBlock fcblock -> prPrec i 0 (concatD [prt 0 fcblock])
    AbsGrammar.DclBlockCsBlock csblock -> prPrec i 0 (concatD [prt 0 csblock])

instance Print [AbsGrammar.DclBlock env infType] where
  prt _ [] = concatD []
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ";"), prt 0 xs]

instance Print (AbsGrammar.PcBlock env infType) where
  prt i = \case
    AbsGrammar.ProcBlock id_ prms beblock -> prPrec i 0 (concatD [doc (showString "procedure"), prt 0 id_, prt 0 prms, doc (showString ";"), prt 0 beblock])

instance Print (AbsGrammar.FcBlock env infType) where
  prt i = \case
    AbsGrammar.FuncBlock id_ prms type_ beblock -> prPrec i 0 (concatD [doc (showString "function"), prt 0 id_, prt 0 prms, doc (showString ":"), prt 0 type_, doc (showString ";"), prt 0 beblock])

instance Print AbsGrammar.Prms where
  prt i = \case
    AbsGrammar.Params prms -> prPrec i 0 (concatD [doc (showString "("), prt 0 prms, doc (showString ")")])
    AbsGrammar.NoParams -> prPrec i 0 (concatD [])

instance Print AbsGrammar.Prm where
  prt i = \case
    AbsGrammar.Param modality idelems type_ -> prPrec i 0 (concatD [prt 0 modality, prt 0 idelems, doc (showString ":"), prt 0 type_])

instance Print AbsGrammar.Modality where
  prt i = \case
    AbsGrammar.Modality_ref -> prPrec i 0 (concatD [doc (showString "var")])
    AbsGrammar.Modality_val -> prPrec i 0 (concatD [])

instance Print [AbsGrammar.Prm] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print (AbsGrammar.Call infType) where
  prt i = \case
    AbsGrammar.CallArgs id_ exprs -> prPrec i 0 (concatD [prt 0 id_, doc (showString "("), prt 0 exprs, doc (showString ")")])

instance Print [AbsGrammar.EXPR infType] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print AbsGrammar.VrBlock where
  prt i = \case
    AbsGrammar.VarBlock vrdefs -> prPrec i 0 (concatD [doc (showString "var"), prt 0 vrdefs])

instance Print AbsGrammar.VrDef where
  prt i = \case
    AbsGrammar.VarDefinition idelems type_ -> prPrec i 0 (concatD [prt 0 idelems, doc (showString ":"), prt 0 type_])

instance Print [AbsGrammar.VrDef] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print AbsGrammar.CsBlock where
  prt i = \case
    AbsGrammar.ConstBlock csdefs -> prPrec i 0 (concatD [doc (showString "const"), prt 0 csdefs])

instance Print AbsGrammar.CsDef where
  prt i = \case
    AbsGrammar.ConstDefinition idelem literal -> prPrec i 0 (concatD [prt 0 idelem, doc (showString "="), prt 0 literal])

instance Print [AbsGrammar.CsDef] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print AbsGrammar.IdElem where
  prt i = \case
    AbsGrammar.IdElement id_ -> prPrec i 0 (concatD [prt 0 id_])

instance Print [AbsGrammar.IdElem] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print AbsGrammar.Type where
  prt i = \case
    AbsGrammar.TypeBaseType basetype -> prPrec i 0 (concatD [prt 0 basetype])
    AbsGrammar.TypeCompType comptype -> prPrec i 0 (concatD [prt 0 comptype])

instance Print AbsGrammar.BaseType where
  prt i = \case
    AbsGrammar.BaseType_integer -> prPrec i 0 (concatD [doc (showString "integer")])
    AbsGrammar.BaseType_boolean -> prPrec i 0 (concatD [doc (showString "boolean")])
    AbsGrammar.BaseType_real -> prPrec i 0 (concatD [doc (showString "real")])
    AbsGrammar.BaseType_char -> prPrec i 0 (concatD [doc (showString "char")])
    AbsGrammar.BaseType_string -> prPrec i 0 (concatD [doc (showString "string")])

instance Print AbsGrammar.CompType where
  prt i = \case
    AbsGrammar.Array n1 n2 type_ -> prPrec i 0 (concatD [doc (showString "array"), doc (showString "["), prt 0 n1, doc (showString ".."), prt 0 n2, doc (showString "]"), doc (showString "of"), prt 0 type_])
    AbsGrammar.Pointer type_ -> prPrec i 0 (concatD [doc (showString "^"), prt 0 type_])

instance Print (AbsGrammar.EXPR infType) where
  prt i = \case
    AbsGrammar.SelExpr expr1 expr2 expr3 infType -> prPrec i 0 (concatD [prt 1 expr1, doc (showString "?"), prt 0 expr2, doc (showString ":"), prt 0 expr3])
    AbsGrammar.BinaryExpression AbsGrammar.Or expr1 expr2 infType -> prPrec i 0 (concatD [prt 0 expr1, doc (showString "or"), prt 1 expr2])
    AbsGrammar.BinaryExpression AbsGrammar.And expr1 expr2 infType -> prPrec i 1 (concatD [prt 1 expr1, doc (showString "and"), prt 2 expr2])
    AbsGrammar.UnaryExpression AbsGrammar.Not expr infType -> prPrec i 2 (concatD [doc (showString "not"), prt 3 expr])
    AbsGrammar.BinaryExpression AbsGrammar.Eq expr1 expr2 infType -> prPrec i 3 (concatD [prt 4 expr1, doc (showString "="), prt 4 expr2])
    AbsGrammar.BinaryExpression AbsGrammar.NotEq expr1 expr2 infType -> prPrec i 3 (concatD [prt 4 expr1, doc (showString "<>"), prt 4 expr2])
    AbsGrammar.BinaryExpression AbsGrammar.LessT expr1 expr2 infType -> prPrec i 3 (concatD [prt 4 expr1, doc (showString "<"), prt 4 expr2])
    AbsGrammar.BinaryExpression AbsGrammar.EqLessT expr1 expr2 infType -> prPrec i 3 (concatD [prt 4 expr1, doc (showString "<="), prt 4 expr2])
    AbsGrammar.BinaryExpression AbsGrammar.GreatT expr1 expr2 infType -> prPrec i 3 (concatD [prt 4 expr1, doc (showString ">"), prt 4 expr2])
    AbsGrammar.BinaryExpression AbsGrammar.EqGreatT expr1 expr2 infType -> prPrec i 3 (concatD [prt 4 expr1, doc (showString ">="), prt 4 expr2])
    AbsGrammar.BinaryExpression AbsGrammar.Sub expr1 expr2 infType -> prPrec i 4 (concatD [prt 4 expr1, doc (showString "-"), prt 5 expr2])
    AbsGrammar.BinaryExpression AbsGrammar.Add expr1 expr2 infType -> prPrec i 5 (concatD [prt 5 expr1, doc (showString "+"), prt 6 expr2])
    AbsGrammar.BinaryExpression AbsGrammar.Div expr1 expr2 infType -> prPrec i 6 (concatD [prt 6 expr1, doc (showString "/"), prt 7 expr2])
    AbsGrammar.BinaryExpression AbsGrammar.Mul expr1 expr2 infType -> prPrec i 7 (concatD [prt 7 expr1, doc (showString "*"), prt 8 expr2])
    AbsGrammar.BinaryExpression AbsGrammar.Mod expr1 expr2 infType -> prPrec i 8 (concatD [prt 8 expr1, doc (showString "mod"), prt 9 expr2])
    AbsGrammar.UnaryExpression AbsGrammar.Negation expr infType -> prPrec i 9 (concatD [doc (showString "-"), prt 10 expr])
    AbsGrammar.UnaryExpression AbsGrammar.Reference expr infType -> prPrec i 9 (concatD [doc (showString "@"), prt 10 expr])
    AbsGrammar.UnaryExpression AbsGrammar.Dereference expr infType -> prPrec i 9 (concatD [prt 10 expr, doc (showString "^")])
    AbsGrammar.ExprLiteral literal -> prPrec i 10 (concatD [prt 0 literal])
    AbsGrammar.ExprCall call infType -> prPrec i 11 (concatD [prt 0 call])
    AbsGrammar.BaseExpr bexpr infType -> prPrec i 12 (concatD [prt 0 bexpr])

instance Print (AbsGrammar.BEXPR infType) where
  prt i = \case
    AbsGrammar.ArrayElem expr1 expr2 -> prPrec i 0 (concatD [prt 12 expr1, doc (showString "["), prt 0 expr2, doc (showString "]")])
    AbsGrammar.Identifier id_ -> prPrec i 0 (concatD [prt 0 id_])

instance Print AbsGrammar.Literal where
  prt i = \case
    AbsGrammar.LiteralInteger n -> prPrec i 0 (concatD [prt 0 n])
--    AbsGrammar.LiteralString str -> prPrec i 0 (concatD [printString str])
    AbsGrammar.LiteralString str -> prPrec i 0 (concatD [prt 0 str])
    AbsGrammar.LiteralChar c -> prPrec i 0 (concatD [prt 0 c])
    AbsGrammar.LiteralDouble d -> prPrec i 0 (concatD [prt 0 d])
    AbsGrammar.LiteralBoolean boolean -> prPrec i 0 (concatD [prt 0 boolean])
