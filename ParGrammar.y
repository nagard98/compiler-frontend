-- -*- haskell -*- File generated by the BNF Converter (bnfc 2.9.4.1).

-- Parser definition for use with Happy
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
{-# LANGUAGE PatternSynonyms #-}

module ParGrammar
  ( happyError
  , myLexer
  , pP
  ) where

import Prelude

import qualified AbsGrammar
import LexGrammar

}

%name pP P
-- no lexer declaration
%monad { Err } { (>>=) } { return }
%tokentype {Token}
%token
  '('         { PT _ (TS _ 1)  }
  ')'         { PT _ (TS _ 2)  }
  '*'         { PT _ (TS _ 3)  }
  '+'         { PT _ (TS _ 4)  }
  ','         { PT _ (TS _ 5)  }
  '-'         { PT _ (TS _ 6)  }
  '.'         { PT _ (TS _ 7)  }
  '..'        { PT _ (TS _ 8)  }
  '/'         { PT _ (TS _ 9)  }
  ':'         { PT _ (TS _ 10) }
  ':='        { PT _ (TS _ 11) }
  ';'         { PT _ (TS _ 12) }
  '<'         { PT _ (TS _ 13) }
  '<='        { PT _ (TS _ 14) }
  '<>'        { PT _ (TS _ 15) }
  '='         { PT _ (TS _ 16) }
  '>'         { PT _ (TS _ 17) }
  '>='        { PT _ (TS _ 18) }
  '?'         { PT _ (TS _ 19) }
  '@'         { PT _ (TS _ 20) }
  '['         { PT _ (TS _ 21) }
  ']'         { PT _ (TS _ 22) }
  '^'         { PT _ (TS _ 23) }
  'and'       { PT _ (TS _ 24) }
  'array'     { PT _ (TS _ 25) }
  'begin'     { PT _ (TS _ 26) }
  'boolean'   { PT _ (TS _ 27) }
  'break'     { PT _ (TS _ 28) }
  'char'      { PT _ (TS _ 29) }
  'const'     { PT _ (TS _ 30) }
  'continue'  { PT _ (TS _ 31) }
  'do'        { PT _ (TS _ 32) }
  'downto'    { PT _ (TS _ 33) }
  'else'      { PT _ (TS _ 34) }
  'end'       { PT _ (TS _ 35) }
  'false'     { PT _ (TS _ 36) }
  'for'       { PT _ (TS _ 37) }
  'function'  { PT _ (TS _ 38) }
  'if'        { PT _ (TS _ 39) }
  'integer'   { PT _ (TS _ 40) }
  'mod'       { PT _ (TS _ 41) }
  'not'       { PT _ (TS _ 42) }
  'of'        { PT _ (TS _ 43) }
  'or'        { PT _ (TS _ 44) }
  'procedure' { PT _ (TS _ 45) }
  'program'   { PT _ (TS _ 46) }
  'real'      { PT _ (TS _ 47) }
  'repeat'    { PT _ (TS _ 48) }
  'return'    { PT _ (TS _ 49) }
  'string'    { PT _ (TS _ 50) }
  'then'      { PT _ (TS _ 51) }
  'to'        { PT _ (TS _ 52) }
  'true'      { PT _ (TS _ 53) }
  'until'     { PT _ (TS _ 54) }
  'var'       { PT _ (TS _ 55) }
  'while'     { PT _ (TS _ 56) }
  L_Ident     { PT _ (TV $$)   }
  L_charac    { PT _ (TC $$)   }
  L_doubl     { PT _ (TD $$)   }
  L_integ     { PT _ (TI $$)   }
  L_quoted    { PT _ (TL $$)   }

%%

Ident :: { AbsGrammar.TokIdent }
Ident  : L_Ident { AbsGrammar.TokIdent (mkPosToken $1) }

Char    :: { AbsGrammar.TokChar }
Char     : L_charac { AbsGrammar.TokChar (mkPosToken $1) }

Double  :: { AbsGrammar.TokDouble }
Double   : L_doubl  { AbsGrammar.TokDouble (mkPosToken $1) }

Integer :: { AbsGrammar.TokInteger }
Integer  : L_integ  { AbsGrammar.TokInteger (mkPosToken $1) }

String  :: { AbsGrammar.TokString }
String   : L_quoted { AbsGrammar.TokString (mkPosToken $1) }

Boolean :: { AbsGrammar.TokBoolean }
Boolean
  : 'true' { AbsGrammar.TokBoolean (mkPosToken $1) }
  | 'false' { AbsGrammar.TokBoolean (mkPosToken $1) }

P :: { AbsGrammar.P () ()}
P : PBlock ListDclBlock BEBlock '.' { AbsGrammar.Prog $1 $2 $3 ()}

PBlock :: { AbsGrammar.PBlock }
PBlock : 'program' Ident ';' { AbsGrammar.ProgBlock $2 }

BEBlock :: { AbsGrammar.BEBlock () ()}
BEBlock
  : 'begin' ListStmt 'end' { AbsGrammar.BegEndBlock $2 () }

Stmt :: { AbsGrammar.Stmt () ()}
Stmt
  : DclBlock { AbsGrammar.StmtDecl $1 }
  | BEBlock { AbsGrammar.StmtComp $1 }
  | EXPR ':=' EXPR { AbsGrammar.StmtAssign $1 $3 }
  | Call { AbsGrammar.StmtCall $1 }
  | SelStmt { AbsGrammar.StmtSelect $1 }
  | IterStmt { AbsGrammar.StmtIter $1 }
  | Return { AbsGrammar.StmtReturn $1 }
  | 'break' { AbsGrammar.StmtBreak }
  | 'continue' { AbsGrammar.StmtContinue }

ListStmt :: { [AbsGrammar.Stmt () ()] }
ListStmt
  : {- empty -} { [] }
  | Stmt { (:[]) $1 }
  | Stmt ';' ListStmt { (:) $1 $3 }

SelStmt :: { AbsGrammar.SelStmt () () }
SelStmt
  : 'if' EXPR 'then' Stmt %shift { AbsGrammar.StmtIf $2 $4 }
  | 'if' EXPR 'then' Stmt 'else' Stmt %shift { AbsGrammar.StmtIfElse $2 $4 $6 }

IterStmt :: { AbsGrammar.IterStmt () () }
IterStmt
  : 'while' EXPR 'do' Stmt { AbsGrammar.StmtWhileDo $2 $4 }
  | 'repeat' Stmt 'until' EXPR { AbsGrammar.StmtRepeat $2 $4 }
  | 'for' EXPR ':=' EXPR ForDirection EXPR 'do' Stmt { AbsGrammar.StmtFor $2 $4 $5 $6 $8 }

ForDirection :: { AbsGrammar.ForDirection }
ForDirection
  : 'to' { AbsGrammar.ForDirection_to }
  | 'downto' { AbsGrammar.ForDirection_downto }

Return :: { AbsGrammar.Return () }
Return : 'return' EXPR { AbsGrammar.Ret $2 }

DclBlock :: { AbsGrammar.DclBlock () ()}
DclBlock
  : PcBlock { AbsGrammar.DclBlockPcBlock $1 }
  | VrBlock { AbsGrammar.DclBlockVrBlock $1 }
  | FcBlock { AbsGrammar.DclBlockFcBlock $1 }
  | CsBlock { AbsGrammar.DclBlockCsBlock $1 }

ListDclBlock :: { [AbsGrammar.DclBlock () ()] }
ListDclBlock
  : {- empty -} { [] } | DclBlock ';' ListDclBlock { (:) $1 $3 }

PcBlock :: { AbsGrammar.PcBlock () ()}
PcBlock
  : 'procedure' Ident Prms ';' BEBlock { AbsGrammar.ProcBlock $2 $3 $5 }

FcBlock :: { AbsGrammar.FcBlock () ()}
FcBlock
  : 'function' Ident Prms ':' Type ';' BEBlock { AbsGrammar.FuncBlock $2 $3 $5 $7 }

Prms :: { AbsGrammar.Prms }
Prms
  : '(' ListPrm ')' { AbsGrammar.Params $2 }
  | {- empty -} { AbsGrammar.NoParams }

Prm :: { AbsGrammar.Prm }
Prm : Modality ListIdElem ':' Type { AbsGrammar.Param $1 $2 $4 }

Modality :: { AbsGrammar.Modality }
Modality
  : 'var' { AbsGrammar.Modality_ref }
  | {- empty -} { AbsGrammar.Modality_val }

ListPrm :: { [AbsGrammar.Prm] }
ListPrm : Prm { (:[]) $1 } | Prm ',' ListPrm { (:) $1 $3 }

Call :: { AbsGrammar.Call () }
Call : Ident '(' ListEXPR ')' { AbsGrammar.CallArgs $1 $3 }

ListEXPR :: { [AbsGrammar.EXPR ()] }
ListEXPR
  : {- empty -} { [] }
  | EXPR { (:[]) $1 }
  | EXPR ',' ListEXPR { (:) $1 $3 }

VrBlock :: { AbsGrammar.VrBlock }
VrBlock : 'var' ListVrDef { AbsGrammar.VarBlock $2 }

VrDef :: { AbsGrammar.VrDef }
VrDef : ListIdElem ':' Type { AbsGrammar.VarDefinition $1 $3 }

ListVrDef :: { [AbsGrammar.VrDef] }
ListVrDef : VrDef { (:[]) $1 } | VrDef ',' ListVrDef { (:) $1 $3 }

CsBlock :: { AbsGrammar.CsBlock }
CsBlock : 'const' ListCsDef { AbsGrammar.ConstBlock $2 }

CsDef :: { AbsGrammar.CsDef }
CsDef : IdElem '=' Literal { AbsGrammar.ConstDefinition $1 $3 }

ListCsDef :: { [AbsGrammar.CsDef] }
ListCsDef : CsDef { (:[]) $1 } | CsDef ',' ListCsDef { (:) $1 $3 }

IdElem :: { AbsGrammar.IdElem }
IdElem : Ident { AbsGrammar.IdElement $1 }

ListIdElem :: { [AbsGrammar.IdElem] }
ListIdElem
  : IdElem { (:[]) $1 } | IdElem ',' ListIdElem { (:) $1 $3 }


Type :: { AbsGrammar.Type }
Type
  : BaseType { AbsGrammar.TypeBaseType $1 }
  | CompType { AbsGrammar.TypeCompType $1 }

BaseType :: { AbsGrammar.BaseType }
BaseType
  : 'integer' { AbsGrammar.BaseType_integer }
  | 'boolean' { AbsGrammar.BaseType_boolean }
  | 'real' { AbsGrammar.BaseType_real }
  | 'char' { AbsGrammar.BaseType_char }
  | 'string' { AbsGrammar.BaseType_string }

CompType :: { AbsGrammar.CompType }
CompType
  : 'array' '[' Integer '..' Integer ']' 'of' Type { AbsGrammar.Array $3 $5 $8 }
  | '^' Type { AbsGrammar.Pointer $2 }

EXPR :: { AbsGrammar.EXPR () }
EXPR
  : EXPR1 { $1 }
  | EXPR1 '?' EXPR ':' EXPR { AbsGrammar.SelExpr $1 $3 $5 ()}

EXPR1 :: { AbsGrammar.EXPR () }
EXPR1 : EXPR2 { $1 } | EXPR1 'or' EXPR2 { AbsGrammar.BinaryExpression AbsGrammar.Or $1 $3 ()}

EXPR2 :: { AbsGrammar.EXPR () }
EXPR2 : EXPR3 { $1 } | EXPR2 'and' EXPR3 { AbsGrammar.BinaryExpression AbsGrammar.And $1 $3 ()}

EXPR3 :: { AbsGrammar.EXPR () }
EXPR3 : EXPR4 { $1 } | 'not' EXPR4 { AbsGrammar.UnaryExpression AbsGrammar.Not $2 ()}

EXPR4 :: { AbsGrammar.EXPR () }
EXPR4
  : EXPR5 { $1 }
  | EXPR5 '=' EXPR5 { AbsGrammar.BinaryExpression AbsGrammar.Eq $1 $3 () }
  | EXPR5 '<>' EXPR5 { AbsGrammar.BinaryExpression AbsGrammar.NotEq $1 $3 () }
  | EXPR5 '<' EXPR5 { AbsGrammar.BinaryExpression AbsGrammar.LessT $1 $3 () }
  | EXPR5 '<=' EXPR5 { AbsGrammar.BinaryExpression AbsGrammar.EqLessT $1 $3 () }
  | EXPR5 '>' EXPR5 { AbsGrammar.BinaryExpression AbsGrammar.GreatT $1 $3 () }
  | EXPR5 '>=' EXPR5 { AbsGrammar.BinaryExpression AbsGrammar.EqGreatT $1 $3 () }

EXPR5 :: { AbsGrammar.EXPR () }
EXPR5 : EXPR6 { $1 } | EXPR5 '-' EXPR6 { AbsGrammar.BinaryExpression AbsGrammar.Sub $1 $3 () }

EXPR6 :: { AbsGrammar.EXPR () }
EXPR6 : EXPR7 { $1 } | EXPR6 '+' EXPR7 { AbsGrammar.BinaryExpression AbsGrammar.Add $1 $3 () }

EXPR7 :: { AbsGrammar.EXPR () }
EXPR7 : EXPR8 { $1 } | EXPR7 '/' EXPR8 { AbsGrammar.BinaryExpression AbsGrammar.Div $1 $3 () }

EXPR8 :: { AbsGrammar.EXPR () }
EXPR8 : EXPR9 { $1 } | EXPR8 '*' EXPR9 { AbsGrammar.BinaryExpression AbsGrammar.Mul $1 $3 () }

EXPR9 :: { AbsGrammar.EXPR () }
EXPR9 : EXPR10 { $1 } | EXPR9 'mod' EXPR10 { AbsGrammar.BinaryExpression AbsGrammar.Mod $1 $3 () }

EXPR10 :: { AbsGrammar.EXPR () }
EXPR10
  : EXPR11 { $1 }
  | '-' EXPR11 { AbsGrammar.UnaryExpression AbsGrammar.Negation $2 () }
  | '@' EXPR11 { AbsGrammar.UnaryExpression AbsGrammar.Reference $2 () }
  | EXPR11 '^' { AbsGrammar.UnaryExpression AbsGrammar.Dereference $1 () }

EXPR11 :: { AbsGrammar.EXPR ()}
EXPR11 : EXPR12 { $1 } | Call { AbsGrammar.ExprCall $1 () }

EXPR12 :: { AbsGrammar.EXPR ()}
EXPR12
  : '(' EXPR ')' { $2 }
  | Literal { AbsGrammar.ExprLiteral $1 }
  | BEXPR { AbsGrammar.BaseExpr $1 () }

BEXPR :: { AbsGrammar.BEXPR () }
BEXPR
  : EXPR12 '[' EXPR ']' { AbsGrammar.ArrayElem $1 $3 }
  | Ident { AbsGrammar.Identifier $1 }

Literal :: { AbsGrammar.Literal }
Literal
  : Integer { AbsGrammar.LiteralInteger $1 }
  | String { AbsGrammar.LiteralString $1 }
  | Char { AbsGrammar.LiteralChar $1 }
  | Double { AbsGrammar.LiteralDouble $1 }
  | Boolean { AbsGrammar.LiteralBoolean $1 }

{

type Err = Either String

happyError :: [Token] -> Err a
happyError ts = Left $
  "syntax error at " ++ tokenPos ts ++
  case ts of
    []      -> []
    [Err _] -> " due to lexer error"
    t:_     -> " before `" ++ (prToken t) ++ "'"

myLexer :: String -> [Token]
myLexer = tokens

}

