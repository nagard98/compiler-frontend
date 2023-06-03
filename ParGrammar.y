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
  '@'         { PT _ (TS _ 19) }
  '['         { PT _ (TS _ 20) }
  ']'         { PT _ (TS _ 21) }
  '^'         { PT _ (TS _ 22) }
  'and'       { PT _ (TS _ 23) }
  'array'     { PT _ (TS _ 24) }
  'begin'     { PT _ (TS _ 25) }
  'boolean'   { PT _ (TS _ 26) }
  'char'      { PT _ (TS _ 27) }
  'do'        { PT _ (TS _ 28) }
  'else'      { PT _ (TS _ 29) }
  'end'       { PT _ (TS _ 30) }
  'false'     { PT _ (TS _ 31) }
  'function'  { PT _ (TS _ 32) }
  'if'        { PT _ (TS _ 33) }
  'integer'   { PT _ (TS _ 34) }
  'mod'       { PT _ (TS _ 35) }
  'not'       { PT _ (TS _ 36) }
  'of'        { PT _ (TS _ 37) }
  'or'        { PT _ (TS _ 38) }
  'procedure' { PT _ (TS _ 39) }
  'program'   { PT _ (TS _ 40) }
  'real'      { PT _ (TS _ 41) }
  'repeat'    { PT _ (TS _ 42) }
  'return'    { PT _ (TS _ 43) }
  'string'    { PT _ (TS _ 44) }
  'then'      { PT _ (TS _ 45) }
  'true'      { PT _ (TS _ 46) }
  'until'     { PT _ (TS _ 47) }
  'var'       { PT _ (TS _ 48) }
  'while'     { PT _ (TS _ 49) }
  L_Ident     { PT _ (TV $$)   }
  L_charac    { PT _ (TC $$)   }
  L_doubl     { PT _ (TD $$)   }
  L_integ     { PT _ (TI $$)   }
  L_quoted    { PT _ (TL $$)   }

%%

Ident :: { AbsGrammar.Ident }
Ident  : L_Ident { AbsGrammar.Ident $1 }

Char    :: { Char }
Char     : L_charac { (read $1) :: Char }

Double  :: { Double }
Double   : L_doubl  { (read $1) :: Double }

Integer :: { Integer }
Integer  : L_integ  { (read $1) :: Integer }

String  :: { String }
String   : L_quoted { $1 }

P :: { AbsGrammar.P }
P : PBlock ListDclBlock BEBlock '.' { AbsGrammar.Prog $1 $2 $3 }

PBlock :: { AbsGrammar.PBlock }
PBlock : 'program' Ident ';' { AbsGrammar.ProgBlock $2 }

BEBlock :: { AbsGrammar.BEBlock }
BEBlock
  : 'begin' ListBegEndStmt 'end' { AbsGrammar.BegEndBlock $2 }

BegEndStmt :: { AbsGrammar.BegEndStmt }
BegEndStmt
  : Stmt ';' { AbsGrammar.BegEndStmt1 $1 }
  | DclBlock { AbsGrammar.BegEndStmtDclBlock $1 }

ListBegEndStmt :: { [AbsGrammar.BegEndStmt] }
ListBegEndStmt
  : {- empty -} { [] } | BegEndStmt ListBegEndStmt { (:) $1 $2 }

Stmt :: { AbsGrammar.Stmt }
Stmt
  : BEBlock { AbsGrammar.StmtComp $1 }
  | LEXPR ':=' REXPR { AbsGrammar.StmtAssign $1 $3 }
  | Call { AbsGrammar.StmtCall $1 }
  | SelStmt { AbsGrammar.StmtSelect $1 }
  | IterStmt { AbsGrammar.StmtIter $1 }
  | Return { AbsGrammar.StmtReturn $1 }

SelStmt :: { AbsGrammar.SelStmt }
SelStmt
  : 'if' REXPR 'then' Stmt %shift { AbsGrammar.StmtIf $2 $4 }
  | 'if' REXPR 'then' Stmt 'else' Stmt %shift { AbsGrammar.StmtIfElse $2 $4 $6 }

IterStmt :: { AbsGrammar.IterStmt }
IterStmt
  : 'while' REXPR 'do' Stmt { AbsGrammar.StmtWhileDo $2 $4 }
  | 'repeat' Stmt 'until' REXPR { AbsGrammar.StmtRepeat $2 $4 }

Return :: { AbsGrammar.Return }
Return : 'return' REXPR { AbsGrammar.Ret $2 }

DclBlock :: { AbsGrammar.DclBlock }
DclBlock
  : PcBlock { AbsGrammar.DclBlockPcBlock $1 }
  | VrBlock { AbsGrammar.DclBlockVrBlock $1 }
  | FcBlock { AbsGrammar.DclBlockFcBlock $1 }

ListDclBlock :: { [AbsGrammar.DclBlock] }
ListDclBlock
  : {- empty -} { [] } | DclBlock ListDclBlock { (:) $1 $2 }

PcBlock :: { AbsGrammar.PcBlock }
PcBlock
  : 'procedure' Ident Prms ';' BEBlock ';' { AbsGrammar.ProcBlock $2 $3 $5 }

FcBlock :: { AbsGrammar.FcBlock }
FcBlock
  : 'function' Ident Prms ':' Type ';' BEBlock ';' { AbsGrammar.FuncBlock $2 $3 $5 $7 }

Prms :: { AbsGrammar.Prms }
Prms
  : '(' ListPrm ')' { AbsGrammar.Params $2 }
  | {- empty -} { AbsGrammar.NoParams }

Prm :: { AbsGrammar.Prm }
Prm : ListVRI ':' Type { AbsGrammar.Param $1 $3 }

ListPrm :: { [AbsGrammar.Prm] }
ListPrm : Prm { (:[]) $1 } | Prm ',' ListPrm { (:) $1 $3 }

Call :: { AbsGrammar.Call }
Call : Ident '(' ListREXPR ')' { AbsGrammar.CallArgs $1 $3 }

ListREXPR :: { [AbsGrammar.REXPR] }
ListREXPR
  : {- empty -} { [] }
  | REXPR { (:[]) $1 }
  | REXPR ',' ListREXPR { (:) $1 $3 }

VrBlock :: { AbsGrammar.VrBlock }
VrBlock : 'var' ListVrDef { AbsGrammar.VarBlock $2 }

VrDef :: { AbsGrammar.VrDef }
VrDef : ListVRI ':' Type { AbsGrammar.VarDefinition $1 $3 }

VRI :: { AbsGrammar.VRI }
VRI : Ident { AbsGrammar.VarId $1 }

ListVRI :: { [AbsGrammar.VRI] }
ListVRI : VRI { (:[]) $1 } | VRI ',' ListVRI { (:) $1 $3 }

ListVrDef :: { [AbsGrammar.VrDef] }
ListVrDef
  : VrDef ';' { (:[]) $1 } | VrDef ';' ListVrDef { (:) $1 $3 }

Boolean :: { AbsGrammar.Boolean }
Boolean
  : 'true' { AbsGrammar.Boolean_true }
  | 'false' { AbsGrammar.Boolean_false }

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
  : 'array' '[' Integer '..' Integer ']' 'of' Type { AbsGrammar.CompType1 $3 $5 $8 }
  | '^' BaseType { AbsGrammar.CompType2 $2 }

REXPR :: { AbsGrammar.REXPR }
REXPR : REXPR1 { $1 } | REXPR 'or' REXPR1 { AbsGrammar.Or $1 $3 }

REXPR1 :: { AbsGrammar.REXPR }
REXPR1
  : REXPR2 { $1 } | REXPR1 'and' REXPR2 { AbsGrammar.And $1 $3 }

REXPR2 :: { AbsGrammar.REXPR }
REXPR2 : REXPR3 { $1 } | 'not' REXPR3 { AbsGrammar.Not $2 }

REXPR3 :: { AbsGrammar.REXPR }
REXPR3
  : REXPR4 { $1 }
  | REXPR4 '=' REXPR4 { AbsGrammar.Eq $1 $3 }
  | REXPR4 '<>' REXPR4 { AbsGrammar.NotEq $1 $3 }
  | REXPR4 '<' REXPR4 { AbsGrammar.LessT $1 $3 }
  | REXPR4 '<=' REXPR4 { AbsGrammar.EqLessT $1 $3 }
  | REXPR4 '>' REXPR4 { AbsGrammar.GreatT $1 $3 }
  | REXPR4 '>=' REXPR4 { AbsGrammar.EqGreatT $1 $3 }

REXPR4 :: { AbsGrammar.REXPR }
REXPR4 : REXPR5 { $1 } | REXPR4 '-' REXPR5 { AbsGrammar.Sub $1 $3 }

REXPR5 :: { AbsGrammar.REXPR }
REXPR5 : REXPR6 { $1 } | REXPR5 '+' REXPR6 { AbsGrammar.Add $1 $3 }

REXPR6 :: { AbsGrammar.REXPR }
REXPR6 : REXPR7 { $1 } | REXPR6 '/' REXPR7 { AbsGrammar.Div $1 $3 }

REXPR7 :: { AbsGrammar.REXPR }
REXPR7 : REXPR8 { $1 } | REXPR7 '*' REXPR8 { AbsGrammar.Mul $1 $3 }

REXPR8 :: { AbsGrammar.REXPR }
REXPR8
  : REXPR9 { $1 } | REXPR8 'mod' REXPR9 { AbsGrammar.Mod $1 $3 }

REXPR9 :: { AbsGrammar.REXPR }
REXPR9
  : REXPR10 { $1 }
  | '-' REXPR10 { AbsGrammar.Negation $2 }
  | '@' REXPR10 { AbsGrammar.Reference $2 }
  | REXPR10 '^' { AbsGrammar.Dereference $1 }

REXPR10 :: { AbsGrammar.REXPR }
REXPR10
  : REXPR11 { $1 }
  | Integer { AbsGrammar.Int $1 }
  | String { AbsGrammar.String $1 }
  | Char { AbsGrammar.Char $1 }
  | Double { AbsGrammar.Real $1 }
  | Boolean { AbsGrammar.Bool $1 }

REXPR11 :: { AbsGrammar.REXPR }
REXPR11 : REXPR12 { $1 } | Call { AbsGrammar.ExprCall $1 }

REXPR12 :: { AbsGrammar.REXPR }
REXPR12
  : '(' REXPR ')' { $2 } | LEXPR { AbsGrammar.LExpression $1 }

LEXPR :: { AbsGrammar.LEXPR }
LEXPR : BLEXPR { AbsGrammar.BaseLExpr $1 }

BLEXPR :: { AbsGrammar.BLEXPR }
BLEXPR
  : Ident { AbsGrammar.Identifier $1 }
  | BLEXPR '[' REXPR ']' { AbsGrammar.ArrayElem $1 $3 }

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
