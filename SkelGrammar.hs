-- File generated by the BNF Converter (bnfc 2.9.4.1).

-- Templates for pattern matching on abstract syntax

{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module SkelGrammar where

import Prelude (($), Either(..), String, (++), Show, show)
import qualified AbsGrammar

type Err = Either String
type Result = Err String

failure :: Show a => a -> Result
failure x = Left $ "Undefined case: " ++ show x

transIdent :: AbsGrammar.TokIdent -> Result
transIdent x = case x of
  AbsGrammar.TokIdent string -> failure x

transP :: AbsGrammar.P -> Result
transP x = case x of
  AbsGrammar.Prog pblock dclblocks beblock -> failure x

transPBlock :: AbsGrammar.PBlock -> Result
transPBlock x = case x of
  AbsGrammar.ProgBlock ident -> failure x

transBEBlock :: AbsGrammar.BEBlock -> Result
transBEBlock x = case x of
  AbsGrammar.BegEndBlock begendstmts -> failure x

transBegEndStmt :: AbsGrammar.BegEndStmt -> Result
transBegEndStmt x = case x of
  AbsGrammar.BegEndStmt1 stmt -> failure x
  AbsGrammar.BegEndStmtDclBlock dclblock -> failure x

transStmt :: AbsGrammar.Stmt -> Result
transStmt x = case x of
  AbsGrammar.StmtComp beblock -> failure x
  AbsGrammar.StmtAssign lexpr rexpr -> failure x
  AbsGrammar.StmtCall call -> failure x
  AbsGrammar.StmtSelect selstmt -> failure x
  AbsGrammar.StmtIter iterstmt -> failure x
  AbsGrammar.StmtReturn return -> failure x

transSelStmt :: AbsGrammar.SelStmt -> Result
transSelStmt x = case x of
  AbsGrammar.StmtIf rexpr stmt -> failure x
  AbsGrammar.StmtIfElse rexpr stmt1 stmt2 -> failure x

transIterStmt :: AbsGrammar.IterStmt -> Result
transIterStmt x = case x of
  AbsGrammar.StmtWhileDo rexpr stmt -> failure x
  AbsGrammar.StmtRepeat stmt rexpr -> failure x

transReturn :: AbsGrammar.Return -> Result
transReturn x = case x of
  AbsGrammar.Ret rexpr -> failure x

transDclBlock :: AbsGrammar.DclBlock -> Result
transDclBlock x = case x of
  AbsGrammar.DclBlockPcBlock pcblock -> failure x
  AbsGrammar.DclBlockVrBlock vrblock -> failure x
  AbsGrammar.DclBlockFcBlock fcblock -> failure x
  AbsGrammar.DclBlockCsBlock csblock -> failure x

transPcBlock :: AbsGrammar.PcBlock -> Result
transPcBlock x = case x of
  AbsGrammar.ProcBlock ident prms beblock -> failure x

transFcBlock :: AbsGrammar.FcBlock -> Result
transFcBlock x = case x of
  AbsGrammar.FuncBlock ident prms type_ beblock -> failure x

transPrms :: AbsGrammar.Prms -> Result
transPrms x = case x of
  AbsGrammar.Params prms -> failure x
  AbsGrammar.NoParams -> failure x

transPrm :: AbsGrammar.Prm -> Result
transPrm x = case x of
  AbsGrammar.Param modality idelems type_ -> failure x

transModality :: AbsGrammar.Modality -> Result
transModality x = case x of
  AbsGrammar.Modality_var -> failure x
  AbsGrammar.Modality1 -> failure x

transCall :: AbsGrammar.Call -> Result
transCall x = case x of
  AbsGrammar.CallArgs ident rexprs -> failure x

transVrBlock :: AbsGrammar.VrBlock -> Result
transVrBlock x = case x of
  AbsGrammar.VarBlock vrdefs -> failure x

transVrDef :: AbsGrammar.VrDef -> Result
transVrDef x = case x of
  AbsGrammar.VarDefinition idelems type_ -> failure x

transCsBlock :: AbsGrammar.CsBlock -> Result
transCsBlock x = case x of
  AbsGrammar.ConstBlock csdefs -> failure x

transCsDef :: AbsGrammar.CsDef -> Result
transCsDef x = case x of
  AbsGrammar.ConstDefinition idelem literal -> failure x

transIdElem :: AbsGrammar.IdElem -> Result
transIdElem x = case x of
  AbsGrammar.IdElement ident -> failure x

--transBoolean :: AbsGrammar.Boolean -> Result
--transBoolean x = case x of
--  AbsGrammar.Boolean_true -> failure x
-- AbsGrammar.Boolean_false -> failure x

transType :: AbsGrammar.Type -> Result
transType x = case x of
  AbsGrammar.TypeBaseType basetype -> failure x
  AbsGrammar.TypeCompType comptype -> failure x

transBaseType :: AbsGrammar.BaseType -> Result
transBaseType x = case x of
  AbsGrammar.BaseType_integer -> failure x
  AbsGrammar.BaseType_boolean -> failure x
  AbsGrammar.BaseType_real -> failure x
  AbsGrammar.BaseType_char -> failure x
  AbsGrammar.BaseType_string -> failure x

transCompType :: AbsGrammar.CompType -> Result
transCompType x = case x of
  AbsGrammar.CompType1 integer1 integer2 type_ -> failure x
  AbsGrammar.CompType2 basetype -> failure x

transREXPR :: AbsGrammar.REXPR -> Result
transREXPR x = case x of
  AbsGrammar.BinaryExpression _ rexpr1 rexpr2 -> failure x
  AbsGrammar.UnaryExpression _ rexpr1 -> failure x
  AbsGrammar.ExprLiteral literal -> failure x
  AbsGrammar.ExprCall call -> failure x
  AbsGrammar.LExpression lexpr -> failure x

transLEXPR :: AbsGrammar.LEXPR -> Result
transLEXPR x = case x of
  AbsGrammar.BaseLExpr blexpr -> failure x

transBLEXPR :: AbsGrammar.BLEXPR -> Result
transBLEXPR x = case x of
  AbsGrammar.Identifier ident -> failure x
  AbsGrammar.ArrayElem blexpr rexpr -> failure x

transLiteral :: AbsGrammar.Literal -> Result
transLiteral x = case x of
  AbsGrammar.LiteralInteger integer -> failure x
  AbsGrammar.LiteralString string -> failure x
  AbsGrammar.LiteralChar char -> failure x
  AbsGrammar.LiteralDouble double -> failure x
  AbsGrammar.LiteralBoolean boolean -> failure x
