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

-- Implementare TokIdent al posto di Ident
transIdent :: AbsGrammar.TokIdent -> Result
transIdent x = case x of
  AbsGrammar.TokIdent string -> failure x

transP :: (Show env, Show infType)  => AbsGrammar.P env infType -> Result
transP x = case x of
  AbsGrammar.Prog pblock dclblocks beblock globEnv -> failure x

transPBlock :: AbsGrammar.PBlock -> Result
transPBlock x = case x of
  AbsGrammar.ProgBlock ident -> failure x

transBEBlock :: (Show env, Show infType)  => AbsGrammar.BEBlock env infType -> Result
transBEBlock x = case x of
  AbsGrammar.BegEndBlock stmts env -> failure x

transStmt :: (Show env, Show infType)  => AbsGrammar.Stmt env infType -> Result
transStmt x = case x of
  AbsGrammar.StmtDecl dclblock -> failure x
  AbsGrammar.StmtComp beblock -> failure x
  AbsGrammar.StmtAssign expr1 expr2 -> failure x
  AbsGrammar.StmtCall call -> failure x
  AbsGrammar.StmtSelect selstmt -> failure x
  AbsGrammar.StmtIter iterstmt -> failure x
  AbsGrammar.StmtReturn return -> failure x
  AbsGrammar.StmtBreak -> failure x
  AbsGrammar.StmtContinue -> failure x

transSelStmt :: (Show env, Show infType)  => AbsGrammar.SelStmt env infType -> Result
transSelStmt x = case x of
  AbsGrammar.StmtIf expr stmt -> failure x
  AbsGrammar.StmtIfElse expr stmt1 stmt2 -> failure x

transIterStmt :: (Show env, Show infType)  => AbsGrammar.IterStmt env infType -> Result
transIterStmt x = case x of
  AbsGrammar.StmtWhileDo expr stmt -> failure x
  AbsGrammar.StmtRepeat stmt expr -> failure x
  AbsGrammar.StmtFor ident initExpr fordirection expr stmt2 -> failure x

transForDirection :: AbsGrammar.ForDirection -> Result
transForDirection x = case x of
  AbsGrammar.ForDirection_to -> failure x
  AbsGrammar.ForDirection_downto -> failure x

transReturn :: Show infType => AbsGrammar.Return infType -> Result
transReturn x = case x of
  AbsGrammar.Ret expr -> failure x

transDclBlock :: (Show env, Show infType)  => AbsGrammar.DclBlock env infType -> Result
transDclBlock x = case x of
  AbsGrammar.DclBlockPcBlock pcblock -> failure x
  AbsGrammar.DclBlockVrBlock vrblock -> failure x
  AbsGrammar.DclBlockFcBlock fcblock -> failure x
  AbsGrammar.DclBlockCsBlock csblock -> failure x

transPcBlock :: (Show env, Show infType) => AbsGrammar.PcBlock env infType -> Result
transPcBlock x = case x of
  AbsGrammar.ProcBlock ident prms beblock -> failure x

transFcBlock :: (Show env, Show infType) => AbsGrammar.FcBlock env infType -> Result
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
  AbsGrammar.Modality_ref -> failure x
  AbsGrammar.Modality_val -> failure x

transCall :: Show infType => AbsGrammar.Call infType -> Result
transCall x = case x of
  AbsGrammar.CallArgs ident exprs -> failure x

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
  AbsGrammar.Array integer1 integer2 type_ -> failure x
  AbsGrammar.Pointer type_ -> failure x

transEXPR :: Show infType => AbsGrammar.EXPR infType -> Result
transEXPR x = case x of
  AbsGrammar.SelExpr expr1 expr2 expr3 infType -> failure x
  AbsGrammar.BinaryExpression _ rexpr1 rexpr2 infType -> failure x
  AbsGrammar.UnaryExpression _ rexpr1 infType -> failure x
  AbsGrammar.ExprLiteral literal -> failure x
  AbsGrammar.ExprCall call infType -> failure x
  AbsGrammar.BaseExpr bexpr infType -> failure x

transBEXPR :: Show infType => AbsGrammar.BEXPR infType -> Result
transBEXPR x = case x of
  AbsGrammar.ArrayElem expr1 expr2 -> failure x
  AbsGrammar.Identifier ident -> failure x

transLiteral :: AbsGrammar.Literal -> Result
transLiteral x = case x of
  AbsGrammar.LiteralInteger integer -> failure x
  AbsGrammar.LiteralString string -> failure x
  AbsGrammar.LiteralChar char -> failure x
  AbsGrammar.LiteralDouble double -> failure x
  AbsGrammar.LiteralBoolean boolean -> failure x
