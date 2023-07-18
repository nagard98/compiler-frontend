-- File generated by the BNF Converter (bnfc 2.9.4.1).

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Use fewer imports" #-}
{-# HLINT ignore "Redundant bracket" #-}

-- | The abstract syntax of language grammar.

module AbsGrammar where

import Prelude (Char, Double, Integer, Int, String)
import qualified Prelude as C (Eq, Ord, Show, Read)
import Prelude (Show (show),(++), (==), (&&), Bool)
import qualified Data.String
import Data.Array (Array)

type Position = (Int, Int)

data TokWrap = TokId TokIdent | TokC TokChar | TokI TokInteger | TokB TokBoolean | TokS TokString | TokD TokDouble

-- New types that store token position
newtype TokIdent = TokIdent (Position, String)
  deriving (C.Eq, C.Ord, C.Show, C.Read)
newtype TokChar = TokChar (Position, String)
  deriving (C.Eq, C.Ord, C.Show, C.Read)
newtype TokDouble = TokDouble (Position, String)
  deriving (C.Eq, C.Ord, C.Show, C.Read)
newtype TokInteger = TokInteger (Position, String)
  deriving (C.Eq, C.Ord, C.Show, C.Read)
newtype TokString = TokString (Position, String)
  deriving (C.Eq, C.Ord, C.Show, C.Read)
newtype TokBoolean = TokBoolean (Position, String)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data P env infType = Prog PBlock [DclBlock env infType] (BEBlock env infType) env
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data PBlock = ProgBlock TokIdent
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data BEBlock env infType = BegEndBlock [Stmt env infType] env
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Stmt env infType
    = StmtDecl (DclBlock env infType)
    | StmtComp (BEBlock env infType)
    | StmtAssign (EXPR infType) (EXPR infType)
    | StmtCall (Call infType)
    | StmtSelect (SelStmt env infType)
    | StmtIter (IterStmt env infType)
    | StmtReturn (Return infType)
    | StmtBreak
    | StmtContinue
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data SelStmt env infType = StmtIf (EXPR infType) (Stmt env infType) | StmtIfElse (EXPR infType) (Stmt env infType) (Stmt env infType)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data IterStmt env infType =
      StmtWhileDo (EXPR infType) (Stmt env infType)
    | StmtRepeat (Stmt env infType) (EXPR infType)
    | StmtFor (EXPR infType) (EXPR infType) ForDirection (EXPR infType) (Stmt env infType)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data ForDirection = ForDirection_to | ForDirection_downto
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Return infType = Ret (EXPR infType)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data DclBlock env infType
    = DclBlockPcBlock (PcBlock env infType)
    | DclBlockVrBlock VrBlock
    | DclBlockFcBlock (FcBlock env infType)
    | DclBlockCsBlock CsBlock
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data PcBlock env infType = ProcBlock TokIdent Prms (BEBlock env infType)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data FcBlock env infType = FuncBlock TokIdent Prms Type (BEBlock env infType)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Prms = Params [Prm] | NoParams
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Prm = Param Modality [IdElem] Type
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Modality = Modality_ref | Modality_val
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Call infType = CallArgs TokIdent [EXPR infType]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data VrBlock = VarBlock [VrDef]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data VrDef = VarDefinition [IdElem] Type
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data CsBlock = ConstBlock [CsDef]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data CsDef = ConstDefinition IdElem Literal
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data IdElem = IdElement TokIdent
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Type = TypeBaseType BaseType | TypeCompType CompType
  deriving (C.Eq, C.Ord, C.Read)

data BaseType
    = BaseType_integer
    | BaseType_boolean
    | BaseType_real
    | BaseType_char
    | BaseType_string
    | BaseType_error
    | BaseType_void
  deriving (C.Eq, C.Ord, C.Read)

data CompType = Array TokInteger TokInteger Type | Pointer Type
  deriving (C.Ord, C.Read)

data EXPR infType =
      UnaryExpression {operator1 :: UnaryOperator, exp :: EXPR infType, tp :: infType}
    | BinaryExpression {operator2 :: BinaryOperator, exp1, exp2 :: EXPR infType, tp :: infType }
    | ExprLiteral Literal
    | SelExpr (EXPR infType) (EXPR infType) (EXPR infType) infType
    | ExprCall (Call infType) infType
    | BaseExpr (BEXPR infType) infType
    | IntToReal (EXPR infType)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data BinaryOperator = Or | And | Eq | NotEq | LessT | EqLessT | GreatT | EqGreatT | Sub | Add |
                    Div | Mul | Mod deriving (C.Eq, C.Ord, C.Show, C.Read)

data UnaryOperator = Not | Negation | Reference | Dereference deriving (C.Eq, C.Ord, C.Show, C.Read)


data BEXPR infType = ArrayElem (EXPR infType) (EXPR infType) | Identifier TokIdent
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Literal
    = LiteralInteger TokInteger
    | LiteralString TokString
    | LiteralChar TokChar
    | LiteralDouble TokDouble
    | LiteralBoolean TokBoolean
  deriving (C.Eq, C.Ord, C.Show, C.Read)

instance C.Show Type where
    show (TypeCompType comptype) = show comptype
    show (TypeBaseType basetype) = show basetype

instance C.Show CompType where
    show cpTp@(Array (TokInteger (_,i1)) (TokInteger (_,i2)) t) = "Array ["++i1++".."++i2++ "] of type " ++ show t
    show cpTp@(Pointer basetype) =  "Pointer of " ++ show basetype

instance C.Show BaseType where
    show (BaseType_integer) = "Integer"
    show (BaseType_boolean) = "Bool"
    show (BaseType_real) = "Real"
    show (BaseType_char) = "Char"
    show (BaseType_string) = "String"
    show (BaseType_error) = "Error"
    show (BaseType_void) = "Void"

instance C.Eq CompType where
  (==) lArr@(Array {}) rArr@(Array {}) = compArrType lArr rArr
  (==) (Pointer lTp) (Pointer rTp) = lTp == rTp

compArrType :: CompType -> CompType -> Bool
compArrType (Array (TokInteger (_, lB)) (TokInteger (_, lE)) lTp) (Array (TokInteger (_, rB)) (TokInteger (_, rE)) rTp) =
   lB == rB && lE == rE && lTp == rTp

class GetTokPos tok where
  getTokPos :: tok -> Position

instance GetTokPos TokIdent where
  getTokPos (TokIdent (pos,_)) = pos

instance GetTokPos TokBoolean where
  getTokPos (TokBoolean (pos,_)) = pos

instance GetTokPos TokChar where
  getTokPos (TokChar (pos,_)) = pos

instance GetTokPos TokDouble where
  getTokPos (TokDouble (pos,_)) = pos

instance GetTokPos TokInteger where
  getTokPos (TokInteger (pos,_)) = pos

instance GetTokPos TokString where
  getTokPos (TokString (pos,_)) = pos

showExpr :: EXPR t -> String
showExpr (UnaryExpression op exp t) = showUnaryExpr op exp
showExpr (BinaryExpression op exp1 exp2 t) = showBinaryExpr op exp1 exp2
showExpr (ExprLiteral (literal)) = showLiteral literal
showExpr (ExprCall call t) = showCall call
showExpr (BaseExpr bexpr t) = showBaseExpr bexpr
showExpr (IntToReal exp) = "Real(" ++ showExpr exp ++ ")"

showUnaryExpr :: UnaryOperator -> EXPR infType -> String
showUnaryExpr Not exp = "not " ++ showExpr exp
showUnaryExpr Negation exp = "-" ++ showExpr exp
showUnaryExpr Reference exp = "@" ++ showExpr exp
showUnaryExpr Dereference exp = showExpr exp ++ "^"

showBinaryExpr :: BinaryOperator -> EXPR infTYpe -> EXPR infType -> String
showBinaryExpr Or exp1 exp2 = showExpr exp1 ++ "or" ++ showExpr exp2
showBinaryExpr And exp1 exp2 = showExpr exp1 ++ "and" ++ showExpr exp2
showBinaryExpr Eq exp1 exp2 = showExpr exp1 ++ "=" ++ showExpr exp2
showBinaryExpr NotEq exp1 exp2 = showExpr exp1 ++ "<>" ++ showExpr exp2
showBinaryExpr LessT exp1 exp2 = showExpr exp1 ++ "<" ++ showExpr exp2
showBinaryExpr EqLessT exp1 exp2 = showExpr exp1 ++ "<=" ++ showExpr exp2
showBinaryExpr GreatT exp1 exp2 = showExpr exp1 ++ ">" ++ showExpr exp2
showBinaryExpr EqGreatT exp1 exp2 = showExpr exp1 ++ ">=" ++ showExpr exp2
showBinaryExpr Sub exp1 exp2 = showExpr exp1 ++ "-" ++ showExpr exp2
showBinaryExpr Add exp1 exp2 = showExpr exp1 ++ "+" ++ showExpr exp2
showBinaryExpr Div exp1 exp2 = showExpr exp1 ++ "/" ++ showExpr exp2
showBinaryExpr Mul exp1 exp2 = showExpr exp1 ++ "*" ++ showExpr exp2
showBinaryExpr Mod exp1 exp2 = showExpr exp1 ++ "mod" ++ showExpr exp2

showLiteral :: Literal -> String
showLiteral (LiteralInteger (TokInteger (tokpos,tokid))) = tokid
showLiteral (LiteralString (TokString (tokpos,tokid))) = tokid
showLiteral (LiteralChar (TokChar (tokpos,tokid))) = tokid
showLiteral (LiteralDouble (TokDouble (tokpos,tokid))) = tokid
showLiteral (LiteralBoolean (TokBoolean (tokpos,tokid))) = tokid

showCall :: Call t -> String
showCall (CallArgs (TokIdent (tokpos, tokid)) exprs) = tokid++"("++ showExprs exprs ++")"
    where
        showExprs :: [EXPR t] -> String
        showExprs [] = ""
        showExprs [expr] = showExpr expr
        showExprs (expr:exprs) = showExpr expr ++ "," ++ showExprs exprs

showBaseExpr :: BEXPR t -> String
showBaseExpr (ArrayElem idexp iexp) = showExpr idexp ++ "["++ showExpr iexp ++"]"
showBaseExpr (Identifier (TokIdent (tokpos, tokid))) = tokid

