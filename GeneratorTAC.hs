module GeneratorTAC where

import Control.Monad.Trans.State
import qualified AbsGrammar
import qualified Control.Monad.Except as AbsGrammar
import Env

-- TODO: implementare con Data.Sequence invece che con lista
type StateTAC = State (Int, [TACInst])

data TACLabel =
      FuncLab
    | Fall
    deriving (Show)

data TACOp =
          TACAdd
        | TACSub
        | TACDiv
        | TACMul
        | TACOr
        | TACAnd
        | TACNot
        | TACNeg
        | TACRef
        | TACDeref
    deriving (Show)

data TACInst =
    TACBinAss Addr Addr TACOp Addr
    | TACUnAss Addr TACOp Addr
    | TACNulAss Addr Addr
    | TACUncdJmp TACLabel
    | TACCndJmp Addr TACOp Addr TACLabel
    | TACIndxStr Addr Addr Addr
    | TACIndxLd Addr Addr Addr
    deriving (Show)

-- TODO : implementare correttamente; soluzione solo temporanea
getIdAddr :: String -> Env -> StateTAC Addr
getIdAddr id env = newIdAddr

newTmpAddr :: StateTAC Addr
newTmpAddr = do
    (k, ls)<-get;
    put (k+1, ls);
    return (int2TmpName k)

int2TmpName :: Int -> Addr
int2TmpName k = Temporary ("t" ++ show k)

-- TODO : implementare correttamente; soluzione solo temporanea
newIdAddr :: StateTAC Addr
newIdAddr = do
    (k, ls) <- get;
    put (k+1, ls);
    return (int2VarName k)

int2VarName :: Int -> Addr
int2VarName k = ProgVar ("v" ++ show k) 


genProg :: AbsGrammar.P Env AbsGrammar.Type-> AbsGrammar.P Env AbsGrammar.Type
genProg (AbsGrammar.Prog pBlock dclBlock bebBlock globEnv) = AbsGrammar.Prog pBlock dclBlock bebBlock globEnv

genExpr :: AbsGrammar.EXPR AbsGrammar.Type -> Env -> StateTAC Addr
genExpr expr env = case expr of
    (AbsGrammar.UnaryExpression _ _ _) -> genUnrExpr expr env
    (AbsGrammar.BinaryExpression _ _ _ _) -> genBinExpr expr env
    (AbsGrammar.ExprLiteral _) -> genLitExpr expr env
    (AbsGrammar.ExprCall _ _) -> error "TODO: Add call expr gen"
    (AbsGrammar.BaseExpr _ _) -> genBaseExpr expr env

-- TODO: valutare come usare tp(fare cast) ed env
genUnrExpr :: AbsGrammar.EXPR AbsGrammar.Type -> Env -> StateTAC Addr
genUnrExpr (AbsGrammar.UnaryExpression op exp1 tp) env = do
    tmpAddr <- newTmpAddr;
    exprAddr <- genExpr exp1 env;
    addInstr (TACUnAss tmpAddr (unrToTACOp op) exprAddr);
    return tmpAddr;
genUnrExpr _ _ = error "TODO: gestire errore genUnrExpr"

-- TODO: valutare come usare tp(fare cast) ed env
genBinExpr :: AbsGrammar.EXPR AbsGrammar.Type -> Env -> StateTAC Addr
genBinExpr (AbsGrammar.BinaryExpression op exp1 exp2 infType) env = do
    tmpAddr <- newTmpAddr;
    exprAddr1 <- genExpr exp1 env;
    exprAddr2 <- genExpr exp2 env;
    addInstr (TACBinAss tmpAddr exprAddr1 (binToTACOp op) exprAddr2);
    return tmpAddr;
genBinExpr _ _ = error "TODO: gestire errore genBinExpr"

genLitExpr :: AbsGrammar.EXPR AbsGrammar.Type -> Env -> StateTAC Addr
genLitExpr (AbsGrammar.ExprLiteral lit) env = return (TacLit lit)

genBaseExpr :: AbsGrammar.EXPR AbsGrammar.Type -> Env -> StateTAC Addr
genBaseExpr (AbsGrammar.BaseExpr bexpr tp) env = case bexpr of
    (AbsGrammar.Identifier (AbsGrammar.TokIdent (_, id))) -> do getIdAddr id env
    (AbsGrammar.ArrayElem _ _) -> error "TODO: implementare genBaseExpr per ArrayElem"

addInstr :: TACInst -> StateTAC ()
addInstr tacInst = do
    (tmpCount, tacInstrs) <- get;
    -- TODO: implementare con Data.Sequence invece che con lista
    put (tmpCount, tacInstrs ++ [tacInst])

binToTACOp :: AbsGrammar.BinaryOperator -> TACOp
binToTACOp opr = case opr of
    AbsGrammar.Add -> TACAdd
    AbsGrammar.Sub -> TACSub
    AbsGrammar.Div -> TACDiv
    AbsGrammar.Mul -> TACMul
    _ -> error "TODO: Add missing TAC Binary operators"

unrToTACOp :: AbsGrammar.UnaryOperator -> TACOp
unrToTACOp opr = case opr of
    AbsGrammar.Not -> TACNot
    AbsGrammar.Negation -> TACNeg
    AbsGrammar.Reference -> TACRef
    AbsGrammar.Dereference -> TACDeref

data Addr =
      ProgVar { var :: String }
    | TacLit { tacLit :: AbsGrammar.Literal }
    | Temporary { tempInt :: String }
    deriving (Show)