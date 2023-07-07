module GeneratorTAC where

import Control.Monad.Trans.State
import qualified AbsGrammar
import qualified Control.Monad.Except as AbsGrammar
import Env
import qualified Data.Sequence as DS

type StateTAC = State (Int, DS.Seq TACInst)

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

genTAC :: AbsGrammar.P Env AbsGrammar.Type -> DS.Seq TACInst
genTAC prog = getInstrList (execState (genProg prog) (0, DS.empty))
    where
        getInstrList :: (Int, DS.Seq TACInst) -> DS.Seq TACInst
        getInstrList (_, instrList) = instrList

genProg :: AbsGrammar.P Env AbsGrammar.Type -> StateTAC ()
genProg (AbsGrammar.Prog pBlock dclBlocks (AbsGrammar.BegEndBlock stmts scopeEnv) globEnv) = do
    --genDcls dclBlock globEnv
    genStmts stmts scopeEnv

genDcls :: [AbsGrammar.DclBlock Env AbsGrammar.Type] -> Env -> StateTAC ()
genDcls (dcBlock:dcBlocks) env = do
    genDcl dcBlock env;
    genDcls dcBlocks env;
        where
            genDcl :: AbsGrammar.DclBlock Env AbsGrammar.Type -> Env -> StateTAC ()
            genDcl dclBlk env = case dclBlk of
                AbsGrammar.DclBlockCsBlock csBlock -> error "TODO: implementare genCsBlock"
                AbsGrammar.DclBlockFcBlock fcBlock -> error "TODO: implementare genFcBlock"
                AbsGrammar.DclBlockPcBlock pcBlock -> error "TODO: implementare genPcBlock"
                AbsGrammar.DclBlockVrBlock vrBlock -> error "TODO: implementare genVrBlock"


genStmts :: [AbsGrammar.Stmt Env AbsGrammar.Type] -> Env -> StateTAC ()
genStmts (stmt:stmts) env = do
    genStmt stmt env;
    genStmts stmts env;
        where
            genStmt :: AbsGrammar.Stmt Env AbsGrammar.Type -> Env -> StateTAC ()
            genStmt stmt env = case stmt of
                AbsGrammar.StmtAssign _ _ -> genStmtAssign stmt env
                AbsGrammar.StmtDecl _ -> error "TODO: implementare genStmtDecl"
                AbsGrammar.StmtComp _ -> genStmtComp stmt env
                AbsGrammar.StmtCall _ -> error "TODO: implementare genStmtCall"
                AbsGrammar.StmtSelect _ -> error "TODO: implementare genStmtSelect"
                AbsGrammar.StmtIter _ -> error "TODO: implementare genStmtIter"
                AbsGrammar.StmtReturn _ -> error "TODO: implementare genStmtReturn"

genStmts [] env = do return ();


genStmtAssign :: AbsGrammar.Stmt Env AbsGrammar.Type -> Env -> StateTAC ()
genStmtAssign (AbsGrammar.StmtAssign lexpr rexpr) env = do
    varAddr <- genBaseExpr lexpr env;
    exprAddr <- genExpr rexpr env;
    addInstr (TACNulAss varAddr exprAddr)
genStmtAssign _ _ = error "TODO: gestire errore genStmtAssign"

genStmtComp :: AbsGrammar.Stmt Env AbsGrammar.Type -> Env -> StateTAC ()
genStmtComp (AbsGrammar.StmtComp (AbsGrammar.BegEndBlock stmts envScope)) env = genStmts stmts envScope
genStmtComp _ _ = error "TODO: gestire errore genStmtComp"


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
    put (tmpCount, tacInstrs DS.|> tacInst)

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