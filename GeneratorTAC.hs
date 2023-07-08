module GeneratorTAC where

import Control.Monad.Trans.State
import qualified AbsGrammar
import qualified Control.Monad.Except as AbsGrammar
import Env
import HelperTAC
import qualified Data.Sequence as DS


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
    (AbsGrammar.ExprCall _ _) -> error "TODO: implementare genExprCall"
    (AbsGrammar.BaseExpr _ _) -> genBaseExpr expr env

-- TODO: valutare come usare tp(fare cast) ed env
genUnrExpr :: AbsGrammar.EXPR AbsGrammar.Type -> Env -> StateTAC Addr
genUnrExpr (AbsGrammar.UnaryExpression op exp1 tp) env = do
    tmpAddr <- newTmpAddr;
    exprAddr <- genExpr exp1 env;
    exprType <- getExprType exp1;

    exprAddr <- castIfNecessary exprAddr exprType tp;

    addInstr (TACUnAss tmpAddr (unrToTACOp op) exprAddr);
    return tmpAddr;
genUnrExpr _ _ = error "TODO: gestire errore genUnrExpr"

-- TODO: valutare come usare tp(fare cast) ed env
genBinExpr :: AbsGrammar.EXPR AbsGrammar.Type -> Env -> StateTAC Addr
genBinExpr (AbsGrammar.BinaryExpression op exp1 exp2 infType) env = do
    tmpAddr <- newTmpAddr;
    exprAddr1 <- genExpr exp1 env;
    expr1Type <- getExprType exp1;
    exprAddr2 <- genExpr exp2 env;
    expr2Type <- getExprType exp2;

    exprAddr1 <- castIfNecessary exprAddr1 expr1Type infType;
    exprAddr2 <- castIfNecessary exprAddr2 expr2Type infType;

    addInstr (TACBinAss tmpAddr exprAddr1 (binToTACOp op) exprAddr2);
    return tmpAddr;
genBinExpr _ _ = error "TODO: gestire errore genBinExpr"

castIfNecessary :: Addr -> AbsGrammar.Type -> AbsGrammar.Type -> StateTAC Addr
castIfNecessary exprAddr exprType castType = do
    if exprType == castType 
        then return exprAddr;
        else do 
            tmpAddr <- newTmpAddr;
            addInstr (TACUnAss tmpAddr (TACCast castType) exprAddr)
            return tmpAddr;


getExprType :: AbsGrammar.EXPR AbsGrammar.Type -> StateTAC AbsGrammar.Type
getExprType expr = case expr of
    AbsGrammar.UnaryExpression _ _ tp -> return tp;
    AbsGrammar.BinaryExpression _ _ _ tp -> return tp;
    AbsGrammar.ExprCall _ tp -> return tp;
    AbsGrammar.BaseExpr _ tp -> return tp;
    AbsGrammar.ExprLiteral lit -> return (AbsGrammar.TypeBaseType (Env.getTypeFromLiteral lit));


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


