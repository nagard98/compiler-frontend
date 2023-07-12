module GeneratorTAC where

import Control.Monad.Trans.State
import qualified AbsGrammar
import Env
import HelperTAC
import qualified Data.Sequence as DS


genTAC :: AbsGrammar.P Env AbsGrammar.Type -> DS.Seq TACInst
genTAC prog = getInstrList (execState (genProg prog) (0, DS.empty, newStack ))
    where
        getInstrList :: (Int, DS.Seq TACInst, Stack (DS.Seq TACInst)) -> DS.Seq TACInst
        getInstrList (_, instrList, _) = instrList

genProg :: AbsGrammar.P Env AbsGrammar.Type -> StateTAC ()
genProg (AbsGrammar.Prog pBlock dclBlocks (AbsGrammar.BegEndBlock stmts scopeEnv) globEnv) = do
    genDcls dclBlocks globEnv
    genStmts stmts scopeEnv

genDcls :: [AbsGrammar.DclBlock Env AbsGrammar.Type] -> Env -> StateTAC ()
genDcls [] _ = return ()
genDcls (dcBlock:dcBlocks) env = do
    genDcl dcBlock env;
    genDcls dcBlocks env;
        where
            genDcl :: AbsGrammar.DclBlock Env AbsGrammar.Type -> Env -> StateTAC ()
            genDcl dclBlk env = case dclBlk of
                AbsGrammar.DclBlockCsBlock csBlock -> genCsDcl csBlock env
                AbsGrammar.DclBlockFcBlock fcBlock -> genFcDcl fcBlock env
                AbsGrammar.DclBlockPcBlock pcBlock -> genPcDcl pcBlock env
                AbsGrammar.DclBlockVrBlock vrBlock -> genVrDcl vrBlock env


genCsDcl :: AbsGrammar.CsBlock -> Env -> StateTAC ()
genCsDcl (AbsGrammar.ConstBlock csDefs) env = return ()
    --TODO: in realtà sembra non necessario in quanto abbiamo già costanti nel env, salvate come literal
    {-genCsDefs csDefs env
    where
        genCsDefs :: [AbsGrammar.CsDef] -> Env -> StateTAC ()
        genCsDefs [] _ = return ()
        genCsDefs ((AbsGrammar.ConstDefinition (AbsGrammar.IdElement (AbsGrammar.TokIdent (_, id))) lit):csDefs) env = do
            case Env.lookup id env of
                Just (Constant _ _ addr) -> addInstr (TACNulAss addr (TacLit lit))
                _ -> error "TODO: genVrDefIds -> id non esiste in env"
            genCsDefs csDefs env-}


genFcDcl :: AbsGrammar.FcBlock Env AbsGrammar.Type -> Env -> StateTAC ()
genFcDcl (AbsGrammar.FuncBlock (AbsGrammar.TokIdent (_,id)) prms tp (AbsGrammar.BegEndBlock stmts scopeEnv)) env = 
    case Env.lookup id env of
        Just (Function _ _ _ addr) -> do
            newStrm <- createNewStream
            -- TODO: implementa aggiunta label a prima istruzione stream
            pushStream newStrm
            attachLabelToNext (FuncLab addr)
            genStmts stmts scopeEnv
            --TODO: sembra che sia obbligatorio verificare in analisi semantica statica
            --che le funzioni abbiano un return
            closeCurrentStream 
        _ -> error "TODO: genFcDcl -> id funzione non esiste nell' env"

--TODO: controlla se funzionano bene parametri; ovvero addr sono quelli giusti
genPcDcl :: AbsGrammar.PcBlock Env AbsGrammar.Type -> Env -> StateTAC ()
genPcDcl (AbsGrammar.ProcBlock (AbsGrammar.TokIdent (_,id)) prms (AbsGrammar.BegEndBlock stmts scopeEnv)) env =
    case Env.lookup id env of
        Just (Procedure ps _ addr) -> do
            newStrm <- createNewStream
            -- TODO: implementa aggiunta label a prima istruzione stream
            pushStream newStrm
            attachLabelToNext (FuncLab addr)
            genStmts stmts scopeEnv
            addInstr TACReturnVoid
            closeCurrentStream 
        _ -> error "TODO: genPcDcl -> id procedura non esiste nell' env"


genVrDcl :: AbsGrammar.VrBlock -> Env -> StateTAC ()
genVrDcl (AbsGrammar.VarBlock vrDefs) env = do
    genVrDefs vrDefs env
    where
        genVrDefs :: [AbsGrammar.VrDef] -> Env -> StateTAC ()
        genVrDefs [] _ = return ()
        genVrDefs ((AbsGrammar.VarDefinition ids _):vrDefs) env = do
            genVrDefIds ids env
            genVrDefs vrDefs env
            where
                genVrDefIds :: [AbsGrammar.IdElem] -> Env -> StateTAC ()
                genVrDefIds [] _ = return ()
                genVrDefIds ((AbsGrammar.IdElement (AbsGrammar.TokIdent (_,id))):ids) env = do
                    case Env.lookup id env of
                        Just (VarType _ _ tp addr) -> addInstr (TACNulAss addr (getVarDefaultVal tp))
                        _ -> error "TODO: genVrDefIds -> id non esiste in env"
                    genVrDefIds ids env
                


genStmts :: [AbsGrammar.Stmt Env AbsGrammar.Type] -> Env -> StateTAC ()
genStmts [] _ = return ()
genStmts (stmt:stmts) env = do
    genStmt stmt env;
    genStmts stmts env;
        where
            genStmt :: AbsGrammar.Stmt Env AbsGrammar.Type -> Env -> StateTAC ()
            genStmt stmt env = case stmt of
                AbsGrammar.StmtAssign _ _ -> genStmtAssign stmt env
                AbsGrammar.StmtDecl _ -> genStmtDecl stmt env
                AbsGrammar.StmtComp _ -> genStmtComp stmt env
                AbsGrammar.StmtCall _ -> genStmtCall stmt env
                AbsGrammar.StmtSelect _ -> genStmtSelect stmt env
                AbsGrammar.StmtIter _ -> genStmtIter stmt env
                AbsGrammar.StmtReturn _ -> genStmtReturn stmt env

--TODO: gestisci caso assegnamento array
genStmtAssign :: AbsGrammar.Stmt Env AbsGrammar.Type -> Env -> StateTAC ()
genStmtAssign (AbsGrammar.StmtAssign lexpr rexpr) env = case lexpr of
    --TODO: potrebbe essere utile passare giù il tipo; credo di no
    AbsGrammar.BaseExpr arr@(AbsGrammar.ArrayElem _ _) tp -> do
        (baseAddr, offset, _) <- genArrayExpr arr env
        exprAddr <- genExpr rexpr env;
        addInstr (TACIndxStr baseAddr offset exprAddr)
    _ -> do
        varAddr <- genBaseExpr lexpr env;
        exprAddr <- genExpr rexpr env;
        addInstr (TACNulAss varAddr exprAddr)
genStmtAssign _ _ = error "TODO: gestire errore genStmtAssign"

genStmtDecl :: AbsGrammar.Stmt Env AbsGrammar.Type -> Env -> StateTAC ()
genStmtDecl (AbsGrammar.StmtDecl dclBlock) env = genDcls [dclBlock] env
genStmtDecl _ _ = error "TODO: gestire errore genStmtDecl"

genStmtComp :: AbsGrammar.Stmt Env AbsGrammar.Type -> Env -> StateTAC ()
genStmtComp (AbsGrammar.StmtComp (AbsGrammar.BegEndBlock stmts envScope)) env = genStmts stmts envScope
genStmtComp _ _ = error "TODO: gestire errore genStmtComp"

genStmtCall :: AbsGrammar.Stmt Env AbsGrammar.Type -> Env -> StateTAC ()
genStmtCall (AbsGrammar.StmtCall (AbsGrammar.CallArgs (AbsGrammar.TokIdent (_,callId)) args)) env = 
    case Env.lookup callId env of
        Just (Procedure pos prms addr) -> do
            genArgs args env
            addInstr (TACPCall addr (length args))
        _ -> error "TODO:: genStmtCall -> errore, non esiste procedura con questo nome"

genStmtSelect :: AbsGrammar.Stmt Env AbsGrammar.Type -> Env -> StateTAC ()
genStmtSelect (AbsGrammar.StmtSelect selStmt) env = case selStmt of
    AbsGrammar.StmtIf guard stmt -> do
        nextStmtLabel <- newLabel
        genGuard guard Fall nextStmtLabel env
        --TODO: problema di passare env corretto; se è solo statement e non BEBlock,
        --come faccio ad avere l'env locale all'interno dell'if
        --Fare in modo che durante analisi semantica statica all'interno degli if-else, se c'è un unico stmt che
        --non è un blocco BeginEnd, questo venga inserito facendo da wrapper allo statement
        genStmts [stmt] env
        attachLabelToNext nextStmtLabel

    AbsGrammar.StmtIfElse guard tStmt fStmt -> do
        nextStmtLabel <- newLabel
        elseStmtLabel <- newLabel
        genGuard guard Fall elseStmtLabel env
        genStmts [tStmt] env
        addInstr (TACUncdJmp nextStmtLabel)
        genStmts [fStmt] env
        attachLabelToNext nextStmtLabel


genStmtSelect _ _ = error "TODO: gestire errore genStmtSelect"


genStmtIter :: AbsGrammar.Stmt Env AbsGrammar.Type -> Env -> StateTAC ()
genStmtIter (AbsGrammar.StmtIter iterStmt) env = do

    inStmtLabel <- newLabel

    case iterStmt of
        AbsGrammar.StmtWhileDo guard stmt -> do
            guardLabel <- newLabel
            addInstr (TACUncdJmp guardLabel)
            attachLabelToNext inStmtLabel
            genStmts [stmt] env
            attachLabelToNext guardLabel
            genGuard guard inStmtLabel Fall env

        AbsGrammar.StmtRepeat stmt guard -> do
            attachLabelToNext inStmtLabel
            genStmts [stmt] env
            genGuard guard inStmtLabel Fall env

genStmtIter _ _ =  error "TODO: gestire errore genStmtIter"


--TODO: determinare con quale ordine devo fare la valutazione delle guardie
genGuard :: AbsGrammar.EXPR AbsGrammar.Type -> TACLabel -> TACLabel -> Env -> StateTAC ()
genGuard guard trueLab falseLab env = case guard of

    (AbsGrammar.BinaryExpression opr b1@expr1 b2@expr2 _) -> case opr of

        AbsGrammar.And -> do
            b1False <- case falseLab of
                        Fall -> newLabel
                        _ -> return falseLab

            genGuard b1 Fall b1False env
            genGuard b2 trueLab falseLab env
            case falseLab of
                Fall -> attachLabelToNext b1False
                _ -> return ()

        AbsGrammar.Or -> do
            b1True <- case trueLab of
                        Fall -> newLabel
                        _ -> return trueLab

            genGuard b1 b1True Fall env
            genGuard b2 trueLab falseLab env
            case trueLab of
                Fall -> attachLabelToNext b1True
                _ -> return ()

        _ -> if isRelOp opr
                then do
                    e1Addr <- genExpr expr1 env
                    e2Addr <- genExpr expr2 env
                    case (trueLab, falseLab) of
                        (Fall, Fall) -> return ()
                        (_, Fall) -> addInstr (TACCndJmp e1Addr (binToTACOp opr) e2Addr trueLab)
                        (Fall, _) -> addInstr (TACCndJmp e1Addr (notRel opr) e2Addr falseLab)
                        _ -> do
                            addInstr (TACCndJmp e1Addr (binToTACOp opr) e2Addr trueLab)
                            addInstr (TACUncdJmp falseLab)
                            
                else error "TODO: genGuard -> operatore non è ne logico ne relazionale"

    (AbsGrammar.UnaryExpression opr b1@expr tp) -> case opr of
        AbsGrammar.Not -> genGuard b1 falseLab trueLab env

    (AbsGrammar.ExprLiteral (AbsGrammar.LiteralBoolean (AbsGrammar.TokBoolean (_, value)))) -> case value of
        "true" -> addInstr (TACUncdJmp trueLab)
        "false" -> addInstr (TACUncdJmp falseLab)
        _ -> error "TODO: genGuard -> valore literal non è ne true ne false"

    _ -> error "TODO: genGuard -> espressione guardia non è compatibile"

    where
        isRelOp :: AbsGrammar.BinaryOperator -> Bool
        isRelOp opr = case opr of
            AbsGrammar.LessT -> True
            AbsGrammar.GreatT -> True
            AbsGrammar.EqLessT -> True
            AbsGrammar.EqGreatT -> True
            AbsGrammar.NotEq -> True
            AbsGrammar.Eq -> True
            _ -> False


genStmtReturn :: AbsGrammar.Stmt Env AbsGrammar.Type -> Env -> StateTAC ()
genStmtReturn (AbsGrammar.StmtReturn (AbsGrammar.Ret expr)) env = do
    exprAddr <- genExpr expr env
    addInstr (TACReturn exprAddr)

genExpr :: AbsGrammar.EXPR AbsGrammar.Type -> Env -> StateTAC Addr
genExpr expr env = case expr of
    (AbsGrammar.UnaryExpression _ _ _) -> genUnrExpr expr env
    (AbsGrammar.BinaryExpression _ _ _ _) -> genBinExpr expr env
    (AbsGrammar.ExprLiteral _) -> genLitExpr expr env
    (AbsGrammar.ExprCall _ _) -> genExprCall expr env
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

genExprCall :: AbsGrammar.EXPR AbsGrammar.Type -> Env -> StateTAC Addr
genExprCall (AbsGrammar.ExprCall (AbsGrammar.CallArgs (AbsGrammar.TokIdent (_,callId)) args) tp) env = 
    case Env.lookup callId env of
        Just (Env.Function _ _ retType fAddr) -> do
            --TODO : valutare se necessario fare cast qui; forse non serve considerando
            -- che genExpr fa dei cast
            genArgs args env
            tmpAddr <- newTmpAddr
            addInstr (TACFCall tmpAddr fAddr (length args))
            return tmpAddr
        _ -> error "TODO : errore in genExprCall; funzione con questo nome non esiste"

genArgs :: [AbsGrammar.EXPR AbsGrammar.Type] -> Env -> StateTAC ()
genArgs (arg:args) env = do
    tmpArgAddr <- genExpr arg env
    addInstr (TACParam tmpArgAddr)
    genArgs args env

genArgs [] env = return ()

genBaseExpr :: AbsGrammar.EXPR AbsGrammar.Type -> Env -> StateTAC Addr
genBaseExpr (AbsGrammar.BaseExpr bexpr tp) env = case bexpr of
    
    (AbsGrammar.Identifier (AbsGrammar.TokIdent (_, id))) -> do 
        getIdAddr id env
    
    arr@(AbsGrammar.ArrayElem _ _) -> do
        (baseAddr, offset, _) <- genArrayExpr arr env
        elemAddr <- newTmpAddr
        addInstr (TACIndxLd elemAddr baseAddr offset)
        return elemAddr

genBaseExpr _ _ = error "TODO: genBaseExpr -> gestire altri casi mancanti"

genArrayExpr :: AbsGrammar.BEXPR AbsGrammar.Type -> Env -> StateTAC (Addr, Addr, AbsGrammar.Type)
genArrayExpr (AbsGrammar.ArrayElem (AbsGrammar.Identifier (AbsGrammar.TokIdent (_,id))) indexExpr) env = 
    case Env.lookup id env of
    
        Just (VarType _ _ tp baseAddr) -> do
            offset <- newTmpAddr
            indexAddr <- genExpr indexExpr env
            sizeAddr <- genLitExpr (convertIntToExpr (sizeof tp)) env
            addInstr (TACBinAss offset indexAddr TACMul sizeAddr)
            return (baseAddr, offset, tp)
    
        _ -> error "TODO: genArrayExpr -> id non esiste nel env"

genArrayExpr (AbsGrammar.ArrayElem arr@(AbsGrammar.ArrayElem _ _) indexExpr) env = do
    (baseAddr, lastOffset, lastArrType) <- genArrayExpr arr env
    tp <- case lastArrType of
        AbsGrammar.TypeCompType (AbsGrammar.Array _ _ nextArrType) -> return nextArrType
        _ -> error "TODO: genArrayExpr -> verifica cosa serve con altri tipi"
    tmp <- newTmpAddr
    offset <- newTmpAddr
    indexAddr <- genExpr indexExpr env
    --TODO: valutare se possibile scriverlo meglio
    sizeAddr <- genLitExpr (convertIntToExpr (sizeof tp)) env
    addInstr (TACBinAss tmp indexAddr TACMul sizeAddr)
    addInstr (TACBinAss offset tmp TACAdd lastOffset)
    return (baseAddr, offset, tp)

genArrayExpr _ _ = error "TODO: genArrayExpr -> non è un array"

    

addInstr :: TACInst -> StateTAC ()
addInstr tacInst = do
    (tmpCount, tacInstrs, stackStrms) <- get;
    if null stackStrms
        then put (tmpCount, tacInstrs DS.|> tacInst, stackStrms)
        else do
            (strm, stack) <- pop stackStrms
            newStack <- push (strm DS.|> tacInst) stack 
            put (tmpCount, tacInstrs, newStack)


attachLabelToNext :: TACLabel -> StateTAC ()
attachLabelToNext label = addInstr (LabelNext label)
