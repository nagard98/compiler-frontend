module GeneratorTAC where

import Control.Monad.Trans.State.Strict
import qualified AbsGrammar
import Env
import HelperTAC
import qualified Data.Sequence as DS
import System.Process (CreateProcess(env))
import Data.IntMap (size)
import Debug.Trace (traceM)


genTAC :: AbsGrammar.P Env AbsGrammar.Type -> TACQuadSeq
genTAC prog = getInstrList (execState (genProg prog) newTACState)
    where
        getInstrList :: TACStateStruct -> TACQuadSeq
        getInstrList state = quads state

        newTACState = TACStateStruct {
            quads = TACQuadSeq DS.empty,
            stackStrms = newStack,
            tmpCount = 0,
            labCount = 0,
            labelsNextInstr = AddrList []
        }

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
                        Just (Variable mod _ tp addr) -> addInstr (TACNulAss addr (getVarDefaultVal tp))
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

                AbsGrammar.StmtBreak -> do
                    case Env.lookup "break" env of
                        Just (InsideLoop label) -> addInstr (TACUncdJmp label)

                AbsGrammar.StmtContinue -> 
                    case Env.lookup "continue" env of
                        Just (InsideLoop label) -> addInstr (TACUncdJmp label)

--TODO: gestisci caso assegnamento array
genStmtAssign :: AbsGrammar.Stmt Env AbsGrammar.Type -> Env -> StateTAC ()
genStmtAssign (AbsGrammar.StmtAssign lexpr rexpr) env = do

    lXAddr <- genExpr lexpr True env
    rXAddr <- genExpr rexpr False env
    tmpAddr <- newTmpAddr

    case (lXAddr, rXAddr) of
        (Addr lAddr, Addr rAddr) ->
            addInstr (TACNulAss lAddr rAddr)

        (ArrayAddr lBase lOffset, ArrayAddr rBase rOffset) -> do
            addInstr (TACIndxLd tmpAddr rBase rOffset)
            addInstr (TACIndxStr lBase lOffset tmpAddr)

        --TODO: è giusto così?
        (RefAddr lRAddr, RefAddr rRAddr) -> do
            addInstr (TACNulAss lRAddr rRAddr)

        (ArrayAddr base offset, Addr rAddr) ->
            addInstr (TACIndxStr base offset rAddr)

        (Addr lAddr, ArrayAddr base offset) ->
            addInstr (TACIndxLd lAddr base offset)

        (RefAddr lRefAddr, Addr rAddr) ->
            addInstr (TACDerefAss lRefAddr rAddr)

        (Addr lAddr, RefAddr rRefAddr) ->
            addInstr (TACAssDeref lAddr rRefAddr)

        --TODO: valutare se sono fatti bene questi casi; Assegna valore di un array a reference?
        (RefAddr lRefAddr, ArrayAddr base offset) -> do
            addInstr (TACIndxLd tmpAddr base offset)
            addInstr (TACDerefAss lRefAddr tmpAddr)

        (ArrayAddr base offset, RefAddr rRefAddr) -> do
            addInstr (TACAssDeref tmpAddr rRefAddr)
            addInstr (TACIndxStr base offset tmpAddr)


    {-case lexpr of
    --TODO: potrebbe essere utile passare giù il tipo; credo di no
    {-AbsGrammar.BaseExpr arr@(AbsGrammar.ArrayElem _ _) tp -> do
        (baseAddr, offset, _) <- genArrayExpr arr env
        exprAddr <- genExpr rexpr env;
        addInstr (TACIndxStr baseAddr offset exprAddr)-}
    AbsGrammar.UnaryExpression AbsGrammar.Dereference assExpr tp -> do
        --TODO: sostituire genExpr per lexpr con funzione adatta
        derefExpr <- genExpr lexpr env
        exprAddr <- genExpr assExpr env 
        addInstr (TACDerefAss derefExpr exprAddr)
        return()

    _ -> do
        varAddr <- genBaseExpr lexpr env;
        exprAddr <- genExpr rexpr env;
        addInstr (TACNulAss varAddr exprAddr)-}

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
            genArgs args prms env
            addInstr (TACPCall addr (length args))
        _ -> error "TODO:: genStmtCall -> errore, non esiste procedura con questo nome"

genStmtSelect :: AbsGrammar.Stmt Env AbsGrammar.Type -> Env -> StateTAC ()
genStmtSelect (AbsGrammar.StmtSelect selStmt) env = case selStmt of
    AbsGrammar.StmtIf guard stmt -> do
        nextStmtLabel <- newLabel
        genGuard guard Fall nextStmtLabel env
        genStmts [stmt] env
        attachLabelToNext nextStmtLabel

    AbsGrammar.StmtIfElse guard tStmt fStmt -> do
        nextStmtLabel <- newLabel
        elseStmtLabel <- newLabel
        genGuard guard Fall elseStmtLabel env
        genStmts [tStmt] env
        addInstr (TACUncdJmp nextStmtLabel)
        attachLabelToNext elseStmtLabel
        genStmts [fStmt] env
        attachLabelToNext nextStmtLabel

    
genStmtSelect _ _ = error "TODO: gestire errore genStmtSelect"


genStmtIter :: AbsGrammar.Stmt Env AbsGrammar.Type -> Env -> StateTAC ()
genStmtIter (AbsGrammar.StmtIter iterStmt) env = do

    scopeEnv <- getIterScopeEnv iterStmt

    inStmtLabel <- newLabel
    guardLabel <- newLabel

    breakLabel <- case Env.lookup "break" scopeEnv of
        Just (InsideLoop label) -> return label
        Nothing -> newLabel

    continueLabel <- case Env.lookup "continue" scopeEnv of
        Just (InsideLoop label) -> return label
        Nothing -> newLabel


    case iterStmt of
        AbsGrammar.StmtWhileDo guard (AbsGrammar.StmtComp (AbsGrammar.BegEndBlock stmts _)) -> do
            addInstr (TACUncdJmp guardLabel)
            attachLabelToNext inStmtLabel
            genStmts stmts scopeEnv                  

            attachLabelToNext guardLabel
            attachLabelToNext continueLabel
            genGuard guard inStmtLabel Fall scopeEnv

            attachLabelToNext breakLabel

        AbsGrammar.StmtRepeat (AbsGrammar.StmtComp (AbsGrammar.BegEndBlock stmts _))  guard -> do
            attachLabelToNext inStmtLabel
            genStmts stmts scopeEnv

            attachLabelToNext guardLabel
            attachLabelToNext continueLabel
            genGuard guard inStmtLabel Fall scopeEnv

            attachLabelToNext breakLabel


        AbsGrammar.StmtFor condVar@(AbsGrammar.BaseExpr (AbsGrammar.Identifier (AbsGrammar.TokIdent (_, id))) tp) initExpr forDirection limitExpr (AbsGrammar.StmtComp (AbsGrammar.BegEndBlock stmts scopeEnv))  -> do
            genStmtAssign (AbsGrammar.StmtAssign condVar initExpr) scopeEnv
            guardLabel <- newLabel
            tmpAddr <- newTmpAddr
            addInstr (TACUncdJmp guardLabel)
            attachLabelToNext inStmtLabel
            addInstr (TACBinAss tmpAddr addr (forDirToArithmOp forDirection) (TacLit (TACIntLit 1)))
            addInstr (TACNulAss addr tmpAddr)
            genStmts stmts scopeEnv
            attachLabelToNext guardLabel
            genGuard (AbsGrammar.BinaryExpression (forDirToRelOp forDirection) condVar limitExpr (AbsGrammar.TypeBaseType AbsGrammar.BaseType_boolean)) inStmtLabel Fall scopeEnv
            where
                forDirToArithmOp :: AbsGrammar.ForDirection -> TACOp
                forDirToArithmOp AbsGrammar.ForDirection_downto = TACSubInt
                forDirToArithmOp AbsGrammar.ForDirection_to = TACAddInt

                forDirToRelOp :: AbsGrammar.ForDirection -> AbsGrammar.BinaryOperator
                forDirToRelOp AbsGrammar.ForDirection_downto = AbsGrammar.GreatT
                forDirToRelOp AbsGrammar.ForDirection_to = AbsGrammar.LessT

                Just (Variable _ _ _ addr) = Env.lookup id env

        _ -> error "Internal Error: genStmtIter -> shouldn't reach here"

    where
        getIterScopeEnv :: AbsGrammar.IterStmt Env AbsGrammar.Type -> StateTAC Env
        getIterScopeEnv (AbsGrammar.StmtWhileDo _ (AbsGrammar.StmtComp (AbsGrammar.BegEndBlock _ scopeEnv))) = return scopeEnv
        getIterScopeEnv (AbsGrammar.StmtRepeat (AbsGrammar.StmtComp (AbsGrammar.BegEndBlock _ scopeEnv)) _) = return scopeEnv
        getIterScopeEnv (AbsGrammar.StmtFor _ _ _ _ (AbsGrammar.StmtComp (AbsGrammar.BegEndBlock _ scopeEnv))) = return scopeEnv

genStmtIter _ _ =  error "TODO: gestire errore genStmtIter"


--TODO: determinare con quale ordine devo fare la valutazione delle guardie
genGuard :: AbsGrammar.EXPR AbsGrammar.Type -> TACLabel -> TACLabel -> Env -> StateTAC ()
genGuard guard trueLab falseLab env = case guard of

    (AbsGrammar.BinaryExpression opr b1@expr1 b2@expr2 tp) -> case opr of

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
                    --TODO: verificare se funziona correttamente passando False
                    e1XAddr <- genExpr expr1 False env
                    e2XAddr <- genExpr expr2 False env
                    expr1Type <- getExprType expr1

                    e1Addr <- case e1XAddr of
                        (Addr a) -> return a
                        _ -> error "TODO: genGuard -> getisci errore creazione address expr"
                    e2Addr <- case e2XAddr of
                        (Addr a) -> return a
                        _ -> error "TODO: genGuard -> getisci errore creazione address expr"

                    case (trueLab, falseLab) of
                        (Fall, Fall) -> return ()
                        (_, Fall) -> addInstr (TACCndJmp e1Addr (binToTACOp opr (gramTypeToTACType expr1Type)) e2Addr trueLab)
                        (Fall, _) -> addInstr (TACCndJmp e1Addr (notRel opr (gramTypeToTACType expr1Type)) e2Addr falseLab)
                        _ -> do
                            addInstr (TACCndJmp e1Addr (binToTACOp opr (gramTypeToTACType expr1Type)) e2Addr trueLab)
                            addInstr (TACUncdJmp falseLab)

                else error "TODO: genGuard -> operatore non è ne logico ne relazionale"

    (AbsGrammar.UnaryExpression opr b1@expr tp) -> case opr of
        AbsGrammar.Not -> genGuard b1 falseLab trueLab env

    (AbsGrammar.ExprLiteral (AbsGrammar.LiteralBoolean (AbsGrammar.TokBoolean (_, value)))) -> 
        case value of
            "true" -> if trueLab == Fall then return () else addInstr (TACUncdJmp trueLab)
            "false" -> if falseLab == Fall then return () else addInstr (TACUncdJmp falseLab)
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
    --TODO: stiamo facendo l'assunzione che possiamo restituire solo r-value
    exprXAddr <- genExpr expr False env
    exprAddr <- case exprXAddr of
        (Addr a) -> return a
        _ -> error "TODO: genStmtReturn -> getisci errore creazione address expr"
    addInstr (TACReturn exprAddr)

genExpr :: AbsGrammar.EXPR AbsGrammar.Type -> Bool -> Env -> StateTAC XAddr
genExpr expr genL env =
    case expr of
        --TODO: considerare se passare genL alle funzioni
        (AbsGrammar.UnaryExpression _ _ _) -> genUnrExpr expr genL env
        (AbsGrammar.BinaryExpression _ _ _ _) -> genBinExpr expr genL env
        (AbsGrammar.ExprLiteral _) -> genLitExpr expr genL env
        (AbsGrammar.SelExpr _ _ _ _) -> error "TODO: implementare selExpr"
        (AbsGrammar.ExprCall _ _) -> genExprCall expr genL env
        (AbsGrammar.BaseExpr _ _) -> genBaseExpr expr genL env
        (AbsGrammar.IntToReal exp) -> genCastIntToRealExpr expr genL env


-- TODO: valutare come usare tp(fare cast) ed env
genUnrExpr :: AbsGrammar.EXPR AbsGrammar.Type -> Bool -> Env -> StateTAC XAddr
genUnrExpr (AbsGrammar.UnaryExpression op@AbsGrammar.Dereference exp1 tp) genL env = do
    expXAddr <- genExpr exp1 True env
    exprType <- getExprType exp1
    case (exprType, expXAddr) of
        (AbsGrammar.TypeCompType (AbsGrammar.Pointer ptType), Addr addr) ->
            if genL
                then return $ RefAddr addr
                else do
                    tmpAddr <- newTmpAddr
                    addInstr (TACAssDeref tmpAddr addr)
                    return $ Addr tmpAddr
        _ -> error "TODO:genUnrExpr -> gestire altri casi case come da slide"

genUnrExpr (AbsGrammar.UnaryExpression op@AbsGrammar.Reference exp1 tp) genL env = do
    expXAddr <- genExpr exp1 True env
    exprType <- getExprType exp1
    case (exprType, expXAddr) of
        (AbsGrammar.TypeBaseType _, Addr addr) ->
            if genL
                then return $ RefAddr addr
                else do
                    tmpAddr <- newTmpAddr
                    addInstr (TACAssRef tmpAddr addr)
                    return $ Addr tmpAddr
        _ -> error "TODO:genUnrExpr -> gestire altri casi case come da slide"

genUnrExpr (AbsGrammar.UnaryExpression op exp1 tp) genL env = do
    if genL
    then
        error "TODO: genUnrExpr -> E non è l-expr"
    else do
        tmpAddr <- newTmpAddr;
        rVal <- genExpr exp1 False env;
        exprType <- getExprType exp1;

        case rVal of
            (Addr addr) -> do
                addInstr (TACUnAss tmpAddr (unrToTACOp op (gramTypeToTACType tp)) addr);
                return $ Addr tmpAddr;

            _ -> error "TODO: genUnrExpr (Neg, Not) -> implementare altri casi case "

genUnrExpr _ _ _ = error "TODO: gestire errore genUnrExpr"


genBinExpr :: AbsGrammar.EXPR AbsGrammar.Type -> Bool -> Env -> StateTAC XAddr
genBinExpr binExp@(AbsGrammar.BinaryExpression op exp1 exp2 tp) genL env = do
    if isGuardBinOp op
        then do
            tmpAddr <- newTmpAddr
            trueLab <- newLabel
            falseLab <- newLabel
            outLabel <- newLabel

            genGuard binExp trueLab falseLab env
            attachLabelToNext trueLab
            addInstr (TACNulAss tmpAddr (TacLit (TACBoolLit True)))
            addInstr (TACUncdJmp outLabel)
            attachLabelToNext falseLab
            addInstr (TACNulAss tmpAddr (TacLit (TACBoolLit False)))
            
            attachLabelToNext outLabel
            return $ Addr tmpAddr

        else if genL
            then error "TODO: E non è l-expr"
            else do
                tmpAddr <- newTmpAddr;
                rVal1 <- genExpr exp1 False env;
                expr1Type <- getExprType exp1;
                rVal2 <- genExpr exp2 False env;
                expr2Type <- getExprType exp2;

                case (rVal1, rVal2) of
                    (Addr tmpAddr1, Addr tmpAddr2) -> do
                        addInstr (TACBinAss tmpAddr tmpAddr1 (binToTACOp op (gramTypeToTACType tp)) tmpAddr2);
                        return $ Addr tmpAddr

                    (Addr tmpAddr1, ArrayAddr base2 offset2) -> do

                        tmpAddr <- newTmpAddr
                        addInstr (TACIndxLd tmpAddr base2 offset2)

                        tmpRes <- newTmpAddr
                        addInstr (TACBinAss tmpRes tmpAddr1 (binToTACOp op (gramTypeToTACType tp)) tmpAddr);
                        return $ Addr tmpRes

                    _ -> error "TODO: genBinExpr -> gestisci altri casi case come da slide "

genBinExpr _ _ _ = error "TODO: gestire errore genBinExpr"


getExprType :: AbsGrammar.EXPR AbsGrammar.Type -> StateTAC AbsGrammar.Type
getExprType expr = case expr of
    AbsGrammar.UnaryExpression _ _ tp -> return tp;
    AbsGrammar.BinaryExpression _ _ _ tp -> return tp;
    AbsGrammar.SelExpr _ _ _ tp -> return tp;
    AbsGrammar.ExprCall _ tp -> return tp;
    AbsGrammar.BaseExpr _ tp -> return tp;
    AbsGrammar.ExprLiteral lit -> return (AbsGrammar.TypeBaseType (Env.getTypeFromLiteral lit));
    AbsGrammar.IntToReal _ -> return (AbsGrammar.TypeBaseType AbsGrammar.BaseType_real)


genLitExpr :: AbsGrammar.EXPR AbsGrammar.Type -> Bool -> Env -> StateTAC XAddr
genLitExpr (AbsGrammar.ExprLiteral lit) genL env = return $ Addr (TacLit $ makeTACLit lit)

genExprCall :: AbsGrammar.EXPR AbsGrammar.Type -> Bool -> Env -> StateTAC XAddr
genExprCall (AbsGrammar.ExprCall (AbsGrammar.CallArgs (AbsGrammar.TokIdent (_,callId)) args) tp) genL env =
    case Env.lookup callId env of
        Just (Env.Function _ prms retType fAddr) -> do
            genArgs args prms env
            tmpAddr <- newTmpAddr
            addInstr (TACFCall tmpAddr fAddr (length args))
            --TODO: implementa correttamente return
            return $ Addr tmpAddr
        _ -> error "TODO : errore in genExprCall; funzione con questo nome non esiste"


genArgs :: [AbsGrammar.EXPR AbsGrammar.Type] -> AbsGrammar.Prms -> Env -> StateTAC ()
genArgs (arg:args) prms env = do
    (mod, restPrms) <- nextParamMod prms
    
    case mod of
        AbsGrammar.Modality_val -> do
            tmpArgAddr <- genExpr arg False env
            case tmpArgAddr of
                (Addr addr) -> addInstr (TACParam addr)
    
        AbsGrammar.Modality_ref -> do
            tmpArgAddr <- genExpr arg True env
            case tmpArgAddr of
                (RefAddr refAddr) -> addInstr (TACParam refAddr)
                (Addr argAddr) -> do
                    tmpAddr <- newTmpAddr
                    addInstr (TACAssRef tmpAddr argAddr)
                    addInstr (TACParam tmpAddr)
                _-> error "TODO: genArgs -> implementa correttamente"



    genArgs args restPrms env
    where
        nextParamMod :: AbsGrammar.Prms -> StateTAC (AbsGrammar.Modality, AbsGrammar.Prms)
        nextParamMod (AbsGrammar.Params ((AbsGrammar.Param mod (a:[]) tp):[])) = return (mod, AbsGrammar.NoParams)
        nextParamMod (AbsGrammar.Params ((AbsGrammar.Param mod (a:[]) tp):restPrms)) = return (mod, (AbsGrammar.Params restPrms))
        nextParamMod (AbsGrammar.Params ((AbsGrammar.Param mod (id:restId) tp):restPrms)) = return (mod, (AbsGrammar.Params ((AbsGrammar.Param mod restId tp):restPrms)))
        nextParamMod _ = error "InternalError: shouldn't be searching param if there aren't any more"
        

genArgs [] _ _ = return ()


genBaseExpr :: AbsGrammar.EXPR AbsGrammar.Type -> Bool -> Env -> StateTAC XAddr
genBaseExpr expr@(AbsGrammar.BaseExpr bexpr tp) genL env = do
    case bexpr of

        (AbsGrammar.Identifier (AbsGrammar.TokIdent (_, id))) -> do
            case Env.lookup id env of
                Just (Env.Variable AbsGrammar.Modality_val _ idType addr) -> do
                    return $ Addr addr
                Just (Env.Variable AbsGrammar.Modality_ref _ idType addr) -> do
                    tmpAddr <- newTmpAddr
                    addInstr (TACAssRef tmpAddr addr)
                    return $ RefAddr tmpAddr
                _ -> error "InternalError : genBaseExpr -> gestisci altri casi con diverse modalità"

        arr@(AbsGrammar.ArrayElem _ _) -> genArrayExpr expr genL env


genBaseExpr be _ _ = error "TODO: genBaseExpr -> gestire altri casi mancanti"


genArrayExpr :: AbsGrammar.EXPR AbsGrammar.Type -> Bool -> Env -> StateTAC XAddr
genArrayExpr (AbsGrammar.BaseExpr (AbsGrammar.ArrayElem arrExpr indexExpr) arrTpOf) genL env = do
    aXVal <- genExpr arrExpr True env
    iXVal <- genExpr indexExpr False env
    tp <- getExprType arrExpr

    case (aXVal, iXVal) of
        (Addr fldAddr, Addr indAddr)-> do        
            genCheckIndexBounds indAddr tp env

            offsetAddr <- newTmpAddr
            sizeXAddr <- genLitExpr (convertIntToExpr (sizeof arrTpOf)) False env
            sizeAddr <- case sizeXAddr of
                (Addr a) -> return a
                _ -> error "TODO: genArrayExpr -> getisci errore creazione address literal"
            addInstr (TACBinAss offsetAddr indAddr TACMulInt sizeAddr)
            if genL
                then return $ ArrayAddr fldAddr offsetAddr
                else do
                    tmpAddr <- newTmpAddr
                    addInstr (TACIndxLd tmpAddr fldAddr offsetAddr)
                    return $ Addr tmpAddr

        (ArrayAddr bsAddr offAddr, Addr indAddr )-> do
            genCheckIndexBounds indAddr tp env

            sizeXAddr <- genLitExpr (convertIntToExpr (sizeof arrTpOf)) False env
            sizeAddr <- case sizeXAddr of
                (Addr a) -> return a
                _ -> error "TODO: genArrayExpr -> getisci errore creazione address literal"
            tmpOffset <- newTmpAddr
            addInstr (TACBinAss tmpOffset indAddr TACMulInt sizeAddr)
            offset <- newTmpAddr
            addInstr (TACBinAss offset tmpOffset TACAddInt offAddr)
            if genL
                then return $ ArrayAddr bsAddr offset
                else do
                    tmpAddr <- newTmpAddr
                    addInstr (TACIndxLd tmpAddr bsAddr offset)
                    return $ Addr tmpAddr

        _ -> error "TODO: genArrayExpr -> considera altri casi case slide"
    
    where
        genCheckIndexBounds :: Addr -> AbsGrammar.Type -> Env -> StateTAC () 
        genCheckIndexBounds indexAddr (AbsGrammar.TypeCompType arrTp@(AbsGrammar.Array {})) env = do
            outOfBoundsLabel <- newLabel
            nextStmtLabel <- newLabel
            addInstr (TACCndJmp indexAddr TACEqLessTInt (TacLit l_end) outOfBoundsLabel)
            addInstr (TACCndJmp indexAddr TACEqLessTInt (TacLit r_end) nextStmtLabel)
            attachLabelToNext outOfBoundsLabel
            case Env.lookup "errOutOfBounds" env of
                Just (Procedure _ _ addr) -> do
                    addInstr (TACPCall addr 0)
                    attachLabelToNext nextStmtLabel
                Nothing -> error "InternalError: errOutOfBounds is a default procedure; it can't be missing"

            where
                (l_end, r_end) = getArrayRange arrTp

        genCheckIndexBounds _ _ _ = error "InternalError: genCheckIndexBounds shouldn't receive non array type"

genArrayExpr _ _ _ = error "TODO: genArrayExpr -> non è un array"


genCastIntToRealExpr :: AbsGrammar.EXPR AbsGrammar.Type -> Bool -> Env -> StateTAC XAddr
genCastIntToRealExpr (AbsGrammar.IntToReal expr) genL env = do
    tmpXAddr <- genExpr expr genL env
    tmpAddr <- newTmpAddr

    case tmpXAddr of
        (Addr addr) -> do
            addInstr (TACUnAss tmpAddr TACCastIntToReal addr)
            return (Addr tmpAddr)
        _ -> error "TODO: genCastIntToReal : non arriva mai qui"


addInstr :: TACInst -> StateTAC ()
addInstr instr = do
    state <- get;
    newQuad <- instrToQuad instr (labelsNextInstr state)
    if null (stackStrms state)
        then put $ state { quads = appendQuad (quads state) newQuad, labelsNextInstr = AddrList [] }
        else do
            (strm, stack) <- pop $ stackStrms state
            newStack <- push (appendQuad strm newQuad) stack
            put $ state {stackStrms = newStack, labelsNextInstr = AddrList []}


attachLabelToNext :: TACLabel -> StateTAC ()
attachLabelToNext label = do
    state <- get
    if label == Fall
        then return ()
        else do
            put $ state {labelsNextInstr = appendAddrList (getLabelAddr label) (labelsNextInstr state) }

