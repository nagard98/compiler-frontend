{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module TypeChecker where
import AbsGrammar
import Env
import HelperTAC
import Control.Monad.State.Strict
import Debug.Trace
import Data.Map

launchStatSemAnalysis :: P env infType -> (Env, Errors, P Env Type)
launchStatSemAnalysis tree = (env, errors finalState, parsedTree)
    where
        ((env, parsedTree), finalState) = runState (parseTree defaultEnv tree) ssaState
        ssaState = SSAStateStruct {idCount = 0, errors = emptyErrors, unInitVars = newStack }

-- Type Checking starting point
parseTree :: Env -> P env infType -> SSAState (Env, P Env Type)
parseTree defEnv (Prog pBlock dclBlock beBlock _) = do
    pushNewUninit
    (globEnv, dBlks) <- parseDclBlocksFirstPass defEnv dclBlock
    (globEnv1, annBlks) <- parseDclBlocks globEnv dBlks
        -- errors and env are propagated from declaration block into beginEnd Block!
        -- notice that globEnv is the env after parsing declaration blocks
    (finalEnv, beBlks, hasReturn) <- parseBEBlock globEnv1 beBlock

    state <- get
    if hasReturn  
        then put $ state{errors = ("ERROR: You cannot have a return statement in the main begin-end block"):(errors state)}   
        else put $ state{errors = errors state} 

    popUninit
    
    return (finalEnv, Prog pBlock annBlks beBlks globEnv)

--TODO: prova a rimuovere DclBlock come valore di ritorno per i first pass
-- Navigates syntax tree and saves info about variables type (declared in a Declaration block) in the global environment
parseDclBlocksFirstPass:: Env -> [DclBlock env infType] -> SSAState (Env, [DclBlock env infType])
parseDclBlocksFirstPass env [] = return (env, [])
parseDclBlocksFirstPass env (x:xs) = do
    (env1, newBlock) <- parseSingleDclBlockFirstPass env x
    (finalEnv, newBlocks) <- parseDclBlocksFirstPass env1 xs
    return (finalEnv, newBlock : newBlocks)
    where
        -- TODO: make sure errores are updated after parsing declaration blocks
        -- e.g. redefining a variable could produce a warning and redefinig a constant an error
        parseSingleDclBlockFirstPass :: Env -> DclBlock env infType -> SSAState (Env, DclBlock env infType)
        parseSingleDclBlockFirstPass env blk = case blk of
            DclBlockVrBlock _ -> parseDclVrBlockFirstPass env blk
            DclBlockCsBlock _ -> parseDclCsBlockFirstPass env blk
            -- TODO: pass global environment to begin-end block and parse inner statemets
            DclBlockFcBlock _ -> parseDclFcBlockFirstPass env blk
            DclBlockPcBlock _ -> parseDclPcBlockFirstPass env blk


-- Navigates syntax tree and saves info about variables type (declared in a Declaration block) in the global environment
parseDclBlocks:: Env -> [DclBlock env infType] -> SSAState (Env, [DclBlock Env Type])
parseDclBlocks env [] = return (env, [])
parseDclBlocks env (x:xs) = do
    (env1, newBlock) <- parseSingleDclBlock env x
    (finalEnv, newBlocks) <- parseDclBlocks env1 xs
    return (finalEnv, newBlock : newBlocks)
    where
        -- TODO: make sure errores are updated after parsing declaration blocks
        -- e.g. redefining a variable could produce a warning and redefinig a constant an error
        parseSingleDclBlock :: Env -> DclBlock env infType -> SSAState (Env, DclBlock Env Type)
        parseSingleDclBlock env blk = case blk of
            DclBlockVrBlock (VarBlock vrDefs) -> return (env, DclBlockVrBlock (VarBlock vrDefs))
            DclBlockCsBlock (ConstBlock csDefs) -> return (env, DclBlockCsBlock (ConstBlock csDefs)) 
            -- TODO: pass global environment to begin-end block and parse inner statemets
            DclBlockFcBlock _ -> parseDclFcBlock env blk
            DclBlockPcBlock _ -> parseDclPcBlock env blk


parseDclVrBlockFirstPass :: Env -> DclBlock env infType -> SSAState (Env, DclBlock env infType)
parseDclVrBlockFirstPass env (DclBlockVrBlock (VarBlock vrDefs)) = do
    newEnv <- parseVrDefs vrDefs env;
    -- add info about variables to the environment
    return (newEnv, DclBlockVrBlock (VarBlock vrDefs))
        where
            -- savese info about variables type in env 
            parseVrDefs :: [VrDef] -> Env -> SSAState Env
            parseVrDefs [] env = return env
            parseVrDefs ((VarDefinition idElements t):vrDefs) env = do
                -- TODO : probabilmente necessaria gestione errori (ovvero restituire anche errori)
                (tmpEnv, _) <- parseIds idElements Modality_val t env True
                return tmpEnv
            

-- add info about constants to the environment          
parseDclCsBlockFirstPass :: Env -> DclBlock env infType -> SSAState (Env, DclBlock env infType)
parseDclCsBlockFirstPass env (DclBlockCsBlock (ConstBlock csDefs)) = do 
    newEnv <- parseConsDefs csDefs env
    return (newEnv, DclBlockCsBlock (ConstBlock csDefs))
        where
            -- saves info about constants type in env 
            parseConsDefs :: [CsDef] -> Env -> SSAState Env
            parseConsDefs [] env = return env
            parseConsDefs ((ConstDefinition (IdElement (TokIdent (pos, id))) literal):cs) env = do 
                tmpEnv <- Env.insert id (Constant pos (TypeBaseType (getTypeFromLiteral literal)) (TacLit (makeTACLit literal))) env
                parseConsDefs cs tmpEnv;
            

-- add info about functions to the environment
-- info are: function position, function name, parameters, return type
parseDclFcBlockFirstPass :: Env -> DclBlock env infType -> SSAState (Env, DclBlock env infType)
parseDclFcBlockFirstPass env dcBlockFc@(DclBlockFcBlock (FuncBlock (TokIdent (pos, id)) params retType beb)) = do 
    fcAddr <- Env.newIdAddr id env

    -- add to env return type (needed for type checking of the return statement) and function info
    -- IMPORTANT NOTE: env must be the secondo argument of mergeEnvs, otherwise the new "return" key will not be updated
    -- this is because the underlying function union (t1, t2) of Data.Map prefers t1 when duplicated keys are encountered
    tmpEnv <- Env.mergeEnvs (Env.fromList [(id, Function pos params retType fcAddr)]) env
    (tmpEnv1, parsedParams) <- parseParams params [] tmpEnv
    return (tmpEnv1, dcBlockFc)


parseDclFcBlock :: Env -> DclBlock env infType -> SSAState (Env, DclBlock Env Type)
parseDclFcBlock env (DclBlockFcBlock fB@(FuncBlock idTok@(TokIdent (pos, id)) params retType beb)) = do
    tmpEnv <-  Env.insert ("return") (Return retType id pos) env         
    
    (finalEnv, annotatedBEB, hasAllReturns) <- parseBEBlock tmpEnv beb

    state <- get
    if hasAllReturns  
        then put $ state{errors = errors state} 
        else put $ state{errors = errMsg:(errors state)}   

    return (finalEnv, DclBlockFcBlock (FuncBlock idTok params retType annotatedBEB))

    where        
        errMsg = ("Missing return statement: body of function " ++ id ++ " at " ++ show pos ++ " does not contain a return statement")

-- add info about procedures to the environment. Same as functions but without return type
parseDclPcBlockFirstPass :: Env -> DclBlock env infType -> SSAState (Env, DclBlock env infType)
parseDclPcBlockFirstPass env dclBlockPc@(DclBlockPcBlock (ProcBlock (TokIdent (pos, id)) params beb)) = do
    pcAddr <- Env.newIdAddr id env
    tmpEnv <- Env.insert id (Procedure pos params pcAddr) env
    (pEnv, pPrms) <- parseParams params [] tmpEnv

    return (pEnv, dclBlockPc)

-- add info about procedures to the environment. Same as functions but without return type
parseDclPcBlock :: Env -> DclBlock env infType -> SSAState (Env, DclBlock Env Type)
parseDclPcBlock env (DclBlockPcBlock (ProcBlock idTok@(TokIdent (pos, id)) params beb)) = do
    (fEnv, annBEB, hasReturn) <- parseBEBlock env beb
    state <- get
    if hasReturn  
        then put $ state{errors = ("ERROR in procedure " ++ id ++ " at " ++ show pos ++ ": You cannot have a return statement in a procedure"):(errors state)}   
        else put $ state{errors = errors state} 
    
    return (fEnv, DclBlockPcBlock (ProcBlock idTok params annBEB))


parseParams :: Prms -> [Prm] -> Env -> SSAState (Env, Prms)
parseParams prms accPrms env = do 
    case prms of
        (Params ( p@(Param mod idList typ):ps )) -> do
            (tmpEnv, _) <- parseIds idList mod typ env False
            (pEnv, pPrms) <- parseParams (Params ps) (p:accPrms) tmpEnv
            return (pEnv, pPrms)
                
        (Params []) -> return (env, Params accPrms)

        NoParams -> return (env, prms)


parseIds :: [IdElem] -> Modality -> Type -> Env -> Bool -> SSAState (Env, [IdElem])
parseIds [] mod typ env _ = return (env, [])
parseIds ( idElem@(IdElement (TokIdent (pos, id))):ids) mod typ env isVar = do
    idAddr <- newIdAddr id env
    tmpEnv <- Env.insert id (Variable mod pos typ idAddr) env

    if isVar
        then do
            insertVar id (Variable mod pos typ idAddr)
            (newEnv, newIds) <- parseIds ids mod typ tmpEnv isVar
            return (newEnv, idElem:newIds)
        else do
            (newEnv, newIds) <- parseIds ids mod typ tmpEnv isVar
            return (newEnv, idElem:newIds) 


-- parse the begin-end block and check the statements for type errors
parseBEBlock:: Env -> BEBlock env infType -> SSAState (Env, BEBlock Env Type, Bool)
parseBEBlock env (BegEndBlock statements annEnv)  = do
    pushNewUninit
    tmpEnv <- parseStatementsFirstPass env statements
    (newEnv, newStatements, hasAllReturns) <- parseStatements tmpEnv statements 
    popUninit
    return (newEnv, BegEndBlock newStatements newEnv, hasAllReturns)
        

parseStatementsFirstPass :: Env -> [Stmt env infType] -> SSAState (Env)
parseStatementsFirstPass env ((StmtDecl dclBlock):stmts) = do
    (newEnv, tmpDclBlocks) <- parseDclBlocksFirstPass env [dclBlock]
    parseStatementsFirstPass newEnv stmts

parseStatementsFirstPass env (_:stmts) = parseStatementsFirstPass env stmts
parseStatementsFirstPass env _ = return env


parseStatements :: Env -> [Stmt env infType] -> SSAState (Env, [Stmt Env Type], Bool)
parseStatements env [] = return (env, [], False)
parseStatements env allStmts = q env allStmts [] False
        where
            q::Env -> [Stmt env infType] -> [Stmt Env Type] -> Bool -> SSAState (Env, [Stmt Env Type], Bool)
            q env [] annStmts accReturn = return (env, annStmts, accReturn)
            q env (s:xs) annStmts accReturn = do
                (env1, annStmt, isReturn) <- parseStatement s env 
                q env1 xs (annStmts++[annStmt]) (accReturn || isReturn)


parseStatement :: Stmt stmtenv infType -> Env  ->  SSAState (Env, Stmt Env Type, Bool)
parseStatement stmt env = case stmt of
            -- tipologie di statement: dichiarazione, blocco, assegnamento, chiamata funzione, if-else, iterazione, return
            -- Dichiarazione
            (StmtDecl dclblock) -> do
                (env2, blocks) <- parseDclBlocks env [dclblock]
                return (env2, (StmtDecl (head blocks)), False)
                    
            -- Blocco
            (StmtComp beblock) -> do
                (env2, block, hasAllReturns) <- parseBEBlock env beblock 
                return (env2, (StmtComp block), hasAllReturns)

            -- Assegnamento
            (StmtAssign expr1 expr2) -> do
                (env2, parsedStmt) <- parseAssignment expr1 expr2 env 
                return (env2, parsedStmt, False)

            -- Iterazione
            (StmtIter iter) -> parseIter (StmtIter iter) env 

            -- Return
            (StmtReturn rt)  -> do
                (env2, parsedStmt) <- parseReturn (StmtReturn rt) env 
                return (env2, parsedStmt, True)

            -- Select
            (StmtSelect sel) -> parseSelection (StmtSelect sel) env 

            -- Chiamata funzione
            (StmtCall call) -> do
                (env2, parsedStmt) <- parseStatementCall env call
                return (env2, parsedStmt, False)


parseIter :: Stmt env infType -> Env -> SSAState (Env, Stmt Env Type, Bool)
-- parsing of while-do statement
parseIter (StmtIter (StmtWhileDo expr stmt)) env  = do
    (env1, parsedExpr, posEnds) <- parseExpression env expr

    (newEnv, parsedStmt, isReturn) <- parseStatement stmt env1 
    let wrappedStmt = wrapInBeginEnd parsedStmt newEnv

    if getTypeFromExpression parsedExpr == TypeBaseType BaseType_boolean || getTypeFromExpression parsedExpr == TypeBaseType BaseType_error
        then return (newEnv, StmtIter (StmtWhileDo parsedExpr wrappedStmt), isReturn)
        else do
            state <- get
            put $ state {errors = ("ERROR in range " ++ show posEnds  ++ ": condition " ++ showExpr parsedExpr ++ " of while-do statement is not of type Bool but it is of type " ++ show (getTypeFromExpression parsedExpr)):(errors state)}
            return (newEnv, StmtIter (StmtWhileDo parsedExpr wrappedStmt), isReturn)

-- parsing of repeat-until statement
parseIter (StmtIter (StmtRepeat stmt expr)) env  = do
    (env1, parsedStmt, isReturn) <- parseStatement stmt env 
    let wrappedStmt = wrapInBeginEnd parsedStmt env1
    (newEnv, parsedExpr, posEnds) <- parseExpression env1 expr

    if getTypeFromExpression parsedExpr == TypeBaseType BaseType_boolean || getTypeFromExpression parsedExpr == TypeBaseType BaseType_error
        then return (newEnv, StmtIter (StmtRepeat wrappedStmt parsedExpr), isReturn)
        else do
            state <- get
            put $ state {errors = ("ERROR in range " ++ show posEnds ++ ": condition " ++ showExpr parsedExpr ++ " of repeat-until statement is not of type Bool but it is of type " ++ show (getTypeFromExpression parsedExpr)):(errors state)}
            return (newEnv, StmtIter (StmtRepeat wrappedStmt parsedExpr), isReturn)

parseSelection :: Stmt env infType -> Env -> SSAState (Env, Stmt Env Type, Bool)
parseSelection (StmtSelect (StmtIf expr stmt)) env  = do
    (env1, parsedExpr, posEnds) <- parseExpression env expr
    (newEnv, parsedStmt, isReturn) <- parseStatement stmt env1 
    let wrappedStmt = wrapInBeginEnd parsedStmt newEnv

    if getTypeFromExpression parsedExpr == TypeBaseType BaseType_boolean || getTypeFromExpression parsedExpr == TypeBaseType BaseType_error
        then return (newEnv, StmtSelect (StmtIf parsedExpr wrappedStmt), isReturn)
        else do
            state <- get
            put $ state {errors = ("ERROR in range " ++ show posEnds  ++ ": condition " ++ showExpr parsedExpr ++ " of if statement is not of type Bool but it is of type " ++ show (getTypeFromExpression parsedExpr)):(errors state)}
            return (newEnv, StmtSelect (StmtIf parsedExpr wrappedStmt), isReturn)

parseSelection (StmtSelect (StmtIfElse expr stmt1 stmt2)) env  = do
    (env1, parsedExpr, posEnds) <- parseExpression env expr
    (newEnv1, parsedStmt1, isReturn1) <- parseStatement stmt1 env1 
    let wrappedStmt1 = wrapInBeginEnd parsedStmt1 newEnv1
    (newEnv2, parsedStmt2, isReturn2) <- parseStatement stmt2 newEnv1 
    let wrappedStmt2 = wrapInBeginEnd parsedStmt2 newEnv2

    if getTypeFromExpression parsedExpr == TypeBaseType BaseType_boolean || getTypeFromExpression parsedExpr == TypeBaseType BaseType_error
        then return (newEnv2, StmtSelect (StmtIfElse parsedExpr wrappedStmt1 wrappedStmt2), isReturn1 && isReturn2)
        else do
            state <- get
            put $ state { errors = ("ERROR in range " ++ show posEnds  ++ ": condition " ++ showExpr parsedExpr ++ " of if-else statement is not of type Bool but it is of type " ++ show (getTypeFromExpression parsedExpr)):(errors state) }
            return (newEnv2, StmtSelect (StmtIfElse parsedExpr wrappedStmt1 wrappedStmt2), isReturn1 && isReturn2)


-- If the provided statement is not insiede a begin-end block, wraps it in one
-- This is needed to simplify TAC generation by having to deal only with begin-end blocks containing statements
-- With the current grammar the body of a iteration (or an if-else) statement can be eather a single statement or a begin-end block with other statements
wrapInBeginEnd :: Stmt Env infType -> Env -> Stmt Env infType
wrapInBeginEnd (StmtComp (BegEndBlock stmts begEnv)) _ = StmtComp (BegEndBlock stmts begEnv)
wrapInBeginEnd stmt stmEnv = StmtComp (BegEndBlock [stmt] stmEnv)

parseReturn :: Stmt env infType -> Env -> SSAState (Env, Stmt Env Type)
parseReturn (StmtReturn (Ret expr)) env = do
    (newEnv, parsedExpr, posEnds) <- parseExpression env expr
    
    case Env.lookup "return" env of 
    
        Just (Return expectedType funName funPos) ->
            if sup expectedType (getTypeFromExpression parsedExpr) /= expectedType && getTypeFromExpression parsedExpr /= TypeBaseType BaseType_error
                then do
                    state <- get
                    put $ state { errors = (("ERROR in range " ++ show posEnds  ++ ": function " ++ funName ++ " at " ++ show funPos ++ 
                        " expects a " ++ show expectedType ++ " to be returned " ++
                        "but the expression following the return statement has type " ++ show (getTypeFromExpression parsedExpr) )):(errors state)}
                    return ( newEnv, StmtReturn (Ret parsedExpr))
                else
                    -- everything is ok or the error did not happen here, return the parsed expression 
                    return (newEnv, StmtReturn (Ret parsedExpr))

        -- Theoretically this should never happen, 
        -- since the return type of the function is saved in the environment when the function definition is parsed
        Nothing -> do
            state <- get
            put $ state { errors = ("Internal type checking error: can't find expected return type of current function in the environment while parsing the expression following the return at " ++ show posEnds):(errors state)}
            return (newEnv, StmtReturn (Ret parsedExpr)) 
            

parseAssignment :: EXPR infType -> EXPR infType -> Env -> SSAState (Env, Stmt Env Type)
parseAssignment expr1 expr2 env = case (expr1, expr2) of
            -- Assegno a variabile un letterale
            ( (BaseExpr (Identifier tId@(TokIdent (_,id))) tp), (ExprLiteral literal) ) -> do
                removeVar id
                (env2, parsedid, posEnds) <- parseExpression env (BaseExpr (Identifier tId) tp)
                parseLitAssignment tId literal env2 posEnds
            
            -- Assegno a variabile valore espressione generica: 1) parsing dell'espressione e trovo il tipo; 2) controllo compatibilità con letterale in assegnamento
            ( (BaseExpr (Identifier tId@(TokIdent (_,id))) tp), expr ) -> do
                removeVar id
                (env2, parsedid, posEnds) <- parseExpression env (BaseExpr (Identifier tId) tp)
                (env3, parsedexpr, posEnds2) <- parseExpression env2 expr
                parseIdExprAssignment tId parsedexpr env3 posEnds --TODO: ricavare il range corretto, anche nei casi successivi
            
            -- Puntatore è un l-value valido
            ( (UnaryExpression Dereference expr1 t), expr2 ) -> do
                (env2, parsedexpr1, posEnds1) <- parseExpression env (UnaryExpression Dereference expr1 t)
                (env3, parsedexpr2, posEnds2) <- parseExpression env2 expr2
                parseExprExprAssignment parsedexpr1 parsedexpr2 env3 posEnds1
            
            -- Riferimento ad un puntatore è un l-value valido            
            ( (UnaryExpression Reference expr1 t), expr2 ) -> do
                (env2, parsedexpr1, posEnds1) <- parseExpression env (UnaryExpression Reference expr1 t)
                (env3, parsedexpr2, posEnds2) <- parseExpression env2 expr2                
                parseExprExprAssignment parsedexpr1 parsedexpr2 env3 posEnds1
            
            -- Array elements are valid l-values
            ( arr@(BaseExpr (ArrayElem idexpr iexpr) t), expr ) -> do
                (env2, parsedexpr, posEnds1) <- parseExpression env expr
                --(env3, err3, parsediexpr) <- parseExpression env2 err2 iexpr
                (env3, parsedarrexpr, posEnds2) <- parseExpression env2 arr
                
                parseArrayAssignment parsedarrexpr parsedexpr env3 posEnds1
            
            -- Il resto dei possibili l-value non è valido
            ( expr1, expr2 ) -> do
                (env2, parsedexpr1, posEnds1) <- parseExpression env expr1
                (env3, parsedexpr2, posEnds2) <- parseExpression env2 expr2
                state <- get
                put $ state { errors = ("Error in range " ++ show posEnds1 ++ ": expression "++ showExpr expr1 ++" is not a valid l-value for the assignment"):(errors state)}
                return (env3, (StmtAssign parsedexpr1 parsedexpr2) )


-- Checks if r-expression matches typing with an l-expression that is an element of an array
parseArrayAssignment:: EXPR Type -> EXPR Type -> Env -> PosEnds -> SSAState (Env, Stmt Env Type)
parseArrayAssignment bExpr@(BaseExpr (ArrayElem bbexpr iiexpr) t) expr env posEnds = do
    if t == rtype
        then return (env, (StmtAssign bExpr expr))
        else 
            -- 3 cases: a) int to real, b) char to string, c) incompatible types -> error
            case (t, rtype) of
                
                (TypeBaseType BaseType_real, TypeBaseType BaseType_integer) -> 
                    return (env, (StmtAssign bExpr (IntToReal expr) ) )                   
                (TypeBaseType BaseType_string, TypeBaseType BaseType_char) -> 
                    return (env, (StmtAssign bExpr (CharToString expr) ) )  
                
                -- cases in which an error was previously generated: no new error messages
                (TypeBaseType BaseType_error, _) -> 
                    return (env, (StmtAssign bExpr expr ) )
                (_, TypeBaseType BaseType_error) -> 
                    return (env, (StmtAssign bExpr expr ) )

                --TODO: implementare posizione con il modo che avevamo discusso
                -- Durante il parsing le funzioni restituiscono anche 2 posizioni (quella più a sx e quella più a dx)
                

                _ -> do 
                    state <- get
                    put $ state { errors = ("Error in range " ++ show posEnds ++ ". l-Expression "++showExpr bExpr++" is of type " ++ show t ++ " but it is assigned value of type "++show rtype ++"."):(errors state)}
                    return (env, StmtAssign bExpr (IntToReal expr)) 


    where
        -- Tipo del valore che si vuole assegnare
        rtype = getTypeFromExpression expr

--TODO: a cosa serviva questa parte? scrivetemelo su whatsapp
-- generic expression that is not a base expression
{-parseArrayAssignment idexpr ltype iexpr expr env errs = if ltype == getTypeFromExpression expr --TODO: risolvere errore assegnamento array ad array di array (vedere testfile4)
    then
        return (env, errs, (StmtAssign idexpr expr) )
    else
        -- 2 cases: a) int to real, b) incompatible types -> error
        case (ltype, getTypeFromExpression expr) of
            (TypeBaseType BaseType_real, TypeBaseType BaseType_integer) -> return (env, errs, (StmtAssign idexpr (IntToReal expr) ) )
            otherwise -> return (env, ("Error. l-Expression is of type " ++ show ltype  ++ " but it is assigned value of type "++show (getTypeFromExpression expr) ++"."):errs, (StmtAssign idexpr expr) )
-}

-- check if literal type matches with the one saved in the environment. 
-- If it doesn't return current environment and a new error message
parseLitAssignment:: TokIdent -> Literal -> Env -> PosEnds -> SSAState (Env, Stmt Env Type)
parseLitAssignment tkId@(TokIdent (idPos, idVal)) literal env posEnds = case Env.lookup idVal env of
    
    Just (Variable mod envPos envType addr) ->
        if envType == TypeBaseType litType
            then return (
                env,
                -- NOTICE HOW WE ANNOTATE THE TREE, saving info about type of expr!
                StmtAssign (BaseExpr (Identifier tkId) envType) (ExprLiteral literal)
                )
            else case (envType, litType ) of
                -- 3 cases: casting int->real, char->string or incompatible types
                (TypeBaseType BaseType_real, BaseType_integer) -> return (env, StmtAssign (BaseExpr (Identifier tkId) envType) (IntToReal (ExprLiteral literal)) )
                (TypeBaseType BaseType_string, BaseType_char) -> return (env, StmtAssign (BaseExpr (Identifier tkId) envType) (CharToString (ExprLiteral literal)) )

                -- cases in which an error was previously generated: no new error messages, pass the error forwards
                (TypeBaseType BaseType_error, _) -> 
                    return (env, StmtAssign (BaseExpr (Identifier tkId) (TypeBaseType BaseType_error)) (ExprLiteral literal) )
                (_, BaseType_error) -> 
                    return (env, StmtAssign (BaseExpr (Identifier tkId) (TypeBaseType BaseType_error)) (ExprLiteral literal) )

                -- In case of error it is added to the state
                (_, _)  -> do
                        state <- get
                        put $ state { errors = ("Error in range " ++ show posEnds ++ ". id "++ idVal ++ " is of type " ++ show envType ++ " but is assigned value of type " ++ show litType):(errors state)}
                        return (env, StmtAssign (BaseExpr (Identifier tkId) (TypeBaseType BaseType_error)) (ExprLiteral literal))

    
    Nothing -> do
        state <- get
        put $ state { errors = ("Error in ragne " ++ show posEnds ++
                ". Unknown identifier: " ++ idVal ++
                " is used but has never been declared."):(errors state)}
        return (env, StmtAssign (BaseExpr (Identifier tkId) (TypeBaseType BaseType_error)) (ExprLiteral literal))

    where
        litType = getTypeFromLiteral literal


-- Given identifier and expression (already parsed with inferred type!) assigns type to token of identifier
parseIdExprAssignment :: TokIdent -> EXPR Type -> Env -> PosEnds -> SSAState (Env, Stmt Env Type)
parseIdExprAssignment tkId@(TokIdent (idPos, idVal)) expr env posEnds = case Env.lookup idVal env of
    Just (Variable mod envPos envType addr) ->
        if envType == exprType
            
            then return (
                env,
                StmtAssign (BaseExpr (Identifier tkId) envType) expr -- annoto literal con il tipo corretto
                )
            
            else case (envType, exprType ) of
                -- 3 cases: 1) casting int->real, 2) casting char->string, 3) incompatible types
                (TypeBaseType BaseType_real, TypeBaseType BaseType_integer) -> 
                    return (env, StmtAssign (BaseExpr (Identifier tkId) envType) (IntToReal expr) )
                (TypeBaseType BaseType_string, TypeBaseType BaseType_char) -> 
                    return (env, StmtAssign (BaseExpr (Identifier tkId) envType) (CharToString expr) )

                -- cases in which an error was previously generated: no new error messages, pass the error forwards
                (TypeBaseType BaseType_error, _) -> 
                    return (env, StmtAssign (BaseExpr (Identifier tkId) (TypeBaseType BaseType_error)) expr )
                (_, TypeBaseType BaseType_error) -> 
                    return (env, StmtAssign (BaseExpr (Identifier tkId) (TypeBaseType BaseType_error)) expr )

                (_, _)  -> do
                    state <- get
                    put $ state { errors = ("Error in range " ++ show posEnds ++ ". id "++ idVal ++ " is of type " ++ show envType ++ " but is assigned value of type " ++ show exprType):(errors state)}
                    return (
                        env, 
                        StmtAssign (BaseExpr (Identifier tkId) envType) expr 
                        )

    Nothing -> do
        state <- get
        put $ state { errors = ("Error in range " ++ show posEnds ++
            ". Unknown identifier: " ++ idVal ++ " is used but has never been declared."):(errors state)}
        return (
            env,
            StmtAssign (BaseExpr (Identifier tkId) (TypeBaseType BaseType_error)) expr
            )

    where
        exprType = getTypeFromExpression expr


parseExprExprAssignment :: EXPR Type -> EXPR Type -> Env -> PosEnds -> SSAState (Env, Stmt Env Type)
parseExprExprAssignment expr1 expr2 env posEnds = 
    -- TODO: includere posizione e stringa dell'espressione sinistra nel messaggio di errore
    if getTypeFromExpression expr1 == getTypeFromExpression expr2
        then return (env, (StmtAssign expr1 expr2) )
        else case (getTypeFromExpression expr1, getTypeFromExpression expr2) of
            
            (TypeBaseType BaseType_real, TypeBaseType BaseType_integer) -> 
                return (env, (StmtAssign expr1 (IntToReal expr2)))
            (TypeBaseType BaseType_string, TypeBaseType BaseType_char) -> 
                return (env, (StmtAssign expr1 (CharToString expr2)))

            -- cases in which an error was previously generated: no new error messages, pass the error forwards
            (TypeBaseType BaseType_error, _) -> 
                return (env, (StmtAssign expr1 expr2))
            (_, TypeBaseType BaseType_error) -> 
                return (env, (StmtAssign expr1 expr2))
        
            _ -> do
                state <- get
                put $ state { errors = ("Error in range " ++ show posEnds ++ ". l-Expression "++ showExpr expr1 ++ " is of type " ++ show (getTypeFromExpression expr1) ++ " but is assigned value of type " ++ show (getTypeFromExpression expr2)):(errors state)}
                return (env, (StmtAssign expr1 expr2))


-- Given current environment, errors and syntax tree, returns annotated tree and updated environment and errors
parseExpression :: Env -> EXPR infType -> SSAState (Env, EXPR Type, PosEnds)

--TODO: ottenere informazione sulla posizione per stamparla nel messaggio di errore per tutti i casi
--TODO: evitare messaggi di errore indotti (conseguenza di assegnazioni del tipo error)

-- Boolean Unary Negation
parseExpression env (UnaryExpression Not exp t) = do
    (env2, parsedexp, posEnds) <- parseExpression env exp
    if getTypeFromExpression parsedexp == TypeBaseType BaseType_boolean || getTypeFromExpression parsedexp == TypeBaseType BaseType_error 
        then -- no new errors are generated
            return (
                env2,  
                (UnaryExpression Not parsedexp (sup (getTypeFromExpression parsedexp) (TypeBaseType BaseType_boolean)) ),
                posEnds
                )
        else do
            state <- get
            put $ state { errors = ("Error"++". Boolean negation 'not' applied expression of type " ++ show (getTypeFromExpression parsedexp) ++ " instead of boolean type."):(errors state)}
            return (
                env2, 
                (UnaryExpression Not parsedexp (TypeBaseType BaseType_error) ),
                posEnds
                )
        

-- Arithmetic Unary Negation
parseExpression env (UnaryExpression Negation exp t) = do
    (env2, parsedexp, posEnds) <- parseExpression env exp
    --TODO: come viene gestito se il tipo errore? No dovrebbe poi essere restituito poi l'errore invece che il tipo integer
    if getTypeFromExpression parsedexp == TypeBaseType BaseType_integer || getTypeFromExpression parsedexp == TypeBaseType BaseType_real || getTypeFromExpression parsedexp == TypeBaseType BaseType_error 
        then
            return (
                env2,
                (UnaryExpression Negation parsedexp (sup (getTypeFromExpression parsedexp) (TypeBaseType BaseType_integer)) ),
                posEnds
                )
        else do
            state <- get
            put $ state { errors = ("Error"++". Arithmetic unary minus '-' applied to type " ++ show (getTypeFromExpression parsedexp) ++ " instead of numeric type."):(errors state)}
            return (
                env2, 
                (UnaryExpression Negation parsedexp (TypeBaseType BaseType_error) ),
                posEnds 
                )
        

-- Binary Boolean operations (And,Or)
parseExpression env (BinaryExpression And exp1 exp2 t) = parseBinaryBooleanExpression env And exp1 exp2
parseExpression env (BinaryExpression Or exp1 exp2 t) = parseBinaryBooleanExpression env Or exp1 exp2

-- Binary Arithmetic operations (Add,Sub,Mul,Div,Mod)
parseExpression env (BinaryExpression Add exp1 exp2 t) = parseBinaryArithmeticExpression env Add exp1 exp2
parseExpression env (BinaryExpression Sub exp1 exp2 t) = parseBinaryArithmeticExpression env Sub exp1 exp2
parseExpression env (BinaryExpression Mul exp1 exp2 t) = parseBinaryArithmeticExpression env Mul exp1 exp2
parseExpression env (BinaryExpression Div exp1 exp2 t) = parseBinaryArithmeticExpression env Div exp1 exp2
parseExpression env (BinaryExpression Mod exp1 exp2 t) = parseBinaryArithmeticExpression env Mod exp1 exp2

-- Binary numeric relations (Eq,NotEq,LessT,EqLessT,GreatT,EqGreatT)
parseExpression env (BinaryExpression Eq exp1 exp2 t) = parseBinaryRelationExpression env Eq exp1 exp2
parseExpression env (BinaryExpression NotEq exp1 exp2 t) = parseBinaryRelationExpression env NotEq exp1 exp2
parseExpression env (BinaryExpression LessT exp1 exp2 t) = parseBinaryRelationExpression env LessT exp1 exp2
parseExpression env (BinaryExpression EqLessT exp1 exp2 t) = parseBinaryRelationExpression env EqLessT exp1 exp2
parseExpression env (BinaryExpression GreatT exp1 exp2 t) = parseBinaryRelationExpression env GreatT exp1 exp2
parseExpression env (BinaryExpression EqGreatT exp1 exp2 t) = parseBinaryRelationExpression env EqGreatT exp1 exp2

-- Dereference
parseExpression env (UnaryExpression Dereference exp t)  = do 
    (env2, parsedexp, posEnds) <- parseExpression env exp

    --TODO: aggiornare posEnds, allunga a destra di 1 colonna per l'operatore ^

    if checkLExpr parsedexp
        then
            case getTypeFromExpression parsedexp of
                (TypeBaseType BaseType_error) -> 
                    return (env2, (UnaryExpression Dereference parsedexp (TypeBaseType BaseType_error)), posEnds )

                (TypeCompType (Pointer pointedType) ) -> 
                    return (env2, (UnaryExpression Dereference parsedexp pointedType), posEnds )

                _ -> do
                    state <- get
                    --TODO: nonostante questo errore, viene segnato errore anche nella chiamata della funzione dove viene usata l'espressione
                    put $ state { errors = ("Error at "++ show posEnds ++": Expression "++ showExpr exp ++" should be of type pointer, but instead is of type "++show (getTypeFromExpression parsedexp)):(errors state)}
                    return (env2, (UnaryExpression Dereference parsedexp (TypeBaseType BaseType_error)), posEnds )
             
        else do
            state <- get
            --TODO: nonostante questo errore, viene segnato errore anche nella chiamata della funzione dove viene usata l'espressione
            put $ state { errors = ("Error at "++ show posEnds ++": Invalid dereference '^' operation. "++ showExpr exp ++" is not a valid l-expression."):(errors state)}
            return (env2, (UnaryExpression Dereference parsedexp (TypeBaseType BaseType_error)), posEnds )


-- Reference
parseExpression env (UnaryExpression Reference exp t) = do
    (env2, parsedexp, posEnds) <- parseExpression env exp
    traceM $ "\n"++ show (getTypeFromExpression parsedexp)++"  "++ showExpr exp ++"\n"
    
    --TODO: aggiornare posEnds, allunga a sinistra di 1 colonna per l'operatore @

    if getTypeFromExpression parsedexp == TypeBaseType BaseType_error
        then 
            return (env2, (UnaryExpression Reference parsedexp (TypeBaseType BaseType_error)), posEnds )
        else 
            if checkLExpr parsedexp
                then return (env2, (UnaryExpression Reference parsedexp (TypeCompType (Pointer (getTypeFromExpression parsedexp)))), posEnds )
                else do
                    state <- get
                    --TODO: nonostante questo errore, viene segnato errore anche nella chiamata della funzione dove viene usata l'espressione
                    put $ state { errors = ("Error at "++ show posEnds ++": Invalid reference '@' operation. "++ showExpr exp ++" is not a valid l-expression."):(errors state)}
                    return (
                        env2, 
                        (UnaryExpression Reference parsedexp (TypeBaseType BaseType_error) ),
                        posEnds
                        )
    

-- Literals (base case of recursions)
parseExpression env (ExprLiteral literal) = do
    return (env, (ExprLiteral literal), posEnds )
    where
        posEnds = getLitPosEnds literal

-- Function calls
parseExpression env (ExprCall call t) = parseExpressionCall env call

-- Base Expressions: identifies or array elements
parseExpression env (BaseExpr bexpr t) = do
    (env2, parsedbexpr, t, posEnds) <- parseBaseExpression env bexpr
    return (env2, (BaseExpr parsedbexpr t), posEnds)
        

-- Type casted expressions: verify that types match
-- Integer to Real
parseExpression env (IntToReal expr) = do
    (env2, parsedexpr, posEnds) <- parseExpression env expr
    case getTypeFromExpression parsedexpr of
        
        TypeBaseType BaseType_integer -> return (env2, (IntToReal parsedexpr), posEnds)
        
        TypeBaseType BaseType_real -> do
            state <- get
            put $ state { errors = ("Warning: removed unneeded implicit type casting from Integer to Real"):(errors state)}
            return (env2, parsedexpr, posEnds) -- expression is real already: remove type casting wrapper
        
        TypeBaseType BaseType_error -> return (env2, (IntToReal parsedexpr), posEnds) -- in case of errors that already happened, no new error messages are generated

        _ -> do
            state <- get
            put $ state { errors = ("Error: type casting from Integer to Real applied to type " ++ show (getTypeFromExpression parsedexpr) ++ "."):(errors state)}
            return ( env2, (IntToReal parsedexpr), posEnds )

-- Char to String
parseExpression env (CharToString expr) = do
    (env2, parsedexpr, posEnds) <- parseExpression env expr
    case getTypeFromExpression parsedexpr of
        
        TypeBaseType BaseType_char -> return (env2, (CharToString parsedexpr), posEnds)
        
        TypeBaseType BaseType_string -> do
            state <- get
            put $ state { errors = ("Warning: removed unneeded implicit type casting from Char to String"):(errors state)}
            return (env2, parsedexpr, posEnds) -- expression is string already: remove type casting wrapper
        TypeBaseType BaseType_error -> return (env2, (CharToString parsedexpr), posEnds) -- in case of errors that already happened, no new error messages are generated
        
        _ -> do
            state <- get
            put $ state { errors = ("Error: type casting from Char to String applied to type " ++ show (getTypeFromExpression parsedexpr) ++ "."):(errors state)}
            return ( env2, (CharToString parsedexpr), posEnds )


-- parseExpression env errs expr = return (env, errs, (ExprLiteral (LiteralInteger (TokInteger ((0,0), "10")))) ) -- temporaneamente ogni espressione non specificata diventa il numero 10 (ora ridondante)

parseStatementCall :: Env -> Call infType -> SSAState (Env, Stmt Env Type)
parseStatementCall env (CallArgs tkId@(TokIdent (tokpos,tokid)) args ) = do
    (env2, parsedcall, t, posEnds) <- parseCall env (CallArgs tkId args )
    return (env2, (StmtCall parsedcall) )

parseExpressionCall :: Env -> Call infType -> SSAState (Env, EXPR Type, PosEnds)
parseExpressionCall env (CallArgs tkId@(TokIdent (tokpos,tokid)) args ) = do
    (env2, parsedcall, t, posEnds) <- parseCall env (CallArgs tkId args )
    return (env2, (ExprCall parsedcall t), posEnds )

parseCall :: Env -> Call infType -> SSAState (Env, Call Type, Type, PosEnds)
parseCall env call@(CallArgs tkId@(TokIdent (tokpos@(x,y),tokid)) args ) = do
    --posEnds contiene la posizione dell'argomento più a destra
    (env2, parsedargs, posEndsArgs) <- parseArguments env args [] posEndsToken
    
    case Env.lookup tokid env of
        Just (Function pos parameters t _) -> do
            --TODO: implementare ritorno posEnds da parseFunctionCall
            (env, clType, tp) <- parseFunctionCall env2 (CallArgs tkId parsedargs) parameters t [] posEndsToken{rightmost = rightmost posEndsArgs}
            return (env, clType, tp, posEndsToken)
        
        Just (Procedure pos parameters _) -> do
            --TODO: implementare ritorno posEnds da parseProcedureCall
            (env, clType, tp) <- parseProcedureCall env2 (CallArgs tkId parsedargs) parameters [] posEndsToken{rightmost = rightmost posEndsArgs}
            return (env, clType, tp, posEndsToken)

        Just (Constant pos t addr) -> do
            state <- get
            put $ state { errors = ("Error at: " ++ show posEndsToken ++". Identifier " ++ tokid ++" is used as a function/procedure but it is a constant."):(errors state)}
            return (
                env2, 
                (CallArgs tkId parsedargs ), 
                (TypeBaseType BaseType_error), 
                posEndsToken 
                )
        
        Just (Variable mod pos t addr) -> do
            state <- get
            put $ state { errors = ("Error at: " ++ show posEndsToken ++". Identifier " ++ tokid ++" is used as a function/procedure but it is a variable."):(errors state)}
            return (
                env2,
                (CallArgs tkId parsedargs ),
                (TypeBaseType BaseType_error), 
                posEndsToken 
                ) 
        
        Nothing -> do
            state <- get
            put $ state { errors = ("Error at: " ++ show posEndsToken ++". Unknown identifier: " ++ tokid ++" is used but has never been declared."):(errors state)}
            return (
                env2,
                (CallArgs tkId parsedargs ), 
                (TypeBaseType BaseType_error), 
                posEndsToken 
                ) 

        where
            -----------------TODO------------------------- implementare posEnds per funzioni/procedure
            posEndsToken = PosEnds { leftmost = tokpos, rightmost = (x, y + length tokid) }
        

parseArguments :: Env -> [EXPR infType] -> [EXPR Type]-> PosEnds -> SSAState (Env, [EXPR Type], PosEnds)
parseArguments env [] res posEnds = do
    return (env, res, posEnds)
parseArguments env (arg:args) res posEnds = do
    (env2, parsedexpr, argPosEnds) <- parseExpression env arg
    parseArguments env2 args (res++[parsedexpr]) argPosEnds
        

-- Last parameter is list of parsed expressions (call arguments) that have been type casted if needed
-- Parameters: env, errors, call, params, parsedargs --TODO: dire nel messaggio di errore di mismatch quanti parametri sono previsti?
parseFunctionCall :: Env -> Call Type -> Prms -> Type -> [EXPR Type] -> PosEnds -> SSAState (Env, Call Type, Type)
parseFunctionCall env (CallArgs tkId@(TokIdent (tokpos,tokid)) [] ) NoParams t pargs posEnds = 
    return (env, (CallArgs tkId pargs ), t )

parseFunctionCall env (CallArgs tkId@(TokIdent (tokpos,tokid)) [] ) (Params prms) t pargs posEnds = do
    state <- get
    put $ state { errors = ("Error at " ++ show tokpos ++ " in function "++tokid++": mismatch in number of arguments"):(errors state)}
    return (
        env, 
        (CallArgs tkId pargs ), 
        (TypeBaseType BaseType_error) 
        )

parseFunctionCall env (CallArgs tkId@(TokIdent (tokpos,tokid)) args ) NoParams t pargs posEnds = do
    state <- get
    put $ state { errors = ("Error at " ++ show tokpos ++ " in function "++tokid++": mismatch in number of arguments"):(errors state)}
    return (
        env, 
        (CallArgs tkId (pargs++args) ), 
        (TypeBaseType BaseType_error) 
        )

parseFunctionCall env (CallArgs tkId@(TokIdent (tokpos,tokid)) args ) (Params (prm:prms) ) t pargs posEnds = do
    (env2, args2, pargs2) <- compareArguments env posEnds args prm pargs
    
    newparams <- case prms of
                    [] -> return NoParams
                    _ -> return (Params prms)

    parseFunctionCall env2 (CallArgs tkId args2 ) newparams t pargs2 posEnds
      
        

parseProcedureCall :: Env -> Call Type -> Prms -> [EXPR Type] -> PosEnds -> SSAState (Env, Call Type, Type)
parseProcedureCall env (CallArgs tkId@(TokIdent (tokpos,tokid)) [] ) NoParams pargs posEnds = 
    return (
        env, 
        (CallArgs tkId pargs ), 
        (TypeBaseType BaseType_void)
        )

parseProcedureCall env (CallArgs tkId@(TokIdent (tokpos,tokid)) [] ) (Params prms) pargs posEnds = do
    state <- get
    put $ state { errors = ("Error at " ++ show tokpos ++ " in procedure "++tokid++": mismatch in number of arguments"):(errors state)}
    return (
        env,
        (CallArgs tkId pargs ), 
        (TypeBaseType BaseType_error) 
        )

parseProcedureCall env (CallArgs tkId@(TokIdent (tokpos,tokid)) args ) NoParams pargs posEnds = do
    state <- get
    put $ state { errors = ("Error at " ++ show tokpos ++ " in procedure "++tokid++": mismatch in number of arguments" ):(errors state)}
    return (
        env,
        (CallArgs tkId (pargs++args) ), 
        (TypeBaseType BaseType_error) 
        )

parseProcedureCall env (CallArgs tkId@(TokIdent (tokpos,tokid)) args ) (Params (prm:prms) ) pargs posEnds = do
    (env2, args2, pargs2) <- compareArguments env posEnds args prm pargs
    newparams <- case prms of
                [] -> return NoParams
                _ -> return $ Params prms

    parseProcedureCall env2 (CallArgs tkId args2 ) newparams pargs2 posEnds



-- Parses arguments of the same type defined together and print accurate error messages with position of arguments
-- Boolean parameter keeps track of whether all arguments are of correct type, Last list of expressions are function call arguments type casted if needed
compareArguments :: Env -> PosEnds -> [EXPR Type] -> Prm -> [EXPR Type] -> SSAState (Env, [EXPR Type], [EXPR Type])
compareArguments env p [] (Param _ [] t) pargs = return (env, [] , pargs)
compareArguments env p args (Param _ [] t) pargs = return (env, args, pargs)
compareArguments env p [] (Param _ toks t) pargs = return (env, [], pargs)

compareArguments env p (expr:args) (Param m ((IdElement (TokIdent (parpos@(x,y),parid))):toks) t) pargs
    -- call by reference
    | m == Modality_ref =
        if not (checkLExpr expr) 
            then do
                state <- get
                put $ state { errors = ("Error at "++ show p ++ ": parameter " ++ parid ++ " is passed by reference as specified at " ++ show parPosEnds ++", but argument " ++ showExpr expr ++ " is not a valid l-value."):(errors state)}                
                compareArguments env p args (Param m toks t) (pargs++[expr])
            else if not (argExprType == t)
                    then do
                        state <- get
                        put $ state { errors = 
("Error at "++ show p ++": the argument "++ showExpr expr ++" is of type " ++ show argExprType ++ " but it should be of type " ++ show t ++" as specified by parameter "++ parid ++ " passed by reference at "++ show parPosEnds):(errors state)}                
                        compareArguments env p args (Param m toks t) (pargs++[expr])
                                
                    else
                        compareArguments env p args (Param m toks t) (pargs++[expr])
            
    -- call by value
    | otherwise = do

        -- confronto tra parametri, diversi casi: 1) int to real, 2) real to int, 3) incompatibile --TODO: type casting CharToString?
        if argExprType == t
            then
                compareArguments env p args (Param m toks t) (pargs++[expr])
            else
                case (argExprType, t) of
                    
                    (TypeBaseType BaseType_integer, TypeBaseType BaseType_real) -> 
                        compareArguments env p args (Param m toks t) (pargs++[(IntToReal expr)])
                    
                    (TypeBaseType BaseType_char, TypeBaseType BaseType_string) -> 
                        compareArguments env p args (Param m toks t) (pargs++[(CharToString expr)])

                    -- error in expression of parameter of function/procedure call, no new error messages are generated
                    (TypeBaseType BaseType_error, _) -> 
                        compareArguments env p args (Param m toks t) (pargs++[expr])
                    
                    _ -> do
                        state <- get
                        put $ state { errors = ("Error at "++ show p ++": the argument "++ showExpr expr ++" is of type " ++ show argExprType ++ " but it should be of type " ++ show t ++" as specified by parameter "++ parid ++ " at "++ show parPosEnds):(errors state)}
                        compareArguments env p args (Param m toks t) (pargs++[expr])
    where
        argExprType = getTypeFromExpression expr
        parPosEnds = PosEnds{ leftmost = parpos, rightmost = (x, y + length parid)}

checkLExpr :: EXPR Type -> Bool
checkLExpr (BaseExpr (Identifier _) _) = True
checkLExpr (UnaryExpression Dereference _ _) = True
--Reference non produce un l-value
--TODO: forse in realtà è giusto, devo
--checkLExpr (UnaryExpression Reference _ _) = True
checkLExpr (BaseExpr (ArrayElem _ _) _) = True
checkLExpr _ = False


parseBinaryBooleanExpression :: Env -> BinaryOperator -> EXPR infType -> EXPR infType -> SSAState (Env, EXPR Type, PosEnds)
parseBinaryBooleanExpression env op exp1 exp2 = do
    (env2, parsedexp1, posEndsL) <- parseExpression env exp1
    (env3, parsedexp2, posEndsR) <- parseExpression env2 exp2
    
    if getTypeFromExpression parsedexp1 /= TypeBaseType BaseType_boolean && getTypeFromExpression parsedexp1 /= TypeBaseType BaseType_error
        then do
            state <- get
            put $ state { errors = ("Error. " ++ "First argument of "++ getStringFromOperator op ++" operator is of type " ++ show (getTypeFromExpression parsedexp1) ++ " instead of boolean."):(errors state)}
            return (
                env3, 
                (BinaryExpression op parsedexp1 parsedexp2 (TypeBaseType BaseType_error) ),
                PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR}
                )
        
        else if getTypeFromExpression parsedexp2 /= TypeBaseType BaseType_boolean && getTypeFromExpression parsedexp2 /= TypeBaseType BaseType_error
            then do
                state <- get
                put $ state { errors = ("Error. " ++ "Second argument of "++ getStringFromOperator op ++" operator is of type " ++ show (getTypeFromExpression parsedexp2) ++ " instead of boolean."):(errors state)}
                return (
                    env3, 
                    (BinaryExpression op parsedexp1 parsedexp2 (TypeBaseType BaseType_error) ),
                    PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR}
                    )
            -- if one of the expressions is of type Error, no new messages are produced
            else return (
                    env3, 
                    (BinaryExpression op parsedexp1 parsedexp2 (sup (getTypeFromExpression parsedexp1) (getTypeFromExpression parsedexp2)) ), 
                    PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR})
    where
        -- string form of operator for printing error messages
        getStringFromOperator :: BinaryOperator -> [Char]
        getStringFromOperator And = "'And'"
        getStringFromOperator Or = "'Or'"
        getStringFromOperator _ = ""


parseBinaryArithmeticExpression :: Env -> BinaryOperator -> EXPR infType -> EXPR infType -> SSAState (Env, EXPR Type, PosEnds)
parseBinaryArithmeticExpression env op exp1 exp2 = do
    (env2, parsedexp1, posEndsL) <- parseExpression env exp1
    (env3, parsedexp2, posEndsR) <- parseExpression env2 exp2
    
    -- 3 cases: 1) first element not numeric, 2) second argument not numeric, 3) no errors
    if getTypeFromExpression parsedexp1 /= TypeBaseType BaseType_integer && getTypeFromExpression parsedexp1 /= TypeBaseType BaseType_real && getTypeFromExpression parsedexp1 /= TypeBaseType BaseType_error
        then do
            state <- get
            put $ state { errors = ("Error at " ++ show PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR}  ++ ": First argument of "++ getStringFromOperator op ++ " operator is of type " ++ show (getTypeFromExpression parsedexp1) ++ " instead of numeric (integer or real)."):(errors state)}
            return (
                env3,  
                (BinaryExpression op parsedexp1 parsedexp2 (TypeBaseType BaseType_error) ),
                PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR}
                )
        --TODO: se ho un'espressione del tipo "true+false" viene segnalato come errato soltanto il primo argomento e non il secondo
        else if getTypeFromExpression parsedexp2 /= TypeBaseType BaseType_integer && getTypeFromExpression parsedexp2 /= TypeBaseType BaseType_real && getTypeFromExpression parsedexp2 /= TypeBaseType BaseType_error
            then do 
                state <- get
                put $ state { errors = ("Error. " ++ "Second argument of "++getStringFromOperator op++" operator is of type " ++ show (getTypeFromExpression parsedexp2) ++ " instead of numeric (integer or real)."):(errors state)}
                return (
                    env3, 
                    (BinaryExpression op parsedexp1 parsedexp2 (TypeBaseType BaseType_error) ),
                    PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR}
                    )
            
            -- 3 casi: 1) primo int secondo real, 2) primo real secondo int, 3) no type casting
            else case (getTypeFromExpression parsedexp1, getTypeFromExpression parsedexp2) of
                    
                    (TypeBaseType BaseType_integer, TypeBaseType BaseType_real) -> 
                        return (env3, (BinaryExpression op (IntToReal parsedexp1) parsedexp2 (getType op parsedexp1 parsedexp2) ), PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR})
                    
                    (TypeBaseType BaseType_real, TypeBaseType BaseType_integer) -> 
                        return (env3, (BinaryExpression op parsedexp1 (IntToReal parsedexp2) (getType op parsedexp1 parsedexp2) ), PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR})
                   
                    _ -> return (env3, (BinaryExpression op parsedexp1 parsedexp2 (getType op parsedexp1 parsedexp2) ), PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR})                   
    where
        -- string form of operator for printing error messages
        getStringFromOperator :: BinaryOperator -> [Char]
        getStringFromOperator Add = "'+'"
        getStringFromOperator Sub = "'-'"
        getStringFromOperator Mul = "'*'"
        getStringFromOperator Div = "'/'"
        getStringFromOperator Mod = "'mod'"
        getStringFromOperator _ = ""
        -- division has always result of real type --   TODO: mantenere questa distinzione?
        getType :: BinaryOperator -> EXPR Type -> EXPR Type -> Type
        getType Div _ _ = TypeBaseType BaseType_real
        getType _ parsedexp1 parsedexp2 = (sup (getTypeFromExpression parsedexp1) (getTypeFromExpression parsedexp2) )


parseBinaryRelationExpression :: Env -> BinaryOperator -> EXPR infType -> EXPR infType -> SSAState (Env, EXPR Type, PosEnds)
parseBinaryRelationExpression env op exp1 exp2 = do
    (env2, parsedexp1, posEndsL) <- parseExpression env  exp1
    (env3, parsedexp2, posEndsR) <- parseExpression env2 exp2
    
    -- 3 cases: 1) first element not numeric, 2) second argument not numeric, 3) no errors
    if getTypeFromExpression parsedexp1 /= TypeBaseType BaseType_integer && getTypeFromExpression parsedexp1 /= TypeBaseType BaseType_real && getTypeFromExpression parsedexp1 /= TypeBaseType BaseType_error 
        then do
            state <- get
            put $ state { errors = ("Error. " ++ "First argument of "++ getStringFromOperator op ++ "operator is of type " ++ show (getTypeFromExpression parsedexp1) ++ " instead of numeric (integer or real)."):(errors state)}
            return (
                env3,  
                (BinaryExpression op parsedexp1 parsedexp2 (TypeBaseType BaseType_error) ),
                PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR}
                )

        else if getTypeFromExpression parsedexp2 /= TypeBaseType BaseType_integer && getTypeFromExpression parsedexp2 /= TypeBaseType BaseType_real && getTypeFromExpression parsedexp2 /= TypeBaseType BaseType_error 
            then do
                state <- get
                put $ state { errors = ("Error. " ++ "Second argument of "++getStringFromOperator op++" operator is of type " ++ show (getTypeFromExpression parsedexp2) ++ " instead of numeric (integer or real)."):(errors state)}
                return (
                    env3,
                    (BinaryExpression op parsedexp1 parsedexp2 (TypeBaseType BaseType_error) ),
                    PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR}
                    )
            
             -- result is of boolean type unless one of the expressions contains an error
            else if getTypeFromExpression parsedexp1 == TypeBaseType BaseType_error || getTypeFromExpression parsedexp2 == TypeBaseType BaseType_error
                then return (
                        env3,  
                        (BinaryExpression op parsedexp1 parsedexp2 (TypeBaseType BaseType_error) ),
                        PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR}
                        ) 
                else return (
                        env3,  
                        (BinaryExpression op parsedexp1 parsedexp2 (TypeBaseType BaseType_boolean) ),
                        PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR}
                        )
                
    where

        -- string form of operator for printing error messages 
        getStringFromOperator :: BinaryOperator -> [Char]
        getStringFromOperator Eq = "'='"
        getStringFromOperator NotEq = "'<>'"
        getStringFromOperator LessT = "'<'"
        getStringFromOperator EqLessT = "'<='"
        getStringFromOperator GreatT = "'>'"
        getStringFromOperator EqGreatT = "'>='"
        getStringFromOperator _ = ""


-- output is of type (BEXPR Type). If (EXPR Type) output is required use parseExpression
parseBaseExpression:: Env -> BEXPR infType -> SSAState (Env, BEXPR Type, Type, PosEnds)
parseBaseExpression env (Identifier tkId@(TokIdent (tokpos@(x,y),tokid)) ) = 
    case Env.lookup tokid env of
        
        Just (Variable mod _ envType addr) -> return (env, (Identifier tkId ), envType, posEnds)
        
        Just (Constant _ envType addr) -> return (env, (Identifier tkId ), envType, posEnds)
        
        Nothing -> do
            state <- get
            put $ state { errors = ("Error at " ++ show tokpos ++". Unknown identifier: " ++ tokid ++" is used but has never been declared."):(errors state)}
            return (
                env, 
                (Identifier tkId), 
                TypeBaseType BaseType_error,
                posEnds
                )

        where
            posEnds = PosEnds { leftmost = tokpos, rightmost = (x, y + length tokid) }

parseBaseExpression env (ArrayElem bexpr iexpr) = do
    --TODO: con parsediexpr sarebbe necessario verifiicare se l'indice rientra entro i limiti dell'array
    (env2, parsediexpr, posEndsR) <- parseExpression env iexpr -- parsing of index for type checking (and casting if needed)
    (env3, parsedbexpr, posEndsL) <- parseExpression env2 bexpr -- parsing of base expression to get its type: if it is an array type, return type of element of that array; otherwise it is an error

    case (getTypeFromExpression parsedbexpr, getTypeFromExpression parsediexpr) of -- TODO: refactoring possibile di questa parte di codice? --TODO: posizione nei messaggi di errore
        
        -- 4 cases: 1) array and integer; 2) array and error; 3) error and integer; 4) error and error (distinction necessary to generate appropriate error messages)
        -- + 2 cases of Error types: do not print any new error messages
        (_, TypeBaseType BaseType_error) -> 
            return (
                env3, 
                (ArrayElem parsedbexpr parsediexpr), 
                TypeBaseType BaseType_error,
                PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR}
                )

        (TypeBaseType BaseType_error, _) -> 
            return (
                env3,
                (ArrayElem parsedbexpr parsediexpr), 
                TypeBaseType BaseType_error,
                PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR}
                )
        
        (TypeCompType (Array i1 i2 basetype), TypeBaseType BaseType_integer) -> 
            return (env3, (ArrayElem parsedbexpr parsediexpr), basetype, PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR})

        (TypeCompType (Array i1 i2 basetype), _) -> do
            state <- get
            put $ state { errors = ("Error. Array index is not of numeric type but it is of type "++show (getTypeFromExpression parsediexpr)++"."):(errors state)}
            return (
                env3, 
                (ArrayElem parsedbexpr parsediexpr), 
                TypeBaseType BaseType_error,
                PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR}
                )
        
        (_, TypeBaseType BaseType_integer) -> do
            state <- get
            put $ state { errors = ("Error. Expression " ++ showExpr parsedbexpr ++ " is treated as an array but it is of type " ++ show (getTypeFromExpression parsedbexpr) ++ "."):(errors state)}
            return (
                env3, 
                (ArrayElem parsedbexpr parsediexpr), 
                TypeBaseType BaseType_error,
                PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR}
                )
        
        _ -> do
            state <- get
            put $ state { errors = ("Error. Expression " ++ showExpr parsedbexpr ++ " is treated as an array but it is of type" ++ show (getTypeFromExpression parsedbexpr) ++ "."):("Error. Array index is not of numeric type but it is of type "++show (getTypeFromExpression parsediexpr)++"."):(errors state)}
            return (
                env3,
                (ArrayElem parsedbexpr parsediexpr), 
                TypeBaseType BaseType_error,
                PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR}
                )

