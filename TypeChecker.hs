{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module TypeChecker where
import AbsGrammar
import Env
import HelperTAC
import Control.Monad.State.Strict
import Debug.Trace
import Data.Map
import Errs

launchStatSemAnalysis :: P env infType -> (Env, Problems, P Env Type)
launchStatSemAnalysis tree = (env, errors finalState, parsedTree)
    where
        ((env, parsedTree), finalState) = runState (parseTree defaultEnv tree) ssaState
        ssaState = SSAStateStruct {idCount = 0, labelCount = 0, errors = emptyProblems, unInitVars = newStack }

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
        then put $ state{errors = ((Error, ReturnInMain):(errors state))}
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
        else put $ state{errors = ((Error, MissingReturnInFunction pos id):(errors state))}

    return (finalEnv, DclBlockFcBlock (FuncBlock idTok params retType annotatedBEB))
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
        then put $ state{errors = ((Error, UnexpectedReturnInProcedure pos id ):(errors state))}

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
            case typ of
                --TODO: gestire inizializzazione array
                (TypeCompType (Array {}) ) -> do
                    (newEnv, newIds) <- parseIds ids mod typ tmpEnv isVar
                    return (newEnv, idElem:newIds)
                _ -> do
                    insertVar id (Variable mod pos typ idAddr)
                    (newEnv, newIds) <- parseIds ids mod typ tmpEnv isVar
                    return (newEnv, idElem:newIds)

        else do
            (newEnv, newIds) <- parseIds ids mod typ tmpEnv isVar
            return (newEnv, idElem:newIds)

    where
        bitVector = zeros (lengthForBitVector typ)

-- parse the begin-end block and check the statements for type errors
parseBEBlock:: Env -> BEBlock env infType -> SSAState (Env, BEBlock Env Type, Bool)
parseBEBlock env (BegEndBlock statements annEnv)  = do
    pushNewUninit
    tmpEnv <- parseStatementsFirstPass emptyEnv statements
    mergedEnv <- Env.mergeEnvs tmpEnv env
    (newEnv, newStatements, hasAllReturns) <- parseStatements mergedEnv statements
    popUninit
    return (newEnv, BegEndBlock newStatements newEnv, hasAllReturns)


parseStatementsFirstPass :: Env -> [Stmt env infType] -> SSAState (Env)
parseStatementsFirstPass env ((StmtDecl dclBlock):stmts) = do
    (newEnv, tmpDclBlocks) <- parseDclBlocksFirstPass env [dclBlock]
    parseStatementsFirstPass newEnv stmts

parseStatementsFirstPass env (_:stmts) = parseStatementsFirstPass env stmts
parseStatementsFirstPass env _ = return env


-- Parses a list of statements and annotates the tree
parseStatements :: Env -> [Stmt env infType] -> SSAState (Env, [Stmt Env Type], Bool)
parseStatements env [] = return (env, [], False)
parseStatements env allStmts = q env allStmts [] False
        where
            -- helper function that calls parseStatement for every element in the list of statements
            q::Env -> [Stmt env infType] -> [Stmt Env Type] -> Bool -> SSAState (Env, [Stmt Env Type], Bool)
            q env [] annStmts accReturn = return (env, annStmts, accReturn)
            q env (s:xs) annStmts accReturn = do
                (env1, annStmt, isReturn) <- parseStatement s env
                q env1 xs (annStmts++[annStmt]) (accReturn || isReturn)


-- Parse a single statement and return the annotated tree
parseStatement :: Stmt stmtenv infType -> Env  ->  SSAState (Env, Stmt Env Type, Bool)
parseStatement stmt env = case stmt of
            -- Types of statements: declaration, block, assignment, function/procedure call, select, iter, return, break, continue
            -- parse and annotate the content of the statement and return the annotated tree

            -- Declaration
            (StmtDecl dclblock) -> do
                (env2, blocks) <- parseDclBlocks env [dclblock]
                return (env2, (StmtDecl (head blocks)), False)

            -- Block
            (StmtComp beblock) -> do
                (env2, block, hasAllReturns) <- parseBEBlock env beblock
                return (env2, (StmtComp block), hasAllReturns)

            -- Assignment
            (StmtAssign expr1 expr2) -> do
                (env2, parsedStmt) <- parseAssignment expr1 expr2 env
                return (env2, parsedStmt, False)

            -- Iteration
            (StmtIter iter) -> parseIter (StmtIter iter) env

            -- Return
            (StmtReturn rt)  -> do
                (env2, parsedStmt) <- parseReturn (StmtReturn rt) env
                return (env2, parsedStmt, True)

            -- Select
            (StmtSelect sel) -> parseSelection (StmtSelect sel) env

            -- Function/Procedure call
            (StmtCall call) -> do
                (env2, parsedStmt) <- parseStatementCall env call
                return (env2, parsedStmt, False)

            -- Break
            StmtBreak -> do
                state <- get
                case Env.lookup "break" env of
                    -- break inside loop, this is ok
                    Just (InsideLoop _) -> return (env, StmtBreak, False)
                    _ -> do -- error otherwise 
                        put $ state {errors = ((Error, BreakOutsideLoop):(errors state))}
                        return (env, StmtBreak, False)

            -- Continue
            StmtContinue -> do
                state <- get
                case Env.lookup "continue" env of
                    -- continue inside loop, this is ok
                    Just (InsideLoop _) -> return (env, StmtContinue, False)
                    _ -> do -- error otherwise 
                        put $ state {errors = ((Error, ContinueOutsideLoop):(errors state))}
                        return (env, StmtContinue, False)


-- Parse iteration statement and return the annotated tree
parseIter :: Stmt env infType -> Env -> SSAState (Env, Stmt Env Type, Bool)
-- parsing of while-do statement
parseIter (StmtIter (StmtWhileDo expr stmt)) env  = do
    (env1, parsedExpr, posEnds) <- parseExpression env expr

    (brLab, ctLab) <- newBreakContLabels
    env2 <- Env.insert "break" (InsideLoop brLab) env1
    env3 <- Env.insert "continue" (InsideLoop ctLab) env2

    (newEnv, parsedStmt, isReturn) <- parseStatement stmt env3
    let wrappedStmt = wrapInBeginEnd parsedStmt newEnv

    typeExpr <- getTypeFromExpression parsedExpr

    if typeExpr == TypeBaseType BaseType_boolean || typeExpr == TypeBaseType BaseType_error
        then return (env, StmtIter (StmtWhileDo parsedExpr wrappedStmt), isReturn)
        else do
            state <- get
            exprTp <- getTypeFromExpression parsedExpr
            put $ state {errors = ((Error, TypeMismatchIter
                                    (show posEnds)
                                    (showExpr parsedExpr)
                                    (show exprTp)):(errors state))}

            return (env, StmtIter (StmtWhileDo parsedExpr wrappedStmt), isReturn)

-- parsing of repeat-until statement
parseIter (StmtIter (StmtRepeat stmt expr)) env  = do

    (brLab, ctLab) <- newBreakContLabels
    env1 <- Env.insert "break" (InsideLoop brLab) env
    env2 <- Env.insert "continue" (InsideLoop ctLab) env1

    (env3, parsedStmt, isReturn) <- parseStatement stmt env2
    let wrappedStmt = wrapInBeginEnd parsedStmt env3
    (newEnv, parsedExpr, posEnds) <- parseExpression env3 expr

    typeExpr <- getTypeFromExpression parsedExpr

    if typeExpr == TypeBaseType BaseType_boolean || typeExpr == TypeBaseType BaseType_error
        then return (env, StmtIter (StmtRepeat wrappedStmt parsedExpr), isReturn)
        else do
            state <- get
            exprTp <- getTypeFromExpression parsedExpr
            put $ state {errors = ((Error, TypeMismatchIter
                                    (show posEnds)
                                    (showExpr parsedExpr)
                                    (show exprTp)):(errors state))}
            return (env, StmtIter (StmtRepeat wrappedStmt parsedExpr), isReturn)

-- parsing of for iteration statement
parseIter (StmtIter (StmtFor condVar initExpr forDirection limitExpr stmt)) env  = do
    (env1, parsedAssign) <- parseAssignment condVar initExpr env
    (env2, parsedLimitExpr, posEnds) <- parseExpression env1 limitExpr
    limitExprType <- getTypeFromExpression parsedLimitExpr

    (env3, parsedStmt, isReturn) <- parseStatement stmt env2
    let wrappedStmt = wrapInBeginEnd parsedStmt env3

    case parsedAssign of
        (StmtAssign parsedCondVar@(BaseExpr (Identifier _) condVarType) parsedInitExpr) -> do

            if sup condVarType limitExprType == TypeBaseType BaseType_integer
                then do
                    return (
                        env,
                        StmtIter (StmtFor parsedCondVar parsedInitExpr forDirection parsedLimitExpr wrappedStmt),
                        isReturn)
                else do
                    case (condVarType, limitExprType) of
                        (TypeBaseType BaseType_integer, _) -> do
                            state <- get
                            --TODO: creare errore espressione assegnata al contatore non è intera
                            put $ state {errors = ((Error, ForLoopInvalidCounterAssignment):(errors state))}
                            return (
                                env,
                                StmtIter (StmtFor parsedCondVar parsedInitExpr forDirection parsedLimitExpr wrappedStmt),
                                isReturn)

                        (_, TypeBaseType BaseType_integer) -> do
                            state <- get
                            --TODO: creare errore contatore for non è intero
                            put $ state {errors = ((Error, ForLoopInvalidCounterType):(errors state))}
                            return (
                                env,
                                StmtIter (StmtFor parsedCondVar parsedInitExpr forDirection parsedLimitExpr wrappedStmt),
                                isReturn)
                        (_, _) -> do
                            state <- get
                            --TODO: creare errore ne contatore ne inizializzatore contatore sono interi
                            put $ state {errors = ((Error, ForLoopInvalidCounter):(errors state))}
                            return (
                                env,
                                StmtIter (StmtFor parsedCondVar parsedInitExpr forDirection parsedLimitExpr wrappedStmt),
                                isReturn)

        (StmtAssign condExpr initExpr) -> do
            state <- get
            --TODO: creare errore espressione limite for deve essere intera
            put $ state {errors = ((Error, ReturnInMain):(errors state))}
            return (
                env,
                StmtIter (StmtFor condExpr initExpr forDirection parsedLimitExpr wrappedStmt),
                isReturn)

        _ -> error "Internal Error: should always parsedAssign should always be StmtAssign"


-- Parse select statement and return the annotated tree
parseSelection :: Stmt env infType -> Env -> SSAState (Env, Stmt Env Type, Bool)
parseSelection (StmtSelect (StmtIf expr stmt)) env  = do
    (env1, parsedExpr, posEnds) <- parseExpression env expr
    (newEnv, parsedStmt, isReturn) <- parseStatement stmt env1
    let wrappedStmt = wrapInBeginEnd parsedStmt newEnv

    typeExpr <- getTypeFromExpression parsedExpr

    if typeExpr == TypeBaseType BaseType_boolean || typeExpr == TypeBaseType BaseType_error
        then return (newEnv, StmtSelect (StmtIf parsedExpr wrappedStmt), isReturn)
        else do
            state <- get
            exprTp <- getTypeFromExpression parsedExpr
            put $ state {errors = ((Error, TypeMismatchSelection
                                    (show posEnds)
                                    (showExpr parsedExpr)
                                    (show exprTp)):(errors state))}
            return (newEnv, StmtSelect (StmtIf parsedExpr wrappedStmt), isReturn)

parseSelection (StmtSelect (StmtIfElse expr stmt1 stmt2)) env  = do
    (env1, parsedExpr, posEnds) <- parseExpression env expr

    (newEnv1, parsedStmt1, isReturn1) <- parseStatement stmt1 env1
    let wrappedStmt1 = wrapInBeginEnd parsedStmt1 newEnv1
    (newEnv2, parsedStmt2, isReturn2) <- parseStatement stmt2 newEnv1
    let wrappedStmt2 = wrapInBeginEnd parsedStmt2 newEnv2

    typeExpr <- getTypeFromExpression parsedExpr

    if typeExpr == TypeBaseType BaseType_boolean || typeExpr == TypeBaseType BaseType_error
        then return (newEnv2, StmtSelect (StmtIfElse parsedExpr wrappedStmt1 wrappedStmt2), isReturn1 && isReturn2)
        else do
            state <- get
            exprTp <- getTypeFromExpression parsedExpr
            put $ state { errors = ((Error, TypeMismatchSelection
                                    (show posEnds)
                                    (showExpr parsedExpr)
                                    (show exprTp)):(errors state)) }
            return (newEnv2, StmtSelect (StmtIfElse parsedExpr wrappedStmt1 wrappedStmt2), isReturn1 && isReturn2)


-- If the provided statement is not insiede a begin-end block, wraps it in one
-- This is needed to simplify TAC generation by having to deal only with begin-end blocks containing statements
-- With the current grammar the body of a iteration (or an if-else) statement can be eather a single statement or a begin-end block with other statements
wrapInBeginEnd :: Stmt Env infType -> Env -> Stmt Env infType
wrapInBeginEnd (StmtComp (BegEndBlock stmts begEnv)) _ = StmtComp (BegEndBlock stmts begEnv)
wrapInBeginEnd stmt stmEnv = StmtComp (BegEndBlock [stmt] stmEnv)


-- Parse return statement and return the annotated tree
parseReturn :: Stmt env infType -> Env -> SSAState (Env, Stmt Env Type)
parseReturn (StmtReturn (Ret expr)) env = do
    (newEnv, parsedExpr, posEnds) <- parseExpression env expr
    typeExpr <- getTypeFromExpression parsedExpr

    case Env.lookup "return" env of

        Just (Return expectedType funName funPos) ->
            if sup expectedType (typeExpr) /= expectedType && typeExpr /= TypeBaseType BaseType_error
                then do
                    state <- get
                    exprTp <- getTypeFromExpression parsedExpr
                    put $ state { errors =
                        ((Error, TypeMismatchReturn
                                (show posEnds)
                                funName
                                (show expectedType)
                                (show exprTp)):(errors state))}
                    return ( newEnv, StmtReturn (Ret parsedExpr))
                else
                    -- everything is ok or the error did not happen here, return the parsed expression 
                    return (newEnv, StmtReturn (Ret parsedExpr))

        -- Theoretically this should never happen, 
        -- since the return type of the function is saved in the environment when the function definition is parsed
        Nothing -> do
            state <- get
            put $ state { errors = ((Error, ExpectedTypeNotFound (show posEnds)):(errors state))}
            return (newEnv, StmtReturn (Ret parsedExpr))


-- Parse assignment statement and return the annotated tree
parseAssignment :: EXPR infType -> EXPR infType -> Env -> SSAState (Env, Stmt Env Type)
parseAssignment expr1 expr2 env = case (expr1, expr2) of
            -- Cases of assignments:

            -- Assign a literal to a variable
            ( (BaseExpr (Identifier tId@(TokIdent (_,id))) tp), (ExprLiteral literal) ) -> do
                removeVar id
                (env2, parsedid, posEnds) <- parseExpression env (BaseExpr (Identifier tId) tp)
                parseLitAssignment tId literal env2 posEnds

            -- Assign a generic expression to a variable: need to parse the expression and check if the types are compatible
            ( (BaseExpr (Identifier tId@(TokIdent (_,id))) tp), expr ) -> do
                removeVar id
                (env2, parsedid, posEnds) <- parseExpression env (BaseExpr (Identifier tId) tp)
                (env3, parsedexpr, posEnds2) <- parseExpression env2 expr
                parseIdExprAssignment tId parsedexpr env3 posEnds --TODO: ricavare il range corretto, anche nei casi successivi

            -- Dereference of a pointer is a valid l-value
            ( (UnaryExpression Dereference expr1 t), expr2 ) -> do
                (env2, parsedexpr1, posEnds1) <- parseExpression env (UnaryExpression Dereference expr1 t)
                (env3, parsedexpr2, posEnds2) <- parseExpression env2 expr2
                parseExprExprAssignment parsedexpr1 parsedexpr2 env3 posEnds1

            -- Reference operation is not a valid l-value

            -- Array elements are valid l-values
            ( arr@(BaseExpr (ArrayElem idexpr iexpr) t), expr ) -> do
                (env2, parsedexpr, posEnds1) <- parseExpression env expr
                (env3, parsedarrexpr, posEnds2) <- parseExpression env2 arr

                parseArrayAssignment parsedarrexpr parsedexpr env3 posEnds1

            -- All the other possible expressions are invalid l-values
            ( expr1, expr2 ) -> do
                (env2, parsedexpr1, posEnds1) <- parseExpression env expr1
                (env3, parsedexpr2, posEnds2) <- parseExpression env2 expr2
                state <- get
                put $ state { errors = ((Error, InvalidLValueAssignment (show posEnds1) (showExpr expr1)):(errors state))}
                return (env3, (StmtAssign parsedexpr1 parsedexpr2) )


-- Checks if r-expression matches typing with an l-expression that is an element of an array
parseArrayAssignment:: EXPR Type -> EXPR Type -> Env -> PosEnds -> SSAState (Env, Stmt Env Type)
parseArrayAssignment bExpr@(BaseExpr (ArrayElem bbexpr iiexpr) t) expr env posEnds = do

    rtype <- getTypeFromExpression expr -- the type of the expression that is to be assigned

    if t == rtype
        then return (env, (StmtAssign bExpr expr)) -- if the type of the l-value and the expression match, then annotate the tree with the same type
        else
            -- if the types are not the same, 2 cases: a) type casting int to real, b) incompatible types -> error
            case (t, rtype) of

                (TypeBaseType BaseType_real, TypeBaseType BaseType_integer) ->
                    return (env, (StmtAssign bExpr (IntToReal expr) ) )

                -- if there is an error that was previously generated, don't generate new error messages but just pass the annotated tree
                (TypeBaseType BaseType_error, _) ->
                    return (env, (StmtAssign bExpr expr ) )
                (_, TypeBaseType BaseType_error) ->
                    return (env, (StmtAssign bExpr expr ) )

                -- the types are incompatible and none of them is the Error type -> generate new error message
                _ -> do
                    state <- get
                    put $ state { errors = ((Error, TypeMismatchArrayAssignment
                                            (show posEnds)
                                            (showExpr bExpr)
                                            (show t)
                                            (show rtype)):(errors state))}
                    return (env, StmtAssign bExpr expr)


-- Parse assignment of a literal to a variable denoted by just its identifier: check if literal type matches with the one saved in the environment
-- If it doesn't return current environment and a new error message
parseLitAssignment:: TokIdent -> Literal -> Env -> PosEnds -> SSAState (Env, Stmt Env Type)
parseLitAssignment tkId@(TokIdent (idPos, idVal)) literal env posEnds = case Env.lookup idVal env of

    Just (Variable mod envPos envType addr) ->
        if envType == TypeBaseType litType
            then return (
                env,
                StmtAssign (BaseExpr (Identifier tkId) envType) (ExprLiteral literal)
                )
            else case (envType, litType ) of
                -- 2 cases: casting int->real or incompatible types
                (TypeBaseType BaseType_real, BaseType_integer) -> return (env, StmtAssign (BaseExpr (Identifier tkId) envType) (IntToReal (ExprLiteral literal)) )

                -- cases in which an error was previously generated: no new error messages, pass the error forwards
                (TypeBaseType BaseType_error, _) ->
                    return (env, StmtAssign (BaseExpr (Identifier tkId) (TypeBaseType BaseType_error)) (ExprLiteral literal) )
                (_, BaseType_error) ->
                    return (env, StmtAssign (BaseExpr (Identifier tkId) (TypeBaseType BaseType_error)) (ExprLiteral literal) )

                -- In case of error it is added to the state
                (_, _)  -> do
                        state <- get
                        put $ state { errors = ((Error, TypeMismatchLiteral (show posEnds) idVal (showLiteral literal) (show litType) (show envType)):(errors state))}
                        return (env, StmtAssign (BaseExpr (Identifier tkId) (TypeBaseType BaseType_error)) (ExprLiteral literal))

    -- identifier was not found in the environment -> error, variable is not declared in this scope
    Nothing -> do
        state <- get
        put $ state { errors = ((Error, UnknownIdentifier (show posEnds) idVal):(errors state))}
        return (env, StmtAssign (BaseExpr (Identifier tkId) (TypeBaseType BaseType_error)) (ExprLiteral literal))

    where
        litType = getTypeFromLiteral literal


-- Given identifier and expression (already parsed with inferred type!) assigns type to token of identifier
parseIdExprAssignment :: TokIdent -> EXPR Type -> Env -> PosEnds -> SSAState (Env, Stmt Env Type)
parseIdExprAssignment tkId@(TokIdent (idPos, idVal)) expr env posEnds = do
    exprType <- getTypeFromExpression expr

    case Env.lookup idVal env of
        Just (Variable mod envPos envType addr) ->
            if envType == exprType

                then return (
                    env,
                    StmtAssign (BaseExpr (Identifier tkId) envType) expr -- annotate literal with the correct type
                    )

                else case (envType, exprType ) of
                    -- 2 cases: 1) type casting int to real, 2) incompatible types -> error
                    (TypeBaseType BaseType_real, TypeBaseType BaseType_integer) ->
                        return (env, StmtAssign (BaseExpr (Identifier tkId) envType) (IntToReal expr) )
                    -- cases in which an error was previously generated: no new error messages, pass the error forwards
                    (TypeBaseType BaseType_error, _) ->
                        return (env, StmtAssign (BaseExpr (Identifier tkId) (TypeBaseType BaseType_error)) expr )
                    (_, TypeBaseType BaseType_error) ->
                        return (env, StmtAssign (BaseExpr (Identifier tkId) (TypeBaseType BaseType_error)) expr )

                    (_, _)  -> do
                        state <- get
                        put $ state { errors = ((Error, TypeMismatchIdExprAssignment (show posEnds) idVal (show envType) (show exprType)):(errors state))}
                        return (
                            env,
                            StmtAssign (BaseExpr (Identifier tkId) envType) expr
                            )

        Nothing -> do
            state <- get
            put $ state { errors = ((Error, UnknownIdentifier (show posEnds) idVal):(errors state))}
            return (
                env,
                StmtAssign (BaseExpr (Identifier tkId) (TypeBaseType BaseType_error)) expr
                )



parseExprExprAssignment :: EXPR Type -> EXPR Type -> Env -> PosEnds -> SSAState (Env, Stmt Env Type)
parseExprExprAssignment expr1 expr2 env posEnds = do
    typeExpr1 <- getTypeFromExpression expr1
    typeExpr2 <- getTypeFromExpression expr2

    -- TODO: includere posizione e stringa dell'espressione sinistra nel messaggio di errore
    if typeExpr1 == typeExpr2
        then return (env, (StmtAssign expr1 expr2) )
        else case (typeExpr1, typeExpr2) of

            (TypeBaseType BaseType_real, TypeBaseType BaseType_integer) ->
                return (env, (StmtAssign expr1 (IntToReal expr2)))

            -- cases in which an error was previously generated: no new error messages, pass the error forwards
            (TypeBaseType BaseType_error, _) ->
                return (env, (StmtAssign expr1 expr2))
            (_, TypeBaseType BaseType_error) ->
                return (env, (StmtAssign expr1 expr2))

            _ -> do
                state <- get
                exprTp1 <- getTypeFromExpression expr1
                exprTp2 <- getTypeFromExpression expr2
                put $ state { errors = ((Error, TypeMismatchExprExprAssignment
                                        (show posEnds)
                                        (showExpr expr1)
                                        (show expr1)
                                        (show exprTp2)):(errors state))}
                return (env, (StmtAssign expr1 expr2))


-- Given current environment, errors and syntax tree, returns annotated tree and updated environment and errors
parseExpression :: Env -> EXPR infType -> SSAState (Env, EXPR Type, PosEnds)

-- Boolean Unary Negation
parseExpression env (UnaryExpression Not exp t) = do
    (env2, parsedexp, posEnds) <- parseExpression env exp
    typeExpr <- getTypeFromExpression parsedexp

    if typeExpr == TypeBaseType BaseType_boolean || typeExpr == TypeBaseType BaseType_error
        then -- no new errors are generated
            return (
                env2,
                (UnaryExpression Not parsedexp (sup (typeExpr) (TypeBaseType BaseType_boolean)) ),
                posEnds
                )
        else do
            state <- get
            exprTp <- getTypeFromExpression parsedexp
            put $ state { errors = ((Error, TypeMismatchUnaryExpr (show posEnds) (show exprTp)):(errors state))}
            return (
                env2,
                (UnaryExpression Not parsedexp (TypeBaseType BaseType_error) ),
                posEnds
                )


-- Arithmetic Unary Negation
parseExpression env (UnaryExpression Negation exp t) = do
    (env2, parsedexp, posEnds) <- parseExpression env exp
    typeExpr <- getTypeFromExpression parsedexp

    -- if the type is numeric or it is an error no new error messages are generated
    -- sup function guarantess that if the type was Error the expression is annotated with type Error
    if typeExpr == TypeBaseType BaseType_integer || typeExpr == TypeBaseType BaseType_real || typeExpr == TypeBaseType BaseType_error
        then
            return (
                env2,
                (UnaryExpression Negation parsedexp (sup (typeExpr) (TypeBaseType BaseType_integer)) ),
                posEnds
                )
        else do -- generate new error message
            state <- get
            exprTp <- getTypeFromExpression parsedexp
            put $ state { errors = ((Error, TypeMismatchArithmeticMinus (show posEnds) (show exprTp)):(errors state))}
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
    typeExpr <- getTypeFromExpression parsedexp
    --TODO: aggiornare posEnds, allunga a destra di 1 colonna per l'operatore ^

    -- need to get type of pointed expression: can do so only if expression is of pointer type
    case typeExpr of
        (TypeBaseType BaseType_error) ->
            return (env2, (UnaryExpression Dereference parsedexp (TypeBaseType BaseType_error)), posEnds )

        (TypeCompType (Pointer pointedType) ) ->
            return (env2, (UnaryExpression Dereference parsedexp pointedType), posEnds )

        _ -> do
            state <- get
            exprTp <- getTypeFromExpression parsedexp
            --TODO: nonostante questo errore, viene segnato errore anche nella chiamata della funzione dove viene usata l'espressione
            put $ state { errors = ((Error, TypeMismatchPointer (show posEnds) (showExpr exp) (show exprTp)):(errors state))}
            return (env2, (UnaryExpression Dereference parsedexp (TypeBaseType BaseType_error)), posEnds )
{- 
    if validLExpr parsedexp
        then
            case typeExpr of
                (TypeBaseType BaseType_error) ->
                    return (env2, (UnaryExpression Dereference parsedexp (TypeBaseType BaseType_error)), posEnds )

                (TypeCompType (Pointer pointedType) ) ->
                    return (env2, (UnaryExpression Dereference parsedexp pointedType), posEnds )

                _ -> do
                    state <- get
                    exprTp <- getTypeFromExpression parsedexp
                    --TODO: nonostante questo errore, viene segnato errore anche nella chiamata della funzione dove viene usata l'espressione
                    put $ state { errors = ((Error, TypeMismatchPointer (show posEnds) (showExpr exp) (show exprTp)):(errors state))}
                    return (env2, (UnaryExpression Dereference parsedexp (TypeBaseType BaseType_error)), posEnds )

        else do
            state <- get
            --TODO: nonostante questo errore, viene segnato errore anche nella chiamata della funzione dove viene usata l'espressione
            put $ state { errors =((Error, InvalidLExpressionDereference (show posEnds) (showExpr exp)):(errors state))}
            return (env2, (UnaryExpression Dereference parsedexp (TypeBaseType BaseType_error)), posEnds )
-}


-- Reference
parseExpression env (UnaryExpression Reference exp t) = do
    (env2, parsedexp, posEnds) <- parseExpression env exp
    typeExpr <- getTypeFromExpression parsedexp
    traceM $ "\n"++ show (typeExpr)++"  "++ showExpr exp ++"\n"

    --TODO: aggiornare posEnds, allunga a sinistra di 1 colonna per l'operatore @
    -- can create a pointer only if expression is a valid l-expression, including a pointer
    if typeExpr == TypeBaseType BaseType_error
        then
            return (env2, (UnaryExpression Reference parsedexp (TypeBaseType BaseType_error)), posEnds )
        else
            if validLExpr parsedexp || isPointerType typeExpr
                then return (env2, (UnaryExpression Reference parsedexp (TypeCompType (Pointer (typeExpr)))), posEnds )
                else do
                    state <- get
                    exprTp <- getTypeFromExpression parsedexp
                    --TODO: nonostante questo errore, viene segnato errore anche nella chiamata della funzione dove viene usata l'espressione
                    put $ state { errors = ((Error, TypeMismatchReference (show posEnds) (show exprTp)):(errors state))}
                    return (
                        env2,
                        (UnaryExpression Reference parsedexp (TypeBaseType BaseType_error) ),
                        posEnds
                        )
            where
                isPointerType :: Type -> Bool
                isPointerType (TypeCompType (Pointer _)) = True
                isPointerType _ = False


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
    typeExpr <- getTypeFromExpression parsedexpr

    case typeExpr of

        TypeBaseType BaseType_integer -> return (env2, (IntToReal parsedexpr), posEnds)

        TypeBaseType BaseType_real -> do
            state <- get
            put $ state { errors = ((Warning, UnnecessaryCasting (show posEnds) "Integer" "Real"):(errors state))}
            return (env2, parsedexpr, posEnds) -- expression is real already: remove type casting wrapper

        TypeBaseType BaseType_error -> return (env2, (IntToReal parsedexpr), posEnds) -- in case of errors that already happened, no new error messages are generated

        _ -> do
            state <- get
            put $ state { errors = ((Warning, ImplicitCasting (show posEnds) "Integer" "Real"):(errors state))}
            return ( env2, (IntToReal parsedexpr), posEnds )

parseExpression env (SelExpr cond expr1 expr2 t) = do
    (env2, parsedcond, posEndsC) <- parseExpression env cond
    (env3, parsedexpr1, posEnds1) <- parseExpression env2 expr1
    (env4, parsedexpr2, posEnds2) <- parseExpression env3 expr2
    typecond <- getTypeFromExpression parsedcond
    typeexpr1 <- getTypeFromExpression parsedexpr1
    typeexpr2 <- getTypeFromExpression parsedexpr2
    if typecond /= TypeBaseType BaseType_boolean && typecond /= TypeBaseType BaseType_error
    then do
        state <- get
        put $ state { errors = ((Error, TypeErrorConditionSelectionExpression (show (getNewPosEnds posEndsC posEnds2)) (showExpr parsedcond) (show typecond)):(errors state))}
        return (env4, (SelExpr parsedcond parsedexpr1 parsedexpr2 (TypeBaseType BaseType_error)), (getNewPosEnds posEndsC posEnds2))
    else
        case (typeexpr1, typeexpr2) of
            (TypeBaseType BaseType_error, _) -> return (env4, (SelExpr parsedcond parsedexpr1 parsedexpr2 (TypeBaseType BaseType_error)), (getNewPosEnds posEndsC posEnds2))
            (_, TypeBaseType BaseType_error) -> return (env4, (SelExpr parsedcond parsedexpr1 parsedexpr2 (TypeBaseType BaseType_error)), (getNewPosEnds posEndsC posEnds2))
            (typeexpr1, typeexpr2) -> if typeexpr1 == typeexpr2
                                        then
                                            return (env4, (SelExpr parsedcond parsedexpr1 parsedexpr2 typeexpr1), (getNewPosEnds posEndsC posEnds2))
                                        else do
                                            state <- get
                                            put $ state { errors = ((Error, TypeMismatchSelectionExpression (show (getNewPosEnds posEndsC posEnds2)) (showExpr parsedexpr1) (showExpr parsedexpr2) (show typeexpr1) (show typeexpr2)):(errors state))}
                                            return (env4, (SelExpr parsedcond parsedexpr1 parsedexpr2 (TypeBaseType BaseType_error)), (getNewPosEnds posEndsC posEnds2))
    where
        getNewPosEnds :: PosEnds -> PosEnds -> PosEnds
        getNewPosEnds PosEnds{leftmost=l1,rightmost=r1} PosEnds{leftmost=l2,rightmost=r2} = PosEnds{leftmost=l1,rightmost=r2}


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
            put $ state { errors = ((Error, CallingConstant (show posEndsToken) tokid):(errors state))}
            return (
                env2,
                (CallArgs tkId parsedargs ),
                (TypeBaseType BaseType_error),
                posEndsToken
                )

        Just (Variable mod pos t addr) -> do
            state <- get
            put $ state { errors = ((Error, CallingVariable (show posEndsToken) tokid):(errors state))}
            return (
                env2,
                (CallArgs tkId parsedargs ),
                (TypeBaseType BaseType_error),
                posEndsToken
                )

        Nothing -> do
            state <- get
            put $ state { errors = ((Error, UnknownIdentifier (show posEndsToken) tokid):(errors state))}
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
    put $ state { errors = ((Error, NumOfArgsMismatch (show tokpos) "function" tokid):(errors state))}
    return (
        env,
        (CallArgs tkId pargs ),
        (TypeBaseType BaseType_error)
        )

parseFunctionCall env (CallArgs tkId@(TokIdent (tokpos,tokid)) args ) NoParams t pargs posEnds = do
    state <- get
    put $ state { errors = ((Error, NumOfArgsMismatch (show tokpos) "function" tokid):(errors state))}
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
    put $ state { errors = ((Error, NumOfArgsMismatch (show tokpos) "procedure" tokid):(errors state))}
    return (
        env,
        (CallArgs tkId pargs ),
        (TypeBaseType BaseType_error)
        )

parseProcedureCall env (CallArgs tkId@(TokIdent (tokpos,tokid)) args ) NoParams pargs posEnds = do
    state <- get
    put $ state { errors = ((Error, NumOfArgsMismatch (show tokpos) "procedure" tokid):(errors state))}
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
    | m == Modality_ref = do
        argExprType <- getTypeFromExpression expr

        if not (validLExpr expr)
            then do
                state <- get
                put $ state { errors = ((Error, InvalidLValueReferenceArg (show p) parid (show parPosEnds) (showExpr expr)):(errors state))}
                compareArguments env p args (Param m toks t) (pargs++[expr])
            else if argExprType /= t
                    then do
                        state <- get
                        put $ state { errors = ((Error, TypeMismatchReferenceArg (show p) (showExpr expr) (show argExprType) (show t) parid (show parPosEnds)):(errors state))}
                        compareArguments env p args (Param m toks t) (pargs++[expr])

                    else
                        compareArguments env p args (Param m toks t) (pargs++[expr])

    -- call by value
    | otherwise = do
        argExprType <- getTypeFromExpression expr

        -- confronto tra parametri, diversi casi: 1) int to real, 2) incompatibile
        if argExprType == t
            then
                compareArguments env p args (Param m toks t) (pargs++[expr])
            else
                case (argExprType, t) of

                    (TypeBaseType BaseType_integer, TypeBaseType BaseType_real) ->
                        compareArguments env p args (Param m toks t) (pargs++[(IntToReal expr)])

                    -- error in expression of parameter of function/procedure call, no new error messages are generated
                    (TypeBaseType BaseType_error, _) ->
                        compareArguments env p args (Param m toks t) (pargs++[expr])

                    _ -> do
                        state <- get
                        put $ state { errors = ((Error, TypeMismatchArgument
                                                    (show p)
                                                    (showExpr expr)
                                                    (show argExprType)
                                                    (show t)
                                                    parid
                                                    (show parPosEnds)):(errors state))}
                        compareArguments env p args (Param m toks t) (pargs++[expr])
    where
        parPosEnds = PosEnds{ leftmost = parpos, rightmost = (x, y + length parid)}


validLExpr :: EXPR Type -> Bool
validLExpr (BaseExpr (Identifier _) _) = True
validLExpr (UnaryExpression Dereference _ _) = True
validLExpr (BaseExpr (ArrayElem _ _) _) = True
validLExpr _ = False


parseBinaryBooleanExpression :: Env -> BinaryOperator -> EXPR infType -> EXPR infType -> SSAState (Env, EXPR Type, PosEnds)
parseBinaryBooleanExpression env op exp1 exp2 = do
    (env2, parsedexp1, posEndsL) <- parseExpression env exp1
    (env3, parsedexp2, posEndsR) <- parseExpression env2 exp2
    typeExpr1 <- getTypeFromExpression parsedexp1
    typeExpr2 <- getTypeFromExpression parsedexp2

    case (isBooleanType typeExpr1, isBooleanType typeExpr2) of
        (False, False) -> do
            state <- get
            put $ state { errors = ((Error, TypeMismatchBooleanOperator
                                    (show exprPosEnds)
                                    (getStringFromOperator op)
                                    "left"
                                    (showExpr exp1)
                                    (show typeExpr1)):
                                    (Error, TypeMismatchBooleanOperator
                                    (show exprPosEnds)
                                    (getStringFromOperator op)
                                    "right"
                                    (showExpr exp2)
                                    (show typeExpr2)):(errors state))}
            return (
                env3,
                (BinaryExpression op parsedexp1 parsedexp2 (TypeBaseType BaseType_error) ),
                PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR}
                )
            where
                exprPosEnds = PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR}

        (True, False) -> do
            state <- get
            put $ state { errors = ((Error, TypeMismatchBooleanOperator
                                    (show exprPosEnds)
                                    (getStringFromOperator op)
                                    "right"
                                    (showExpr exp2)
                                    (show typeExpr2)):(errors state))}
            return (
                env3,
                (BinaryExpression op parsedexp1 parsedexp2 (TypeBaseType BaseType_error) ),
                PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR}
                )
            where
                exprPosEnds = PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR}

        (False, True) -> do
            state <- get
            put $ state { errors = ((Error, TypeMismatchBooleanOperator
                                    (show exprPosEnds)
                                    (getStringFromOperator op)
                                    "left"
                                    (showExpr exp1)
                                    (show typeExpr1)):(errors state))}
            return (
                env3,
                (BinaryExpression op parsedexp1 parsedexp2 (TypeBaseType BaseType_error) ),
                PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR}
                )
            where
                exprPosEnds = PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR}

        (True, True) -> if typeExpr1 == TypeBaseType BaseType_error || typeExpr2 == TypeBaseType BaseType_error
            then
                return (
                env3,
                (BinaryExpression op parsedexp1 parsedexp2 (TypeBaseType BaseType_error) ),
                exprPosEnds
                )
            else
                return (
                env3,
                (BinaryExpression op parsedexp1 parsedexp2 (TypeBaseType BaseType_boolean) ),
                exprPosEnds
                )
            where
                exprPosEnds = PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR}
    where
        -- errors have to be considered valid types in order to avoid generating induced error messages
        isBooleanType :: Type -> Bool
        isBooleanType (TypeBaseType BaseType_boolean) = True
        isBooleanType (TypeBaseType BaseType_error) = True
        isBooleanType _ = False
        -- string form of operator for printing error messages
        getStringFromOperator :: BinaryOperator -> [Char]
        getStringFromOperator And = "'And'"
        getStringFromOperator Or = "'Or'"
        getStringFromOperator _ = ""


parseBinaryArithmeticExpression :: Env -> BinaryOperator -> EXPR infType -> EXPR infType -> SSAState (Env, EXPR Type, PosEnds)
parseBinaryArithmeticExpression env op exp1 exp2 = do
    (env2, parsedexp1, posEndsL) <- parseExpression env exp1
    (env3, parsedexp2, posEndsR) <- parseExpression env2 exp2
    typeExpr1 <- getTypeFromExpression parsedexp1
    typeExpr2 <- getTypeFromExpression parsedexp2

    case (isNumericType typeExpr1, isNumericType typeExpr2) of
        (False, False) -> do
            state <- get
            put $ state { errors = ((Error, TypeMismatchBinaryExpr
                                    (show PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR})
                                    "first"
                                    (showExpr parsedexp1)
                                    (getStringFromOperator op)
                                    (show typeExpr1)
                                    "numeric (Integer or Real)"):
                                    (Error, TypeMismatchBinaryExpr
                                    (show PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR})
                                    "second"
                                    (showExpr parsedexp2)
                                    (getStringFromOperator op)
                                    (show typeExpr2)
                                    "numeric (Integer or Real)"):(errors state))}
            return (
                env3,
                (BinaryExpression op parsedexp1 parsedexp2 (TypeBaseType BaseType_error) ),
                PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR}
                )

        (False, True) -> do
            state <- get
            put $ state { errors = ((Error, TypeMismatchBinaryExpr
                                    (show PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR})
                                    "first"
                                    (showExpr parsedexp1)
                                    (getStringFromOperator op)
                                    (show typeExpr1)
                                    "numeric (Integer or Real)"):(errors state))}
            return (
                env3,
                (BinaryExpression op parsedexp1 parsedexp2 (TypeBaseType BaseType_error) ),
                PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR}
                )

        (True, False) -> do
            state <- get
            put $ state { errors = ((Error, TypeMismatchBinaryExpr
                                (show PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR})
                                "second"
                                (showExpr parsedexp2)
                                (getStringFromOperator op)
                                (show typeExpr2)
                                "numeric (Integer or Real)"):(errors state))}
            return (
                env3,
                (BinaryExpression op parsedexp1 parsedexp2 (TypeBaseType BaseType_error) ),
                PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR}
                )

        (True, True) -> if typeExpr1 == TypeBaseType BaseType_error || typeExpr2 == TypeBaseType BaseType_error
            then
                return (
                    env3,
                    (BinaryExpression op parsedexp1 parsedexp2 (TypeBaseType BaseType_error) ),
                    PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR}
                    )
            -- apply type casting where is needed
            else case (typeExpr1, typeExpr2) of
                    (TypeBaseType BaseType_integer, TypeBaseType BaseType_real) -> do
                        tp <- getType op parsedexp1 parsedexp2
                        return (env3, (BinaryExpression op (IntToReal parsedexp1) parsedexp2 tp), PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR})

                    (TypeBaseType BaseType_real, TypeBaseType BaseType_integer) -> do
                        tp <- getType op parsedexp1 parsedexp2
                        return (env3, (BinaryExpression op parsedexp1 (IntToReal parsedexp2) tp ), PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR})

                    _ -> do
                        tp <- getType op parsedexp1 parsedexp2
                        return (env3, (BinaryExpression op parsedexp1 parsedexp2 tp ), PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR})
    where
        -- errors have to be considered valid types in order to avoid generating induced error messages
        isNumericType :: Type -> Bool
        isNumericType (TypeBaseType BaseType_integer) = True
        isNumericType (TypeBaseType BaseType_real) = True
        isNumericType (TypeBaseType BaseType_error) = True
        isNumericType _ = False

        -- string form of operator for printing error messages
        getStringFromOperator :: BinaryOperator -> [Char]
        getStringFromOperator Add = "'+'"
        getStringFromOperator Sub = "'-'"
        getStringFromOperator Mul = "'*'"
        getStringFromOperator Div = "'/'"
        getStringFromOperator Mod = "'mod'"
        getStringFromOperator _ = ""
        -- division has always result of real type --   TODO: mantenere questa distinzione?
        getType :: BinaryOperator -> EXPR Type -> EXPR Type -> SSAState Type
        getType Div _ _ = return $ TypeBaseType BaseType_real
        getType _ parsedexp1 parsedexp2 = do
            typeExpr1 <- getTypeFromExpression parsedexp1
            typeExpr2 <- getTypeFromExpression parsedexp2
            return (sup typeExpr1 typeExpr2 )


parseBinaryRelationExpression :: Env -> BinaryOperator -> EXPR infType -> EXPR infType -> SSAState (Env, EXPR Type, PosEnds)
parseBinaryRelationExpression env op exp1 exp2 = do
    (env2, parsedexp1, posEndsL) <- parseExpression env  exp1
    (env3, parsedexp2, posEndsR) <- parseExpression env2 exp2
    typeExpr1 <- getTypeFromExpression parsedexp1
    typeExpr2 <- getTypeFromExpression parsedexp2

    -- Eq and NotEq are compatible with any atomic base type (Integer, Real, Boolean, Char), the other operators are only compatible with numeric types (Integer, Real)
    -- 3 cases: 1) first element not valid, 2) second argument not valid, 3) no errors
    if op == Eq || op ==  NotEq
    then case (isAtomicType typeExpr1, isAtomicType typeExpr2) of -- can only accept atomic types, otherwise error message is generated
        (False, False) -> do
            state <- get
            put $ state { errors = ((Error, TypeMismatchBinaryExpr
                                    (show PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR})
                                    "first"
                                    (showExpr parsedexp1)
                                    (getStringFromOperator op)
                                    (show typeExpr1)
                                    "atomic (Integer, Real, Boolean or Char)"):
                                    (Error, TypeMismatchBinaryExpr
                                    (show PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR})
                                    "second"
                                    (showExpr parsedexp2)
                                    (getStringFromOperator op)
                                    (show typeExpr2)
                                    "atomic (Integer, Real, Boolean or Char)"):(errors state))}
            return (
                    env3,
                    (BinaryExpression op parsedexp1 parsedexp2 (TypeBaseType BaseType_error) ),
                    PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR} )

        (False, True) -> do
            state <- get
            put $ state { errors = ((Error, TypeMismatchBinaryExpr
                                    (show PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR})
                                    "first"
                                    (showExpr parsedexp1)
                                    (getStringFromOperator op)
                                    (show typeExpr1)
                                    "atomic (Integer, Real, Boolean or Char)"):(errors state))}
            return (
                    env3,
                    (BinaryExpression op parsedexp1 parsedexp2 (TypeBaseType BaseType_error) ),
                    PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR} )

        (True, False) -> do
            state <- get
            put $ state { errors = ((Error, TypeMismatchBinaryExpr
                                    (show PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR})
                                    "second"
                                    (showExpr parsedexp2)
                                    (getStringFromOperator op)
                                    (show typeExpr2)
                                    "atomic (Integer, Real, Boolean or Char)"):(errors state))}
            return (
                    env3,
                    (BinaryExpression op parsedexp1 parsedexp2 (TypeBaseType BaseType_error) ),
                    PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR} )

        (True, True) ->
            if (sup typeExpr1 typeExpr2) == TypeBaseType BaseType_error && typeExpr1 /= TypeBaseType BaseType_error && typeExpr2 /= TypeBaseType BaseType_error then do
                state <- get
                put $ state { errors = ((Error, TypeIncompatibleBinaryExpr
                                        (show PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR})
                                        (getStringFromOperator op)
                                        (showExpr parsedexp1)
                                        (showExpr parsedexp2)
                                        (show typeExpr1)
                                        (show typeExpr2)):(errors state))}
                return (
                        env3,
                        (BinaryExpression op parsedexp1 parsedexp2 (TypeBaseType BaseType_error) ),
                        PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR} )
                else if typeExpr1 == TypeBaseType BaseType_error || typeExpr2 == TypeBaseType BaseType_error
                    then
                        return (
                                env3,
                                (BinaryExpression op parsedexp1 parsedexp2 (TypeBaseType BaseType_error) ),
                                PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR} )
                    else
                        -- apply type casting if needed
                        case (typeExpr1, typeExpr2) of
                        (TypeBaseType BaseType_integer, TypeBaseType BaseType_real) ->
                            return (
                                env3,
                                (BinaryExpression op (IntToReal parsedexp1) parsedexp2 (TypeBaseType BaseType_boolean) ),
                                PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR} )
                        (TypeBaseType BaseType_real, TypeBaseType BaseType_integer) ->
                            return (
                                env3,
                                (BinaryExpression op parsedexp1 (IntToReal parsedexp2) (TypeBaseType BaseType_boolean) ),
                                PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR} )
                        _ ->
                            return (
                                env3,
                                (BinaryExpression op parsedexp1 parsedexp2 (TypeBaseType BaseType_boolean) ),
                                PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR} )

    -- other binary relations that are not Eq and NotEq
    else
        case (isNumericType typeExpr1, isNumericType typeExpr2) of -- can only accept numeric types, otherwise error message is generated
            -- consider every possible case to generate the correct number of error messages
            (False, False) -> do
                state <- get
                put $ state { errors = ((Error, TypeMismatchBinaryExpr
                                        (show PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR})
                                        "first"
                                        (showExpr parsedexp1)
                                        (getStringFromOperator op)
                                        (show typeExpr1)
                                        "numeric (Integer or Real)"):
                                        (Error, TypeMismatchBinaryExpr
                                        (show PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR})
                                        "second"
                                        (showExpr parsedexp2)
                                        (getStringFromOperator op)
                                        (show typeExpr2)
                                        "numeric (Integer or Real)"):(errors state))}
                return (
                        env3,
                        (BinaryExpression op parsedexp1 parsedexp2 (TypeBaseType BaseType_error) ),
                        PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR} )

            (False, True) -> do
                state <- get
                put $ state { errors = ((Error, TypeMismatchBinaryExpr
                                        (show PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR})
                                        "first"
                                        (showExpr parsedexp1)
                                        (getStringFromOperator op)
                                        (show typeExpr1)
                                        "numeric (Integer or Real)"):(errors state))}
                return (
                        env3,
                        (BinaryExpression op parsedexp1 parsedexp2 (TypeBaseType BaseType_error) ),
                        PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR} )

            (True, False) -> do
                state <- get
                put $ state { errors = ((Error, TypeMismatchBinaryExpr
                                        (show PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR})
                                        "second"
                                        (showExpr parsedexp2)
                                        (getStringFromOperator op)
                                        (show typeExpr2)
                                        "numeric (Integer or Real)"):(errors state))}
                return (
                        env3,
                        (BinaryExpression op parsedexp1 parsedexp2 (TypeBaseType BaseType_error) ),
                        PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR} )

            (True, True) -> -- no new errors
                if typeExpr1 == TypeBaseType BaseType_error || typeExpr2 == TypeBaseType BaseType_error
                    then
                        return (
                                env3,
                                (BinaryExpression op parsedexp1 parsedexp2 (TypeBaseType BaseType_error) ),
                                PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR} )
                    else
                        -- apply type casting if needed
                        case (typeExpr1, typeExpr2) of
                        (TypeBaseType BaseType_integer, TypeBaseType BaseType_real) ->
                            return (
                                env3,
                                (BinaryExpression op (IntToReal parsedexp1) parsedexp2 (TypeBaseType BaseType_boolean) ),
                                PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR} )
                        (TypeBaseType BaseType_real, TypeBaseType BaseType_integer) ->
                            return (
                                env3,
                                (BinaryExpression op parsedexp1 (IntToReal parsedexp2) (TypeBaseType BaseType_boolean) ),
                                PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR} )
                        _ ->
                            return (
                                env3,
                                (BinaryExpression op parsedexp1 parsedexp2 (TypeBaseType BaseType_boolean) ),
                                PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR} )

    where
        -- errors have to be considered valid types in order to avoid generating induced error messages
        isAtomicType :: Type -> Bool
        isAtomicType (TypeBaseType BaseType_integer) = True
        isAtomicType (TypeBaseType BaseType_real) = True
        isAtomicType (TypeBaseType BaseType_boolean) = True
        isAtomicType (TypeBaseType BaseType_char) = True
        isAtomicType (TypeBaseType BaseType_error) = True
        isAtomicType _ = False

        isNumericType :: Type -> Bool
        isNumericType (TypeBaseType BaseType_integer) = True
        isNumericType (TypeBaseType BaseType_real) = True
        isNumericType (TypeBaseType BaseType_error) = True
        isNumericType _ = False

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
            put $ state { errors = ((Error, UnknownIdentifier (show tokpos) tokid):(errors state))}
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
    typeBExpr <- getTypeFromExpression parsedbexpr
    typeIExpr <- getTypeFromExpression parsediexpr

    case (typeBExpr, typeIExpr) of -- TODO: refactoring possibile di questa parte di codice? --TODO: posizione nei messaggi di errore

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
            exprTp <- getTypeFromExpression parsediexpr
            put $ state { errors = ((Error, TypeMismatchArrayIndex (show PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR}) (show exprTp)):(errors state))}
            return (
                env3,
                (ArrayElem parsedbexpr parsediexpr),
                TypeBaseType BaseType_error,
                PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR}
                )

        (_, TypeBaseType BaseType_integer) -> do
            state <- get
            exprTp <- getTypeFromExpression parsedbexpr
            put $ state { errors = ((Error, TypeMismatchNotArray (show PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR}) (showExpr parsedbexpr) (show exprTp)):(errors state))}
            return (
                env3,
                (ArrayElem parsedbexpr parsediexpr),
                TypeBaseType BaseType_error,
                PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR}
                )

        _ -> do
            state <- get
            exprTp1 <- getTypeFromExpression parsedbexpr
            exprTp2 <- getTypeFromExpression parsediexpr
            put $ state { errors =
                ((Error, TypeMismatchNotArray (show PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR}) (showExpr parsedbexpr) (show exprTp1))
                :((Error, TypeMismatchArrayIndex (show PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR}) (show exprTp2)):(errors state)))}
            return (
                env3,
                (ArrayElem parsedbexpr parsediexpr),
                TypeBaseType BaseType_error,
                PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR}
                )
