{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module TypeChecker where
import AbsGrammar
import Env
import Text.Parsec (parserReturn)
import HelperTAC
import Control.Monad.State.Strict
import Debug.Trace
import Data.Map

type Errors = [String]
emptyErrors :: [String]
emptyErrors = []

data PosEnds = PosEnds {
    leftmost :: Position,
    rightmost :: Position
}

instance Show PosEnds where {
    show (PosEnds leftmost rightmost) = show leftmost ++"-"++ show rightmost
}


launchStatSemAnalysis :: P env infType -> (Env, Errors, P Env Type)
launchStatSemAnalysis tree = evalState (parseTree tree defaultEnv emptyErrors) (SSAStateStruct {idCount = 0, errors=[], unInitVars= newStack } )

-- Type Checking starting point
parseTree :: P env infType -> Env -> Errors -> SSAState (Env, Errors, P Env Type)
parseTree (Prog pBlock dclBlock beBlock _) env errs = do
    pushNewUninit
    (globEnv, errors, dBlks) <- parseDclBlocksFirstPass env errs dclBlock
    (globEnv1, errors1, annBlks) <- parseDclBlocks globEnv errors dBlks
        -- errors and env are propagated from declaration block into beginEnd Block!
        -- notice that globEnv is the env after parsing declaration blocks
    (finalEnv, errors2, beBlks) <- parseBEBlock globEnv errors1 beBlock
    finalErrors <- popUninit errors2
    
    return (finalEnv, finalErrors, Prog pBlock annBlks beBlks globEnv)

-- Navigates syntax tree and saves info about variables type (declared in a Declaration block) in the global environment
parseDclBlocksFirstPass:: Env -> Errors -> [DclBlock env infType] -> SSAState (Env, Errors, [DclBlock env infType])
parseDclBlocksFirstPass env errors [] = return (env, errors, [])
parseDclBlocksFirstPass env errors (x:xs) = do
    (env1, errors1, newBlock) <- parseSingleDclBlockFirstPass env errors x
    (finalEnv, finalErrors, newBlocks) <- parseDclBlocksFirstPass env1 errors1 xs
    return (finalEnv, finalErrors, newBlock : newBlocks)
    where
        -- TODO: make sure errores are updated after parsing declaration blocks
        -- e.g. redefining a variable could produce a warning and redefinig a constant an error
        parseSingleDclBlockFirstPass :: Env -> Errors -> DclBlock env infType -> SSAState (Env, Errors, DclBlock env infType)
        parseSingleDclBlockFirstPass env errors blk = case blk of
            DclBlockVrBlock _ -> parseDclVrBlockFirstPass env errors blk
            DclBlockCsBlock _ -> parseDclCsBlockFirstPass env errors blk
            -- TODO: pass global environment to begin-end block and parse inner statemets
            DclBlockFcBlock _ -> parseDclFcBlockFirstPass env errors blk
            DclBlockPcBlock _ -> parseDclPcBlockFirstPass env errors blk


-- Navigates syntax tree and saves info about variables type (declared in a Declaration block) in the global environment
parseDclBlocks:: Env -> Errors -> [DclBlock env infType] -> SSAState (Env, Errors, [DclBlock Env Type])
parseDclBlocks env errors [] = return (env, errors, [])
parseDclBlocks env errors (x:xs) = do
    (env1, errors1, newBlock) <- parseSingleDclBlock env errors x
    (finalEnv, finalErrors, newBlocks) <- parseDclBlocks env1 errors1 xs
    return (finalEnv, finalErrors, newBlock : newBlocks)
    where
        -- TODO: make sure errores are updated after parsing declaration blocks
        -- e.g. redefining a variable could produce a warning and redefinig a constant an error
        parseSingleDclBlock :: Env -> Errors -> DclBlock env infType -> SSAState (Env, Errors, DclBlock Env Type)
        parseSingleDclBlock env errors blk = case blk of
            DclBlockVrBlock (VarBlock vrDefs) -> return (env, errors, DclBlockVrBlock (VarBlock vrDefs))
            DclBlockCsBlock (ConstBlock csDefs) -> return (env, errors, DclBlockCsBlock (ConstBlock csDefs)) 
            -- TODO: pass global environment to begin-end block and parse inner statemets
            DclBlockFcBlock _ -> parseDclFcBlock env errors blk
            DclBlockPcBlock _ -> parseDclPcBlock env errors blk


parseDclVrBlockFirstPass :: Env -> Errors -> DclBlock env infType -> SSAState (Env, Errors, DclBlock env infType)
parseDclVrBlockFirstPass env errors (DclBlockVrBlock (VarBlock vrDefs)) = do
    newEnv <- parseVrDefs vrDefs env;
    -- add info about variables to the environment
    return (newEnv, errors, DclBlockVrBlock (VarBlock vrDefs))
        where
            -- savese info about variables type in env 
            parseVrDefs :: [VrDef] -> Env -> SSAState Env
            parseVrDefs [] env = return env
            parseVrDefs ((VarDefinition idElements t):vrDefs) env = do
                -- TODO : probabilmente necessaria gestione errori (ovvero restituire anche errori)
                (tmpEnv, tmpErrs, _) <- parseIds idElements Modality_val t env [] True
                return tmpEnv
            

-- add info about constants to the environment          
parseDclCsBlockFirstPass :: Env -> Errors -> DclBlock env infType -> SSAState (Env, Errors, DclBlock env infType)
parseDclCsBlockFirstPass env errors (DclBlockCsBlock (ConstBlock csDefs)) = do 
    newEnv <- parseConsDefs csDefs env
    return (newEnv, errors, DclBlockCsBlock (ConstBlock csDefs))
        where
            -- saves info about constants type in env 
            parseConsDefs :: [CsDef] -> Env -> SSAState Env
            parseConsDefs [] env = return env
            parseConsDefs ((ConstDefinition (IdElement (TokIdent (pos, id))) literal):cs) env = do 
                tmpEnv <- Env.insert id (Constant pos (TypeBaseType (getTypeFromLiteral literal)) (TacLit (makeTACLit literal))) env
                parseConsDefs cs tmpEnv;
            

-- add info about functions to the environment
-- info are: function position, function name, parameters, return type
parseDclFcBlockFirstPass :: Env -> Errors -> DclBlock env infType -> SSAState (Env, Errors, DclBlock env infType)
parseDclFcBlockFirstPass env errors dcBlockFc@(DclBlockFcBlock (FuncBlock (TokIdent (pos, id)) params retType beb)) = do 
    fcAddr <- Env.newIdAddr id env

    -- add to env return type (needed for type checking of the return statement) and function info
    -- IMPORTANT NOTE: env must be the secondo argument of mergeEnvs, otherwise the new "return" key will not be updated
    -- this is because the underlying function union (t1, t2) of Data.Map prefers t1 when duplicated keys are encountered
    tmpEnv <- Env.mergeEnvs (Env.fromList [(id, Function pos params retType fcAddr), ("return", Return retType id pos)]) env
    (tmpEnv1, tmpErrors1, parsedParams) <- parseParams params [] tmpEnv errors
    return (tmpEnv1, tmpErrors1, dcBlockFc)


parseDclFcBlock :: Env -> Errors -> DclBlock env infType -> SSAState (Env, Errors, DclBlock Env Type)
parseDclFcBlock env errors (DclBlockFcBlock fB@(FuncBlock idTok@(TokIdent (pos, id)) params retType beb)) = do 
    -- check if function body contains a return statement
    errors1 <- if hasReturnStmt beb then 
        return errors 
        else return (("Missing return statement: body of function " ++ id ++ " at " ++ show pos ++ " does not contain a return statement"):errors)
    
    (finalEnv, finalErrors, annotatedBEB) <- parseBEBlock env errors1 beb
    return (finalEnv, finalErrors, DclBlockFcBlock (FuncBlock idTok params retType annotatedBEB))

    where        
        hasReturnStmt :: BEBlock env infType -> Bool
        hasReturnStmt (BegEndBlock stmts env) = do
            case stmts of
                [] ->  False
                (x:xs) -> case x of
                    (StmtReturn _) -> True -- return statement found
                    _ -> hasReturnStmt (BegEndBlock xs env) -- check next statement


-- add info about procedures to the environment. Same as functions but without return type
parseDclPcBlockFirstPass :: Env -> Errors -> DclBlock env infType -> SSAState (Env, Errors, DclBlock env infType)
parseDclPcBlockFirstPass env errors dclBlockPc@(DclBlockPcBlock (ProcBlock (TokIdent (pos, id)) params beb)) = do
    pcAddr <- Env.newIdAddr id env
    tmpEnv <- Env.insert id (Procedure pos params pcAddr) env
    (pEnv, pErrs, pPrms) <- parseParams params [] tmpEnv errors

    return (pEnv, pErrs, dclBlockPc)

-- add info about procedures to the environment. Same as functions but without return type
parseDclPcBlock :: Env -> Errors -> DclBlock env infType -> SSAState (Env, Errors, DclBlock Env Type)
parseDclPcBlock env errors (DclBlockPcBlock (ProcBlock idTok@(TokIdent (pos, id)) params beb)) = do
    (fEnv, fErrs, annBEB) <- parseBEBlock env errors beb
    return (fEnv, fErrs, DclBlockPcBlock (ProcBlock idTok params annBEB))


parseParams :: Prms -> [Prm] -> Env -> Errors -> SSAState (Env, Errors, Prms)
parseParams prms accPrms env errs = do 
    case prms of
        (Params ( p@(Param mod idList typ):ps )) -> do
            (tmpEnv, tmpErrs, _) <- parseIds idList mod typ env errs False
            (pEnv,pErrs, pPrms) <- parseParams (Params ps) (p:accPrms) tmpEnv tmpErrs
            return (pEnv, pErrs, pPrms)
                
        (Params []) -> return (env, errs, Params accPrms)

        NoParams -> return (env, errs, prms)


parseIds :: [IdElem] -> Modality -> Type -> Env -> Errors -> Bool -> SSAState (Env, Errors, [IdElem])
parseIds [] mod typ env errs _ = return (env, errs, [])
parseIds ( idElem@(IdElement (TokIdent (pos, id))):ids) mod typ env errs isVar = do
    idAddr <- newIdAddr id env
    tmpEnv <- Env.insert id (VarType mod pos typ idAddr) env

    if isVar
        then do
            insertVar id (VarType mod pos typ idAddr)
            (newEnv, newErrs, newIds) <- parseIds ids mod typ tmpEnv errs isVar
            return (newEnv, errs, idElem:newIds)
        else do
            (newEnv, newErrs, newIds) <- parseIds ids mod typ tmpEnv errs isVar
            return (newEnv, errs, idElem:newIds) 


-- parse the begin-end block and check the statements for type errors
parseBEBlock:: Env -> Errors -> BEBlock env infType -> SSAState (Env, Errors, BEBlock Env Type)
parseBEBlock env errors (BegEndBlock statements annEnv) = do
    pushNewUninit
    (tmpEnv, tmpErrors) <- parseStatementsFirstPass env errors statements
    (newEnv, newErrors, newStatements) <- parseStatements tmpEnv tmpErrors statements
    newErrors2 <- popUninit newErrors
    return (newEnv, newErrors2, BegEndBlock newStatements newEnv)
        

parseStatementsFirstPass :: Env -> Errors -> [Stmt env infType] -> SSAState (Env, Errors)
parseStatementsFirstPass env errors ((StmtDecl dclBlock):stmts) = do
    (newEnv, newErrs, tmpDclBlocks) <- parseDclBlocksFirstPass env errors [dclBlock]
    parseStatementsFirstPass newEnv newErrs stmts

parseStatementsFirstPass env errors (_:stmts) = parseStatementsFirstPass env errors stmts
parseStatementsFirstPass env errors _ = return (env,errors)


parseStatements :: Env -> Errors -> [Stmt env infType] -> SSAState (Env, Errors, [Stmt Env Type])
parseStatements env errors [] = return (env, errors, [])
parseStatements env errors allStmts =  q env errors allStmts []
        where
            q::Env -> Errors -> [Stmt env infType] -> [Stmt Env Type] -> SSAState (Env, Errors, [Stmt Env Type])
            q env errors [] annStmts = return (env, errors, annStmts)
            q env errors (s:xs) annStmts = do
                (env1, errors1, annStmt) <- parseStatement s env errors
                q env1 errors1 xs (annStmts++[annStmt])


parseStatement :: Stmt stmtenv infType -> Env -> Errors -> SSAState (Env, Errors, Stmt Env Type)
parseStatement stmt env errs = case stmt of
            -- tipologie di statement: dichiarazione, blocco, assegnamento, chiamata funzione, if-else, iterazione, return
            -- Dichiarazione
            (StmtDecl dclblock) -> do
                (env2, err2, blocks) <- parseDclBlocks env errs [dclblock]
                return (env2, err2, (StmtDecl (head blocks)))
                    
            -- Blocco
            (StmtComp beblock) -> do
                (env2, err2, block) <- parseBEBlock env errs beblock
                return (env2, err2, (StmtComp block))

            -- Assegnamento
            (StmtAssign expr1 expr2) -> parseAssignment expr1 expr2 env errs

            -- Iterazione
            (StmtIter iter) -> parseIter (StmtIter iter) env errs

            -- Return
            (StmtReturn return)  -> parseReturn (StmtReturn return) env errs

            -- Select
            (StmtSelect sel) -> parseSelection (StmtSelect sel) env errs

            -- Chiamata funzione
            (StmtCall call) -> parseStatementCall env errs call


parseIter :: Stmt env infType -> Env -> Errors -> SSAState (Env, Errors, Stmt Env Type)
-- parsing of while-do statement
parseIter (StmtIter (StmtWhileDo expr stmt)) env errs = do
    (env1, errs1, parsedExpr, posEnds) <- parseExpression env errs expr

    (newEnv, newErrs, parsedStmt) <- parseStatement stmt env1 errs1
    let wrappedStmt = wrapInBeginEnd parsedStmt newEnv

    if getTypeFromExpression parsedExpr == TypeBaseType BaseType_boolean || getTypeFromExpression parsedExpr == TypeBaseType BaseType_error
        then return (newEnv, newErrs, StmtIter (StmtWhileDo parsedExpr wrappedStmt))
        else return (newEnv, newErrs ++ ["ERROR in range " ++ show posEnds  ++ ": condition " ++ showExpr parsedExpr ++ " of while-do statement is not of type Bool but it is of type " ++ show (getTypeFromExpression parsedExpr)], StmtIter (StmtWhileDo parsedExpr wrappedStmt)) 

-- parsing of repeat-until statement
parseIter (StmtIter (StmtRepeat stmt expr)) env errs = do
    (env1, errs1, parsedStmt) <- parseStatement stmt env errs
    let wrappedStmt = wrapInBeginEnd parsedStmt env1
    (newEnv, newErrs, parsedExpr, posEnds) <- parseExpression env1 errs1 expr

    if getTypeFromExpression parsedExpr == TypeBaseType BaseType_boolean || getTypeFromExpression parsedExpr == TypeBaseType BaseType_error
        then return (newEnv, newErrs, StmtIter (StmtRepeat wrappedStmt parsedExpr))
        else return (newEnv, newErrs ++ ["ERROR in range " ++ show posEnds ++ ": condition " ++ showExpr parsedExpr ++ " of repeat-until statement is not of type Bool but it is of type " ++ show (getTypeFromExpression parsedExpr)], StmtIter (StmtRepeat wrappedStmt parsedExpr))

parseSelection :: Stmt env infType -> Env -> Errors -> SSAState (Env, Errors, Stmt Env Type)
parseSelection (StmtSelect (StmtIf expr stmt)) env errs = do
    (env1, errs1, parsedExpr, posEnds) <- parseExpression env errs expr
    (newEnv, newErrs, parsedStmt) <- parseStatement stmt env1 errs1
    let wrappedStmt = wrapInBeginEnd parsedStmt newEnv

    if getTypeFromExpression parsedExpr == TypeBaseType BaseType_boolean || getTypeFromExpression parsedExpr == TypeBaseType BaseType_error
        then return (newEnv, newErrs, StmtSelect (StmtIf parsedExpr wrappedStmt))
        else return (newEnv, newErrs ++ ["ERROR in range " ++ show posEnds  ++ ": condition " ++ showExpr parsedExpr ++ " of if statement is not of type Bool but it is of type " ++ show (getTypeFromExpression parsedExpr)], StmtSelect (StmtIf parsedExpr wrappedStmt))

parseSelection (StmtSelect (StmtIfElse expr stmt1 stmt2)) env errs = do
    (env1, errs1, parsedExpr, posEnds) <- parseExpression env errs expr
    (newEnv1, newErrs1, parsedStmt1) <- parseStatement stmt1 env1 errs1
    let wrappedStmt1 = wrapInBeginEnd parsedStmt1 newEnv1
    (newEnv2, newErrs2, parsedStmt2) <- parseStatement stmt2 newEnv1 newErrs1
    let wrappedStmt2 = wrapInBeginEnd parsedStmt2 newEnv2

    if getTypeFromExpression parsedExpr == TypeBaseType BaseType_boolean || getTypeFromExpression parsedExpr == TypeBaseType BaseType_error
        then return (newEnv2, newErrs2, StmtSelect (StmtIfElse parsedExpr wrappedStmt1 wrappedStmt2))
        else return (newEnv2, newErrs2 ++ ["ERROR in range " ++ show posEnds  ++ ": condition " ++ showExpr parsedExpr ++ " of if-else statement is not of type Bool but it is of type " ++ show (getTypeFromExpression parsedExpr)], StmtSelect (StmtIfElse parsedExpr wrappedStmt1 wrappedStmt2))


-- If the provided statement is not insiede a begin-end block, wraps it in one
-- This is needed to simplify TAC generation by having to deal only with begin-end blocks containing statements
-- With the current grammar the body of a iteration (or an if-else) statement can be eather a single statement or a begin-end block with other statements
wrapInBeginEnd :: Stmt Env infType -> Env -> Stmt Env infType
wrapInBeginEnd (StmtComp (BegEndBlock stmts begEnv)) _ = StmtComp (BegEndBlock stmts begEnv)
wrapInBeginEnd stmt stmEnv = StmtComp (BegEndBlock [stmt] stmEnv)

parseReturn :: Stmt env infType -> Env -> Errors -> SSAState (Env, Errors, Stmt Env Type)
parseReturn (StmtReturn (Ret expr)) env errs = do
    (newEnv, newErrs, parsedExpr, posEnds) <- parseExpression env errs expr
    
    case Env.lookup "return" env of 
    
        Just (Return expectedType funName funPos) ->
            if sup expectedType (getTypeFromExpression parsedExpr) /= expectedType && getTypeFromExpression parsedExpr /= TypeBaseType BaseType_error
                then return ( newEnv,
                        ("ERROR in range " ++ show posEnds  ++ ": function " ++ funName ++ " at " ++ show funPos ++ 
                        " expects a " ++ show expectedType ++ " to be returned " ++
                        "but the expression following the return statement has type " ++ show (getTypeFromExpression parsedExpr) ) : newErrs, 
                        StmtReturn (Ret parsedExpr))
                else
                    -- everything is ok or the error did not happen here, return the parsed expression 
                    return (newEnv, newErrs, StmtReturn (Ret parsedExpr))

        -- Theoretically this should never happen, 
        -- since the return type of the function is saved in the environment when the function definition is parsed
        Nothing -> return (
            newEnv,
            ("Internal type checking error: can't find expected return type of current function in the environment while parsing the expression following the return at " ++ show posEnds): newErrs,
            StmtReturn (Ret parsedExpr)
            ) 
        

parseAssignment :: EXPR infType -> EXPR infType -> Env -> Errors -> SSAState (Env, Errors, Stmt Env Type)
parseAssignment expr1 expr2 env errs = case (expr1, expr2) of
            -- Assegno a variabile un letterale
            ( (BaseExpr (Identifier tId@(TokIdent (_,id))) tp), (ExprLiteral literal) ) -> do
                removeVar id
                (env2, errs2, parsedid, posEnds) <- parseExpression env errs (BaseExpr (Identifier tId) tp)
                parseLitAssignment tId literal env2 errs2 posEnds
            
            -- Assegno a variabile valore espressione generica: 1) parsing dell'espressione e trovo il tipo; 2) controllo compatibilità con letterale in assegnamento
            ( (BaseExpr (Identifier tId@(TokIdent (_,id))) tp), expr ) -> do
                removeVar id
                (env2, errs2, parsedid, posEnds) <- parseExpression env errs (BaseExpr (Identifier tId) tp)
                (env3, errs3, parsedexpr, posEnds2) <- parseExpression env2 errs2 expr
                parseIdExprAssignment tId parsedexpr env3 errs3 posEnds --TODO: ricavare il range corretto, anche nei casi successivi
            
            -- Puntatore è un l-value valido
            ( (UnaryExpression Dereference expr1 t), expr2 ) -> do
                (env2, err2, parsedexpr1, posEnds1) <- parseExpression env errs (UnaryExpression Dereference expr1 t)
                (env3, err3, parsedexpr2, posEnds2) <- parseExpression env2 err2 expr2
                parseExprExprAssignment parsedexpr1 parsedexpr2 env3 err3 posEnds1
            
            -- Riferimento ad un puntatore è un l-value valido            
            ( (UnaryExpression Reference expr1 t), expr2 ) -> do
                (env2, err2, parsedexpr1, posEnds1) <- parseExpression env errs (UnaryExpression Reference expr1 t)
                (env3, err3, parsedexpr2, posEnds2) <- parseExpression env2 err2 expr2                
                parseExprExprAssignment parsedexpr1 parsedexpr2 env3 err3 posEnds1
            
            -- Array elements are valid l-values
            ( arr@(BaseExpr (ArrayElem idexpr iexpr) t), expr ) -> do
                (env2, err2, parsedexpr, posEnds1) <- parseExpression env errs expr
                --(env3, err3, parsediexpr) <- parseExpression env2 err2 iexpr
                (env3, err3, parsedarrexpr, posEnds2) <- parseExpression env2 err2 arr
                
                parseArrayAssignment parsedarrexpr parsedexpr env3 err3 posEnds1
            
            -- Il resto dei possibili l-value non è valido
            ( expr1, expr2 ) -> do
                (env2, err2, parsedexpr1, posEnds1) <- parseExpression env errs expr1
                (env3, err3, parsedexpr2, posEnds2) <- parseExpression env2 err2 expr2
                return (env3, ("Error in range " ++ show posEnds1 ++ ": expression "++ showExpr expr1 ++" is not a valid l-value for the assignment"):err3, (StmtAssign parsedexpr1 parsedexpr2) )


-- Checks if r-expression matches typing with an l-expression that is an element of an array
parseArrayAssignment:: EXPR Type -> EXPR Type -> Env -> Errors -> PosEnds -> SSAState (Env, Errors, Stmt Env Type)
parseArrayAssignment bExpr@(BaseExpr (ArrayElem bbexpr iiexpr) t) expr env errs posEnds = do
    if t == rtype
        then return (env, errs, (StmtAssign bExpr expr))
        else 
            -- 3 cases: a) int to real, b) char to string, c) incompatible types -> error
            case (t, rtype) of
                
                (TypeBaseType BaseType_real, TypeBaseType BaseType_integer) -> 
                    return (env, errs, (StmtAssign bExpr (IntToReal expr) ) )                   
                (TypeBaseType BaseType_string, TypeBaseType BaseType_char) -> 
                    return (env, errs, (StmtAssign bExpr (CharToString expr) ) )  
                
                -- cases in which an error was previously generated: no new error messages
                (TypeBaseType BaseType_error, _) -> 
                    return (env, errs, (StmtAssign bExpr expr ) )
                (_, TypeBaseType BaseType_error) -> 
                    return (env, errs, (StmtAssign bExpr expr ) )

                --TODO: implementare posizione con il modo che avevamo discusso
                -- Durante il parsing le funzioni restituiscono anche 2 posizioni (quella più a sx e quella più a dx)
                

                _ -> return (
                    env, 
                    ("Error in range " ++ show posEnds ++ ". l-Expression "++showExpr bExpr++" is of type " ++ show t ++ " but it is assigned value of type "++show rtype ++"."):errs, 
                    StmtAssign bExpr (IntToReal expr) 
                    )

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
parseLitAssignment:: TokIdent -> Literal -> Env -> Errors -> PosEnds -> SSAState (Env, Errors, Stmt Env Type)
parseLitAssignment tkId@(TokIdent (idPos, idVal)) literal env errors posEnds = case Env.lookup idVal env of
    
    Just (VarType mod envPos envType addr) ->
        if envType == TypeBaseType litType
            then return (
                env,
                errors,
                -- NOTICE HOW WE ANNOTATE THE TREE, saving info about type of expr!
                StmtAssign (BaseExpr (Identifier tkId) envType) (ExprLiteral literal)
                )
            else case (envType, litType ) of
                -- 3 cases: casting int->real, char->string or incompatible types
                (TypeBaseType BaseType_real, BaseType_integer) -> return (env, errors, StmtAssign (BaseExpr (Identifier tkId) envType) (IntToReal (ExprLiteral literal)) )
                (TypeBaseType BaseType_string, BaseType_char) -> return (env, errors, StmtAssign (BaseExpr (Identifier tkId) envType) (CharToString (ExprLiteral literal)) )

                -- cases in which an error was previously generated: no new error messages, pass the error forwards
                (TypeBaseType BaseType_error, _) -> 
                    return (env, errors, StmtAssign (BaseExpr (Identifier tkId) (TypeBaseType BaseType_error)) (ExprLiteral literal) )
                (_, BaseType_error) -> 
                    return (env, errors, StmtAssign (BaseExpr (Identifier tkId) (TypeBaseType BaseType_error)) (ExprLiteral literal) )

                -- In case of errors the tree is annotated with the Error type
                (_, _)  -> return (
                    env, 
                    ("Error in range " ++ show posEnds ++ ". id "++ idVal ++ " is of type " ++ show envType ++ " but is assigned value of type " ++ show litType) :errors, 
                    StmtAssign (BaseExpr (Identifier tkId) (TypeBaseType BaseType_error)) (ExprLiteral literal) 
                    )

    
    Nothing -> return (env,
                ("Error in ragne " ++ show posEnds ++
                ". Unknown identifier: " ++ idVal ++
                " is used but has never been declared."):errors,
                StmtAssign (BaseExpr (Identifier tkId) (TypeBaseType BaseType_error)) (ExprLiteral literal))

    where
        litType = getTypeFromLiteral literal

-- Given identifier and expression (already parsed with inferred type!) assigns type to token of identifier
parseIdExprAssignment :: TokIdent -> EXPR Type -> Env -> Errors -> PosEnds -> SSAState (Env, Errors, Stmt Env Type)
parseIdExprAssignment tkId@(TokIdent (idPos, idVal)) expr env errors posEnds = case Env.lookup idVal env of
    Just (VarType mod envPos envType addr) ->
        if envType == exprType
            
            then return (
                env,
                errors,
                StmtAssign (BaseExpr (Identifier tkId) envType) expr -- annoto literal con il tipo corretto
                )
            
            else case (envType, exprType ) of
                -- 3 cases: 1) casting int->real, 2) casting char->string, 3) incompatible types
                (TypeBaseType BaseType_real, TypeBaseType BaseType_integer) -> 
                    return (env, errors, StmtAssign (BaseExpr (Identifier tkId) envType) (IntToReal expr) )
                (TypeBaseType BaseType_string, TypeBaseType BaseType_char) -> 
                    return (env, errors, StmtAssign (BaseExpr (Identifier tkId) envType) (CharToString expr) )

                -- cases in which an error was previously generated: no new error messages, pass the error forwards
                (TypeBaseType BaseType_error, _) -> 
                    return (env, errors, StmtAssign (BaseExpr (Identifier tkId) (TypeBaseType BaseType_error)) expr )
                (_, TypeBaseType BaseType_error) -> 
                    return (env, errors, StmtAssign (BaseExpr (Identifier tkId) (TypeBaseType BaseType_error)) expr )

                (_, _)  -> return (
                    env,
                    ("Error in range " ++ show posEnds ++ ". id "++ idVal ++ " is of type " ++ show envType ++ " but is assigned value of type " ++ show exprType) :errors, 
                    StmtAssign (BaseExpr (Identifier tkId) envType) expr 
                    )

    Nothing -> return (
        env,
        ("Error in range " ++ show posEnds ++
        ". Unknown identifier: " ++ idVal ++ " is used but has never been declared."):errors,
        StmtAssign (BaseExpr (Identifier tkId) (TypeBaseType BaseType_error)) expr
        )

    where
        exprType = getTypeFromExpression expr


parseExprExprAssignment :: EXPR Type -> EXPR Type -> Env -> Errors -> PosEnds -> SSAState (Env, Errors, Stmt Env Type)
parseExprExprAssignment expr1 expr2 env errs posEnds = 
    -- TODO: includere posizione e stringa dell'espressione sinistra nel messaggio di errore
    if getTypeFromExpression expr1 == getTypeFromExpression expr2
        then return (env, errs, (StmtAssign expr1 expr2) )
        else case (getTypeFromExpression expr1, getTypeFromExpression expr2) of
            
            (TypeBaseType BaseType_real, TypeBaseType BaseType_integer) -> 
                return (env, errs, (StmtAssign expr1 (IntToReal expr2)))
            (TypeBaseType BaseType_string, TypeBaseType BaseType_char) -> 
                return (env, errs, (StmtAssign expr1 (CharToString expr2)))

            -- cases in which an error was previously generated: no new error messages, pass the error forwards
            (TypeBaseType BaseType_error, _) -> 
                return (env, errs, (StmtAssign expr1 expr2))
            (_, TypeBaseType BaseType_error) -> 
                return (env, errs, (StmtAssign expr1 expr2))
        
            _ -> return (
                env, 
                ("Error in range " ++ show posEnds ++ ". l-Expression "++ showExpr expr1 ++ " is of type " ++ show (getTypeFromExpression expr1) ++ " but is assigned value of type " ++ show (getTypeFromExpression expr2)):errs, 
                (StmtAssign expr1 expr2)
                )


-- Given current environment, errors and syntax tree, returns annotated tree and updated environment and errors
parseExpression :: Env -> Errors -> EXPR infType -> SSAState (Env, Errors, EXPR Type, PosEnds)

--TODO: ottenere informazione sulla posizione per stamparla nel messaggio di errore per tutti i casi
--TODO: evitare messaggi di errore indotti (conseguenza di assegnazioni del tipo error)

-- Boolean Unary Negation
parseExpression env errs (UnaryExpression Not exp t) = do
    (env2, errs2, parsedexp, posEnds) <- parseExpression env errs exp
    if getTypeFromExpression parsedexp == TypeBaseType BaseType_boolean || getTypeFromExpression parsedexp == TypeBaseType BaseType_error 
        then -- no new errors are generated
            return (
                env2, 
                errs2, 
                (UnaryExpression Not parsedexp (sup (getTypeFromExpression parsedexp) (TypeBaseType BaseType_boolean)) ),
                posEnds
                )
        else
            return (
                env2, 
                ("Error"++". Boolean negation 'not' applied to type " ++ show (getTypeFromExpression parsedexp) ++ " instead of boolean type."):errs2, 
                (UnaryExpression Not parsedexp (TypeBaseType BaseType_error) ),
                posEnds
                )
        

-- Arithmetic Unary Negation
parseExpression env errs (UnaryExpression Negation exp t) = do
    (env2, errs2, parsedexp, posEnds) <- parseExpression env errs exp
    if getTypeFromExpression parsedexp == TypeBaseType BaseType_integer || getTypeFromExpression parsedexp == TypeBaseType BaseType_real || getTypeFromExpression parsedexp == TypeBaseType BaseType_error 
        then
            return (
                env2,
                errs2,
                (UnaryExpression Negation parsedexp (sup (getTypeFromExpression parsedexp) (TypeBaseType BaseType_integer)) ),
                posEnds
                )
        else
            return (
                env2, 
                ("Error"++". Arithmetic unary minus '-' applied to type " ++ show (getTypeFromExpression parsedexp) ++ " instead of numeric type."):errs2, 
                (UnaryExpression Negation parsedexp (TypeBaseType BaseType_error) ),
                posEnds 
                )
        

-- Binary Boolean operations (And,Or)
parseExpression env errs (BinaryExpression And exp1 exp2 t) = parseBinaryBooleanExpression env errs And exp1 exp2
parseExpression env errs (BinaryExpression Or exp1 exp2 t) = parseBinaryBooleanExpression env errs Or exp1 exp2

-- Binary Arithmetic operations (Add,Sub,Mul,Div,Mod)
parseExpression env errs (BinaryExpression Add exp1 exp2 t) = parseBinaryArithmeticExpression env errs Add exp1 exp2
parseExpression env errs (BinaryExpression Sub exp1 exp2 t) = parseBinaryArithmeticExpression env errs Sub exp1 exp2
parseExpression env errs (BinaryExpression Mul exp1 exp2 t) = parseBinaryArithmeticExpression env errs Mul exp1 exp2
parseExpression env errs (BinaryExpression Div exp1 exp2 t) = parseBinaryArithmeticExpression env errs Div exp1 exp2
parseExpression env errs (BinaryExpression Mod exp1 exp2 t) = parseBinaryArithmeticExpression env errs Mod exp1 exp2

-- Binary numeric relations (Eq,NotEq,LessT,EqLessT,GreatT,EqGreatT)
parseExpression env errs (BinaryExpression Eq exp1 exp2 t) = parseBinaryRelationExpression env errs Eq exp1 exp2
parseExpression env errs (BinaryExpression NotEq exp1 exp2 t) = parseBinaryRelationExpression env errs NotEq exp1 exp2
parseExpression env errs (BinaryExpression LessT exp1 exp2 t) = parseBinaryRelationExpression env errs LessT exp1 exp2
parseExpression env errs (BinaryExpression EqLessT exp1 exp2 t) = parseBinaryRelationExpression env errs EqLessT exp1 exp2
parseExpression env errs (BinaryExpression GreatT exp1 exp2 t) = parseBinaryRelationExpression env errs GreatT exp1 exp2
parseExpression env errs (BinaryExpression EqGreatT exp1 exp2 t) = parseBinaryRelationExpression env errs EqGreatT exp1 exp2

-- Dereference
parseExpression env errs (UnaryExpression Dereference exp t)  = do 
    (env2, errs2, parsedexp, posEnds) <- parseExpression env errs exp
    return (
        env2,
        errs2, 
        (UnaryExpression Dereference parsedexp (TypeCompType (Pointer (getTypeFromExpression parsedexp))) ),
        posEnds
        )
--    case getTypeFromExpression parsedexp of
--        TypeBaseType basetype -> return (env2, errs2, (UnaryExpression Dereference parsedexp (TypeCompType (Pointer basetype)) ))
--        _ -> return (env2, ("Error. Dereference operation on type "++ show (getTypeFromExpression parsedexp) ++" is not allowed because it is not a base type."):errs2, (UnaryExpression Dereference parsedexp (TypeBaseType BaseType_error) ))
        

-- Reference
parseExpression env errs (UnaryExpression Reference exp t) = do
    (env2, errs2, parsedexp, posEnds) <- parseExpression env errs exp
    case getTypeFromExpression parsedexp of
        
        TypeCompType (Pointer t) ->
             return (env2, errs2, (UnaryExpression Reference parsedexp t), posEnds )

        TypeBaseType BaseType_error ->
             return (env2, errs2, (UnaryExpression Reference parsedexp (TypeBaseType BaseType_error)), posEnds )
        
        _ -> return (
            env2, 
            ("Error. Invalid reference '@' operation on type " ++ show (getTypeFromExpression parsedexp) ++ "."):errs2, 
            (UnaryExpression Reference parsedexp (TypeBaseType BaseType_error) ),
            posEnds
            )
    

-- Literals (base case of recursions)
parseExpression env errs (ExprLiteral literal) = do
    return (env, errs, (ExprLiteral literal), posEnds )
    where
        posEnds = getLitPosEnds literal

-- Function calls
parseExpression env errs (ExprCall call t) = parseExpressionCall env errs call

-- Base Expressions: identifies or array elements
parseExpression env errs (BaseExpr bexpr t) = do
    (env2, err2, parsedbexpr, t, posEnds) <- parseBaseExpression env errs bexpr
    return (env2, err2, (BaseExpr parsedbexpr t), posEnds)
        

-- Type casted expressions: verify that types match
-- Integer to Real
parseExpression env errs (IntToReal expr) = do
    (env2, errs2, parsedexpr, posEnds) <- parseExpression env errs expr
    case getTypeFromExpression parsedexpr of
        
        TypeBaseType BaseType_integer -> return (env2, errs2, (IntToReal parsedexpr), posEnds)
        
        TypeBaseType BaseType_real -> return (env2, ("Warning: removed unneeded implicit type casting from Integer to Real"):errs2, parsedexpr, posEnds) -- expression is real already: remove type casting wrapper
        
        TypeBaseType BaseType_error -> return (env2, errs2, (IntToReal parsedexpr), posEnds) -- in case of errors that already happened, no new error messages are generated

        _ -> return (
            env2, 
            ("Error: type casting from Integer to Real applied to type " ++ show (getTypeFromExpression parsedexpr) ++ "."):errs2, 
            (IntToReal parsedexpr),
            posEnds
            )

-- Char to String
parseExpression env errs (CharToString expr) = do
    (env2, errs2, parsedexpr, posEnds) <- parseExpression env errs expr
    case getTypeFromExpression parsedexpr of
        
        TypeBaseType BaseType_char -> return (env2, errs2, (CharToString parsedexpr), posEnds)
        
        TypeBaseType BaseType_string -> return (env2, ("Warning: removed unneeded implicit type casting from Char to String"):errs2, parsedexpr, posEnds) -- expression is string already: remove type casting wrapper
        TypeBaseType BaseType_error -> return (env2, errs2, (CharToString parsedexpr), posEnds) -- in case of errors that already happened, no new error messages are generated
        
        _ -> return (
            env2, 
            ("Error: type casting from Char to String applied to type " ++ show (getTypeFromExpression parsedexpr) ++ "."):errs2, 
            (CharToString parsedexpr),
            posEnds
            )


-- parseExpression env errs expr = return (env, errs, (ExprLiteral (LiteralInteger (TokInteger ((0,0), "10")))) ) -- temporaneamente ogni espressione non specificata diventa il numero 10 (ora ridondante)

parseStatementCall :: Env -> Errors -> Call infType -> SSAState (Env, Errors, Stmt Env Type)
parseStatementCall env errs (CallArgs tkId@(TokIdent (tokpos,tokid)) args ) = do
    (env2, err2, parsedcall, t, posEnds) <- parseCall env errs (CallArgs tkId args )
    return (env2, err2, (StmtCall parsedcall) )

parseExpressionCall :: Env -> Errors -> Call infType -> SSAState (Env, Errors, EXPR Type, PosEnds)
parseExpressionCall env errs (CallArgs tkId@(TokIdent (tokpos,tokid)) args ) = do
    (env2, err2, parsedcall, t, posEnds) <- parseCall env errs (CallArgs tkId args )
    return (env2, err2, (ExprCall parsedcall t), posEnds )

parseCall :: Env -> Errors -> Call infType -> SSAState (Env, Errors, Call Type, Type, PosEnds)
parseCall env errs call@(CallArgs tkId@(TokIdent (tokpos@(x,y),tokid)) args ) = do
    --posEnds contiene la posizione dell'argomento più a destra
    (env2, errs2, parsedargs, posEndsArgs) <- parseArguments env errs args [] posEndsToken
    
    case Env.lookup tokid env of
        Just (Function pos parameters t _) -> do
            --TODO: implementare ritorno posEnds da parseFunction
            (env, errs, clType, tp) <- parseFunction env2 errs2 (CallArgs tkId parsedargs) parameters t [] posEndsToken{rightmost = rightmost posEndsArgs}
            return (env, errs, clType, tp, posEndsToken)
        
        Just (Procedure pos parameters _) -> do
            --TODO: implementare ritorno posEnds da parseProcedure
            (env, errs, clType, tp) <- parseProcedure env2 errs2 (CallArgs tkId parsedargs) parameters [] posEndsToken{rightmost = rightmost posEndsArgs}
            return (env, errs, clType, tp, posEndsToken)

        Just (Constant pos t addr) -> do
            return (
                env2, 
                ("Error at: " ++ show posEndsToken ++". Identifier " ++ tokid ++" is used as a function/procedure but it is a constant."):errs2,
                (CallArgs tkId parsedargs ), 
                (TypeBaseType BaseType_error), 
                posEndsToken 
                )
        
        Just (VarType mod pos t addr) -> do
            return (
                env2, 
                ("Error at: " ++ show posEndsToken ++". Identifier " ++ tokid ++" is used as a function/procedure but it is a variable."):errs2,
                (CallArgs tkId parsedargs ),
                (TypeBaseType BaseType_error), 
                posEndsToken 
                ) 
        
        Nothing -> do
            return (
                env2, 
                ("Error at: " ++ show posEndsToken ++". Unknown identifier: " ++ tokid ++" is used but has never been declared."):errs2,
                (CallArgs tkId parsedargs ), 
                (TypeBaseType BaseType_error), 
                posEndsToken 
                ) 

        where
            -----------------TODO------------------------- implementare posEnds per funzioni/procedure
            posEndsToken = PosEnds { leftmost = tokpos, rightmost = (x, y + length tokid) }
        

parseArguments :: Env -> Errors -> [EXPR infType] -> [EXPR Type]-> PosEnds -> SSAState (Env, Errors, [EXPR Type], PosEnds)
parseArguments env errs [] res posEnds = do
    return (env, errs, res, posEnds)
parseArguments env errs (arg:args) res posEnds = do
    (env2, err2, parsedexpr, argPosEnds) <- parseExpression env errs arg
    parseArguments env2 err2 args (res++[parsedexpr]) argPosEnds
        

-- Last parameter is list of parsed expressions (call arguments) that have been type casted if needed
-- Parameters: env, errors, call, params, parsedargs --TODO: dire nel messaggio di errore di mismatch quanti parametri sono previsti?
parseFunction :: Env -> Errors -> Call Type -> Prms -> Type -> [EXPR Type] -> PosEnds -> SSAState (Env, Errors, Call Type, Type)
parseFunction env errs (CallArgs tkId@(TokIdent (tokpos,tokid)) [] ) NoParams t pargs posEnds = 
    return (env, errs, (CallArgs tkId pargs ), t )

parseFunction env errs (CallArgs tkId@(TokIdent (tokpos,tokid)) [] ) (Params prms) t pargs posEnds = 
    return (
        env, 
        ("Error at " ++ show tokpos ++ " in function "++tokid++": mismatch in number of arguments"):errs, 
        (CallArgs tkId pargs ), 
        (TypeBaseType BaseType_error) 
        )

parseFunction env errs (CallArgs tkId@(TokIdent (tokpos,tokid)) args ) NoParams t pargs posEnds = return (
    env, 
    ("Error at " ++ show tokpos ++ " in function "++tokid++": mismatch in number of arguments"):errs, 
    (CallArgs tkId (pargs++args) ), 
    (TypeBaseType BaseType_error) 
    )

parseFunction env errs (CallArgs tkId@(TokIdent (tokpos,tokid)) args ) (Params (prm:prms) ) t pargs posEnds = do
    (env2, err2, args2, pargs2) <- compareArguments env errs posEnds args prm pargs
    
    newparams <- case prms of
                    [] -> return NoParams
                    _ -> return (Params prms)

    parseFunction env2 err2 (CallArgs tkId args2 ) newparams t pargs2 posEnds

    {-if compatible
        then
            parseFunction env2 err2 (CallArgs tkId args2 ) newparams t pargs2 posEnds
        else
            parseFunction env2 err2 (CallArgs tkId args2 ) newparams (TypeBaseType BaseType_error) pargs2 posEnds-}
      
        

parseProcedure :: Env -> Errors -> Call Type -> Prms -> [EXPR Type] -> PosEnds -> SSAState (Env, Errors, Call Type, Type)
parseProcedure env errs (CallArgs tkId@(TokIdent (tokpos,tokid)) [] ) NoParams pargs posEnds = 
    return (
        env, 
        errs, 
        (CallArgs tkId pargs ), 
        (TypeBaseType BaseType_void)
        )

parseProcedure env errs (CallArgs tkId@(TokIdent (tokpos,tokid)) [] ) (Params prms) pargs posEnds = 
    return (
        env, 
        ("Error at " ++ show tokpos ++ " in procedure "++tokid++": mismatch in number of arguments"):errs, 
        (CallArgs tkId pargs ), 
        (TypeBaseType BaseType_error) 
        )

parseProcedure env errs (CallArgs tkId@(TokIdent (tokpos,tokid)) args ) NoParams pargs posEnds = 
    return (
        env, 
        ("Error at " ++ show tokpos ++ " in procedure "++tokid++": mismatch in number of arguments"):errs, 
        (CallArgs tkId (pargs++args) ), 
        (TypeBaseType BaseType_error) 
        )

parseProcedure env errs (CallArgs tkId@(TokIdent (tokpos,tokid)) args ) (Params (prm:prms) ) pargs posEnds = do
    (env2, err2, args2, pargs2) <- compareArguments env errs posEnds args prm pargs
    newparams <- case prms of
                [] -> return NoParams
                _ -> return $ Params prms

    parseProcedure env2 err2 (CallArgs tkId args2 ) newparams pargs2 posEnds

    {-if compatible
        then
            parseProcedure env2 err2 (CallArgs tkId args2 ) newparams pargs2 posEnds
        else
            parseProcedure env2 (("Error at " ++ show tokpos ++ ": arguments not compatible"):err2) (CallArgs tkId args ) newparams pargs2 posEnds
-}



-- Parses arguments of the same type defined together and print accurate error messages with position of arguments
-- Boolean parameter keeps track of whether all arguments are of correct type, Last list of expressions are function call arguments type casted if needed
compareArguments :: Env -> Errors -> PosEnds -> [EXPR Type] -> Prm -> [EXPR Type] -> SSAState (Env, Errors, [EXPR Type], [EXPR Type])
compareArguments env errs p [] (Param _ [] t) pargs = return (env, errs, [] , pargs)
compareArguments env errs p args (Param _ [] t) pargs = return (env, errs, args, pargs)
compareArguments env errs p [] (Param _ toks t) pargs = return (env, errs, [], pargs)

compareArguments env errs p (expr:args) (Param m ((IdElement (TokIdent (parpos@(x,y),parid))):toks) t) pargs = do
    --(env2, err2, parsedexpr, posEnds) <- parseExpression env errs expr
    
    -- confronto tra parametri, diversi casi: 1) int to real, 2) real to int, 3) incompatibile --TODO: type casting CharToString?
    if argExprType == t
        then
            compareArguments env errs p args (Param m toks t) (pargs++[expr])
        else
            case (argExprType, t) of
                
                (TypeBaseType BaseType_integer, TypeBaseType BaseType_real) -> 
                    compareArguments env errs p args (Param m toks t) (pargs++[(IntToReal expr)])
                
                (TypeBaseType BaseType_char, TypeBaseType BaseType_string) -> 
                    compareArguments env errs p args (Param m toks t) (pargs++[(CharToString expr)])

                -- error in expression of parameter of function/procedure call, no new error messages are generated
                (TypeBaseType BaseType_error, _) -> 
                    compareArguments env errs p args (Param m toks t) (pargs++[expr])
                
                _ -> compareArguments env (("Error at "++ show p ++": the argument "++ showExpr expr ++" is of type " ++ show argExprType ++ " but it should be of type " ++ show t ++" as specified by parameter "++ parid ++ " at "++ show parPosEnds):errs) p args (Param m toks t) (pargs++[expr])
    where
        argExprType = getTypeFromExpression expr
        parPosEnds = PosEnds{ leftmost = parpos, rightmost = (x, y + length parid)}
        

parseBinaryBooleanExpression :: Env -> Errors -> BinaryOperator -> EXPR infType -> EXPR infType -> SSAState (Env, Errors, EXPR Type, PosEnds)
parseBinaryBooleanExpression env errs op exp1 exp2 = do
    (env2, errs2, parsedexp1, posEndsL) <- parseExpression env errs exp1
    (env3, errs3, parsedexp2, posEndsR) <- parseExpression env2 errs2 exp2
    
    if getTypeFromExpression parsedexp1 /= TypeBaseType BaseType_boolean && getTypeFromExpression parsedexp1 /= TypeBaseType BaseType_error
        then return (
            env3, 
            ("Error. " ++ "First argument of "++ getStringFromOperator op ++" operator is of type " ++ show (getTypeFromExpression parsedexp1) ++ " instead of boolean."):errs3, 
            (BinaryExpression op parsedexp1 parsedexp2 (TypeBaseType BaseType_error) ),
            PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR}
            )
        
        else if getTypeFromExpression parsedexp2 /= TypeBaseType BaseType_boolean && getTypeFromExpression parsedexp2 /= TypeBaseType BaseType_error
            then return (
                env3, 
                ("Error. " ++ "Second argument of "++ getStringFromOperator op ++" operator is of type " ++ show (getTypeFromExpression parsedexp2) ++ " instead of boolean."):errs3, 
                (BinaryExpression op parsedexp1 parsedexp2 (TypeBaseType BaseType_error) ),
                PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR}
                )
            -- if one of the expressions is of type Error, no new messages are produced
            else return (
                env3, 
                errs3, 
                (BinaryExpression op parsedexp1 parsedexp2 (sup (getTypeFromExpression parsedexp1) (getTypeFromExpression parsedexp2)) ), 
                PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR})
    where
        -- string form of operator for printing error messages
        getStringFromOperator :: BinaryOperator -> [Char]
        getStringFromOperator And = "'And'"
        getStringFromOperator Or = "'Or'"
        getStringFromOperator _ = ""


parseBinaryArithmeticExpression :: Env -> Errors -> BinaryOperator -> EXPR infType -> EXPR infType -> SSAState (Env, Errors, EXPR Type, PosEnds)
parseBinaryArithmeticExpression env errs op exp1 exp2 = do
    (env2, errs2, parsedexp1, posEndsL) <- parseExpression env errs exp1
    (env3, errs3, parsedexp2, posEndsR) <- parseExpression env2 errs2 exp2
    
    -- 3 cases: 1) first element not numeric, 2) second argument not numeric, 3) no errors
    if getTypeFromExpression parsedexp1 /= TypeBaseType BaseType_integer && getTypeFromExpression parsedexp1 /= TypeBaseType BaseType_real && getTypeFromExpression parsedexp1 /= TypeBaseType BaseType_error
        then return (
            env3, 
            
            -------------TODO: esempio uso PosEnds nei messaggi di errore

            ("Error at " ++ show PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR}  ++ ": First argument of "++ getStringFromOperator op ++ " operator is of type " ++ show (getTypeFromExpression parsedexp1) ++ " instead of numeric (integer or real)."):errs3, 
            (BinaryExpression op parsedexp1 parsedexp2 (TypeBaseType BaseType_error) ),
            PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR}
            )
        --TODO: se ho un'espressione del tipo "true+false" viene segnalato come errato soltanto il primo argomento e non il secondo
        else if getTypeFromExpression parsedexp2 /= TypeBaseType BaseType_integer && getTypeFromExpression parsedexp2 /= TypeBaseType BaseType_real && getTypeFromExpression parsedexp2 /= TypeBaseType BaseType_error
            then return (
                env3, 
                ("Error. " ++ "Second argument of "++getStringFromOperator op++" operator is of type " ++ show (getTypeFromExpression parsedexp2) ++ " instead of numeric (integer or real)."):errs3, 
                (BinaryExpression op parsedexp1 parsedexp2 (TypeBaseType BaseType_error) ),
                PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR}
                )
            
            -- 3 casi: 1) primo int secondo real, 2) primo real secondo int, 3) no type casting
            else case (getTypeFromExpression parsedexp1, getTypeFromExpression parsedexp2) of
                    
                    (TypeBaseType BaseType_integer, TypeBaseType BaseType_real) -> 
                        return (env3, errs3, (BinaryExpression op (IntToReal parsedexp1) parsedexp2 (getType op parsedexp1 parsedexp2) ), PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR})
                    
                    (TypeBaseType BaseType_real, TypeBaseType BaseType_integer) -> 
                        return (env3, errs3, (BinaryExpression op parsedexp1 (IntToReal parsedexp2) (getType op parsedexp1 parsedexp2) ), PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR})
                   
                    _ -> return (env3, errs3, (BinaryExpression op parsedexp1 parsedexp2 (getType op parsedexp1 parsedexp2) ), PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR})                   
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


parseBinaryRelationExpression :: Env -> Errors -> BinaryOperator -> EXPR infType -> EXPR infType -> SSAState (Env, Errors, EXPR Type, PosEnds)
parseBinaryRelationExpression env errs op exp1 exp2 = do
    (env2, errs2, parsedexp1, posEndsL) <- parseExpression env errs exp1
    (env3, errs3, parsedexp2, posEndsR) <- parseExpression env2 errs2 exp2
    
    -- 3 cases: 1) first element not numeric, 2) second argument not numeric, 3) no errors
    if getTypeFromExpression parsedexp1 /= TypeBaseType BaseType_integer && getTypeFromExpression parsedexp1 /= TypeBaseType BaseType_real && getTypeFromExpression parsedexp1 /= TypeBaseType BaseType_error 
        then return (
            env3, 
            ("Error. " ++ "First argument of "++ getStringFromOperator op ++ "operator is of type " ++ show (getTypeFromExpression parsedexp1) ++ " instead of numeric (integer or real)."):errs3, 
            (BinaryExpression op parsedexp1 parsedexp2 (TypeBaseType BaseType_error) ),
            PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR}
            )

        else if getTypeFromExpression parsedexp2 /= TypeBaseType BaseType_integer && getTypeFromExpression parsedexp2 /= TypeBaseType BaseType_real && getTypeFromExpression parsedexp2 /= TypeBaseType BaseType_error 
            then return (
                env3, 
                ("Error. " ++ "Second argument of "++getStringFromOperator op++" operator is of type " ++ show (getTypeFromExpression parsedexp2) ++ " instead of numeric (integer or real)."):errs3,
                (BinaryExpression op parsedexp1 parsedexp2 (TypeBaseType BaseType_error) ),
                PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR}
                )
            
             -- result is of boolean type unless one of the expressions contains an error
            else if getTypeFromExpression parsedexp1 == TypeBaseType BaseType_error || getTypeFromExpression parsedexp2 == TypeBaseType BaseType_error
                then return (
                        env3, 
                        errs3, 
                        (BinaryExpression op parsedexp1 parsedexp2 (TypeBaseType BaseType_error) ),
                        PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR}
                        ) 
                else return (
                        env3, 
                        errs3, 
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
parseBaseExpression:: Env -> Errors -> BEXPR infType -> SSAState (Env, Errors, BEXPR Type, Type, PosEnds)

parseBaseExpression env errs (Identifier tkId@(TokIdent (tokpos@(x,y),tokid)) ) = 
    case Env.lookup tokid env of
        
        Just (VarType mod _ envType addr) -> return (env, errs, (Identifier tkId ), envType, posEnds)
        
        Just (Constant _ envType addr) -> return (env, errs, (Identifier tkId ), envType, posEnds)
        
        Nothing -> return (
            env, 
            ("Error at " ++ show tokpos ++". Unknown identifier: " ++ tokid ++" is used but has never been declared."):errs, (Identifier tkId), 
            TypeBaseType BaseType_error,
            posEnds
            )

        where
            posEnds = PosEnds { leftmost = tokpos, rightmost = (x, y + length tokid) }

parseBaseExpression env errs (ArrayElem bexpr iexpr) = do
    --TODO: con parsediexpr sarebbe necessario verifiicare se l'indice rientra entro i limiti dell'array
    (env2, err2, parsediexpr, posEndsR) <- parseExpression env errs iexpr -- parsing of index for type checking (and casting if needed)
    (env3, err3, parsedbexpr, posEndsL) <- parseExpression env2 err2 bexpr -- parsing of base expression to get its type: if it is an array type, return type of element of that array; otherwise it is an error

    case (getTypeFromExpression parsedbexpr, getTypeFromExpression parsediexpr) of -- TODO: refactoring possibile di questa parte di codice? --TODO: posizione nei messaggi di errore
        
        -- 4 cases: 1) array and integer; 2) array and error; 3) error and integer; 4) error and error (distinction necessary to generate appropriate error messages)
        -- + 2 cases of Error types: do not print any new error messages
        (_, TypeBaseType BaseType_error) -> 
            return (
                env3, 
                err3, 
                (ArrayElem parsedbexpr parsediexpr), 
                TypeBaseType BaseType_error,
                PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR}
                )

        (TypeBaseType BaseType_error, _) -> 
            return (
                env3, 
                err3, 
                (ArrayElem parsedbexpr parsediexpr), 
                TypeBaseType BaseType_error,
                PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR}
                )
        
        (TypeCompType (Array i1 i2 basetype), TypeBaseType BaseType_integer) -> 
            return (env3, err3, (ArrayElem parsedbexpr parsediexpr), basetype, PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR})

        (TypeCompType (Array i1 i2 basetype), _) -> 
            return (
                env3, 
                ("Error. Array index is not of numeric type but it is of type "++show (getTypeFromExpression parsediexpr)++"."):err3, 
                (ArrayElem parsedbexpr parsediexpr), 
                TypeBaseType BaseType_error,
                PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR}
                )
        
        (_, TypeBaseType BaseType_integer) -> 
            return (
                env3, 
                ("Error. Expression " ++ showExpr parsedbexpr ++ " is treated as an array but it is of type " ++ show (getTypeFromExpression parsedbexpr) ++ "."):err3, 
                (ArrayElem parsedbexpr parsediexpr), 
                TypeBaseType BaseType_error,
                PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR}
                )
        
        _ -> return (
            env3, 
            ("Error. Expression " ++ showExpr parsedbexpr ++ " is treated as an array but it is of type" ++ show (getTypeFromExpression parsedbexpr) ++ "."):("Error. Array index is not of numeric type but it is of type "++show (getTypeFromExpression parsediexpr)++"."):err3, 
            (ArrayElem parsedbexpr parsediexpr), 
            TypeBaseType BaseType_error,
            PosEnds { leftmost = leftmost posEndsL, rightmost = rightmost posEndsR}
            )


-- Returns annotated type for expressions
getTypeFromExpression :: EXPR Type -> Type
getTypeFromExpression (UnaryExpression op exp t) = t
getTypeFromExpression (BinaryExpression op exp1 exp2 t) = t
getTypeFromExpression (ExprLiteral literal) = (TypeBaseType (getTypeFromLiteral literal) )
getTypeFromExpression (ExprCall call t) = t
getTypeFromExpression (BaseExpr bexp t) = t
getTypeFromExpression (IntToReal _) = TypeBaseType BaseType_real
getTypeFromExpression (CharToString _) = TypeBaseType BaseType_string

getTypeFromBaseExpression:: BEXPR Type -> Env -> Type
getTypeFromBaseExpression (Identifier (TokIdent (tokpos, tokid)) ) env = case Env.lookup tokid env of
    Just (VarType _ _ envType _) -> envType
    Just (Constant _ envType _) -> envType
    Just _ -> TypeBaseType BaseType_error
    Nothing -> TypeBaseType BaseType_error
getTypeFromBaseExpression (ArrayElem bexpr iexpr) env = getTypeFromExpression bexpr

-- Type compatibility for operations
sup :: Type -> Type -> Type
sup t1 t2
    | t1 == t2 = t1
    | t1 == (TypeBaseType BaseType_integer) && t2 == (TypeBaseType BaseType_real) = (TypeBaseType BaseType_real)
    | t1 == (TypeBaseType BaseType_real) && t2 == (TypeBaseType BaseType_integer) = (TypeBaseType BaseType_real)
    | t1 == (TypeBaseType BaseType_char) && t2 == (TypeBaseType BaseType_string) = (TypeBaseType BaseType_string)
    | t1 == (TypeBaseType BaseType_string) && t2 == (TypeBaseType BaseType_char) = (TypeBaseType BaseType_string)
    | otherwise = (TypeBaseType BaseType_error)


getLitPosEnds :: Literal -> PosEnds
getLitPosEnds (LiteralInteger (TokInteger (pos@(x,y), val))) = PosEnds {leftmost=pos, rightmost = (x, y + (length val))}
getLitPosEnds (LiteralChar (TokChar (pos@(x,y), val))) = PosEnds {leftmost=pos, rightmost = (x, y + (length val))}
getLitPosEnds (LiteralString (TokString (pos@(x,y), val))) = PosEnds {leftmost=pos, rightmost = (x, y + (length val))}
getLitPosEnds (LiteralBoolean (TokBoolean (pos@(x,y), val))) = PosEnds {leftmost=pos, rightmost = (x, y + (length val))}
getLitPosEnds (LiteralDouble (TokDouble (pos@(x,y), val))) = PosEnds {leftmost=pos, rightmost = (x, y + (length val))}



insertVar :: String -> EnvData -> SSAState ()
insertVar id entry = do
    state <- get
    (locUninit, poppedStack) <- pop $ unInitVars state
    newLocUninit <- Env.insert id entry locUninit
    newStack <- push newLocUninit poppedStack
    put (state {unInitVars = newStack})
    return ()

removeVar :: String -> SSAState ()
removeVar id = do
    state <- get
    newStack <- recRem id (unInitVars state)
    put $ state{ unInitVars = newStack }
    where
        recRem :: String -> [Env] -> SSAState ([Env])
        recRem _ [] = return []
        recRem key (topEnv:rest) = 
            if member key topEnv 
                then do
                    return $ (delete key topEnv):rest
                else do
                    recRest <- recRem key rest
                    return $ topEnv : recRest


pushNewUninit :: SSAState()
pushNewUninit = do
    state <- get
    newStack <- push emptyEnv (unInitVars state)
    put $ state {unInitVars = newStack}

popUninit :: Errors -> SSAState (Errors)
popUninit errs = do
    state <- get
    (locUninit, rest) <- pop $ unInitVars state
    newErrs <- makeUninitErrs errs (toList locUninit)
    put $ state { unInitVars = rest }
    return newErrs
    where
        makeUninitErrs :: Errors -> [(String, EnvData)] -> SSAState (Errors)
        makeUninitErrs errs [] = return errs
        makeUninitErrs errs ((id,(VarType _ pos@(x,y) _ _)):rest) = makeUninitErrs errMsg rest
            where
                posEnds = PosEnds { leftmost = pos, rightmost = (x, y + length id) }
                errMsg = ("Error at " ++ show posEnds ++ ": Variable " ++ id ++ " has never been initialized"): errs 