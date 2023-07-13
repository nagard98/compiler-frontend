module TypeChecker where
import AbsGrammar
import Env
import Text.Parsec (parserReturn)
import Data.Text.Array (new)
import HelperTAC
import Control.Monad.State.Strict
import Debug.Trace

type Errors = [String]
emptyErrors :: [String]
emptyErrors = []

launchStatSemAnalysis :: P env infType -> (Env, Errors, P Env Type)
launchStatSemAnalysis tree = evalState (parseTree tree emptyEnv {-defaultEnv-} emptyErrors) 0

-- Type Checking starting point
parseTree :: P env infType -> Env -> Errors -> StateCount (Env, Errors, P Env Type)
-- TODO: anche qui valutare se restituire newEnv o env1; Probabilmente dovremmo aggiungere anche un
-- campo env a Prog che indichi l'environment globale
parseTree (Prog pBlock dclBlock beBlock _) env errs = do
    (globEnv, errors1, dBlks) <- parseDclBlocks env errs dclBlock
        -- errors and env are propagated from declaration block into beginEnd Block!
        -- notice that globEnv is the env after parsing declaration blocks
    (newEnv, newErrors, beBlks) <- parseBEBlock globEnv errors1 beBlock
    
    return (newEnv, newErrors, Prog pBlock dBlks beBlks globEnv)


-- Navigates syntax tree and saves info about variables type (declared in a Declaration block) in the global environment
parseDclBlocks:: Env -> Errors -> [DclBlock env infType] -> StateCount (Env, Errors, [DclBlock Env Type])
parseDclBlocks env errors (x:xs) = do
    (env1, errors1, newBlock) <- parseSingleDclBlock env errors x
    (finalEnv, finalErrors, newBlocks) <- parseDclBlocks env1 errors1 xs
    return (finalEnv, finalErrors, newBlock : newBlocks)
        
parseDclBlocks env errors [] = return (env, errors, [])

 -- TODO: make sure errores are updated after parsing declaration blocks
 -- e.g. redefining a variable could produce a warning and redefinig a constant an error
parseSingleDclBlock :: Env -> Errors -> DclBlock env infType -> StateCount (Env, Errors, DclBlock Env Type)
parseSingleDclBlock env errors blk = case blk of

    DclBlockVrBlock _ -> parseDclVrBlock env errors blk --return (newEnv, errors, DclBlockVrBlock (VarBlock vrDefs))

    -- add info about constants to the environment
    DclBlockCsBlock _ -> parseDclCsBlock env errors blk

    -- add info about functions to the environment
    -- info are: function position, function name, parameters, return type
    -- TODO: pass global environment to begin-end block and parse inner statemets
    DclBlockFcBlock _ -> parseDclFcBlock env errors blk

    -- add info about procedures to the environment. Same as functions but without return type
    DclBlockPcBlock _ -> parseDclPcBlock env errors blk


parseDclVrBlock :: Env -> Errors -> DclBlock env infType -> StateCount (Env, Errors, DclBlock Env Type)
parseDclVrBlock env errors (DclBlockVrBlock (VarBlock vrDefs)) = do
    newEnv <- parseVrDefs vrDefs env;
    -- add info about variables to the environment
    return (newEnv, errors, DclBlockVrBlock (VarBlock vrDefs))
        where
            -- savese info about variables type in env 
            parseVrDefs :: [VrDef] -> Env -> StateCount Env
            parseVrDefs ((VarDefinition idElements t):vrDefs) env = do
                -- TODO : probabilmente necessaria gestione errori (ovvero restituire anche errori)
                (tmpEnv, tmpErrs, _) <- parseIds idElements Modality_val t env []
                return tmpEnv
            parseVrDefs _ env = return env
                        
parseDclCsBlock :: Env -> Errors -> DclBlock env infType -> StateCount (Env, Errors, DclBlock Env Type)
parseDclCsBlock env errors (DclBlockCsBlock (ConstBlock csDefs)) = do 
    newEnv <- parseConsDefs csDefs env
    return (newEnv, errors, DclBlockCsBlock (ConstBlock csDefs))
        where
            -- savese info about constants type in env 
            parseConsDefs :: [CsDef] -> Env -> StateCount Env
            parseConsDefs ((ConstDefinition (IdElement (TokIdent (pos, id))) literal):cs) env = do 
                tmpEnv <- Env.insert id (Constant pos (TypeBaseType (getTypeFromLiteral literal)) (TacLit literal)) env
                newEnv <- parseConsDefs cs tmpEnv;
                return newEnv;
            parseConsDefs _ env = return env
            

parseDclFcBlock :: Env -> Errors -> DclBlock env infType -> StateCount (Env, Errors, DclBlock Env Type)
parseDclFcBlock env errors (DclBlockFcBlock fB@(FuncBlock idTok@(TokIdent (pos, id)) params retType beb)) = do 
    -- add to env return type (needed for type checking of the return statement) and function info
    -- IMPORTANT NOTE: env must be the secondo argument of mergeEnvs, otherwise the new "return" key will not be updated
    -- this is because the underlying function union (t1, t2) of Data.Map prefers t1 when duplicated keys are encountered
    fcAddr <- Env.newIdAddr id 
    tmpEnv <- Env.mergeEnvs (Env.fromList [(id, Function pos params retType fcAddr), ("return", Return retType id pos)]) env
    (tmpEnv2, tmpErrors2, annotatedParams) <- parseParams params [] tmpEnv errors
    (finalEnv, finalErrors, annotatedBEB) <- parseBEBlock tmpEnv2 tmpErrors2 beb

    return (finalEnv, finalErrors, DclBlockFcBlock (FuncBlock idTok params retType annotatedBEB))


parseDclPcBlock :: Env -> Errors -> DclBlock env infType -> StateCount (Env, Errors, DclBlock Env Type)
parseDclPcBlock env errors (DclBlockPcBlock pB@(ProcBlock idTok@(TokIdent (pos, id)) params beb)) = do
    pcAddr <- Env.newIdAddr id
    tmpEnv <- Env.insert id (Procedure pos params pcAddr) env
    (pEnv, pErrs, pPrms) <- parseParams params [] tmpEnv errors
    (fEnv, fErrs, annBEB) <- parseBEBlock pEnv errors beb

    return (tmpEnv, errors, DclBlockPcBlock (ProcBlock idTok params annBEB))


parseParams :: Prms -> [Prm] -> Env -> Errors -> StateCount (Env, Errors, Prms)
parseParams prms accPrms env errs = do 
    case prms of
        (Params ( p@(Param mod idList typ):ps )) -> do
            (tmpEnv, tmpErrs, _) <- parseIds idList mod typ env errs
            (pEnv,pErrs, pPrms) <- parseParams (Params ps) (p:accPrms) tmpEnv tmpErrs
            return (pEnv, pErrs, pPrms)
                
        (Params []) -> return (env, errs, Params accPrms)

        NoParams -> return (env, errs, prms)


parseIds :: [IdElem] -> Modality -> Type -> Env -> Errors -> StateCount (Env, Errors, [IdElem])
parseIds [] mod typ env errs = return (env, errs, [])
parseIds ( idElem@(IdElement (TokIdent (pos, id))):ids) mod typ env errs = do
    idAddr <- newIdAddr id
    tmpEnv <- Env.insert id (VarType mod pos typ idAddr) env
    (newEnv, newErrs, newIds) <- parseIds ids mod typ tmpEnv errs
    return (newEnv, errs, idElem:newIds)


-- parse the begin-end block and check the statements for type errors
parseBEBlock:: Env -> Errors -> BEBlock env infType -> StateCount (Env, Errors, BEBlock Env Type)
parseBEBlock env errors (BegEndBlock statements annEnv) = do
    (newEnv, newErrors, newStatements) <- parseStatements env errors statements
    return (newEnv, newErrors, BegEndBlock newStatements newEnv)
        

parseStatements:: Env -> Errors -> [Stmt env infType] -> StateCount (Env, Errors, [Stmt Env Type])
parseStatements env errors [] = return (env, errors, [])
parseStatements env errors allStmts =  q env errors allStmts []
        where
            q::Env -> Errors -> [Stmt env infType] -> [Stmt Env Type] -> StateCount (Env, Errors, [Stmt Env Type])
            q env errors [] annStmts = return (env, errors, annStmts)
            q env errors (s:xs) annStmts = do
                (env1, errors1, annStmt) <- parseStatement s env errors
                q env1 errors1 xs (annStmts++[annStmt])


parseStatement :: Stmt stmtenv infType -> Env -> Errors -> StateCount (Env, Errors, Stmt Env Type)
parseStatement stmt env errs = case stmt of
            -- tipologie di statement: dichiarazione, blocco, assegnamento, chiamata funzione, if-else, iterazione, return
            -- Dichiarazione
            (StmtDecl dclblock) -> do
                (env2, err2, block) <- parseSingleDclBlock env errs dclblock
                return (env2, err2, (StmtDecl block))
                    
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

            -------------------------------------------------------------


-- TODO: add positional info to errors
parseIter :: Stmt env infType -> Env -> Errors -> StateCount (Env, Errors, Stmt Env Type)
-- parsing of while-do statement
parseIter (StmtIter (StmtWhileDo expr stmt)) env errs = do
    (env1, errs1, parsedExpr) <- parseExpression env errs expr
    (newEnv, newErrs, parsedStmt) <- parseStatement stmt env1 errs1
    let wrappedStmt = wrapInBeginEnd parsedStmt newEnv
    if getTypeFromExpression parsedExpr == TypeBaseType BaseType_boolean
        then return (newEnv, newErrs, StmtIter (StmtWhileDo parsedExpr wrappedStmt))
        else return (newEnv, newErrs ++ ["ERROR in range " ++ show (rangeFromExpr expr)  ++ ": condition of while-do statement is not boolean"], StmtIter (StmtWhileDo parsedExpr wrappedStmt)) 

-- parsing of repeat-until statement
parseIter (StmtIter (StmtRepeat stmt expr)) env errs = do
    (env1, errs1, parsedStmt) <- parseStatement stmt env errs
    let wrappedStmt = wrapInBeginEnd parsedStmt env1
    (newEnv, newErrs, parsedExpr) <- parseExpression env1 errs1 expr
    if getTypeFromExpression parsedExpr == TypeBaseType BaseType_boolean
        then return (newEnv, newErrs, StmtIter (StmtRepeat wrappedStmt parsedExpr))
        else return (newEnv, newErrs ++ ["ERROR in range " ++ show (rangeFromExpr expr) ++ ": condition of repeat-until statement is not boolean"], StmtIter (StmtRepeat wrappedStmt parsedExpr))

parseSelection :: Stmt env infType -> Env -> Errors -> StateCount (Env, Errors, Stmt Env Type)
parseSelection (StmtSelect (StmtIf expr stmt)) env errs = do
    (env1, errs1, parsedExpr) <- parseExpression env errs expr
    (newEnv, newErrs, parsedStmt) <- parseStatement stmt env1 errs1
    let wrappedStmt = wrapInBeginEnd parsedStmt newEnv
    if getTypeFromExpression parsedExpr == TypeBaseType BaseType_boolean
        then return (newEnv, newErrs, StmtSelect (StmtIf parsedExpr wrappedStmt))
        else return (newEnv, newErrs ++ ["ERROR: condition of if statement is not boolean"], StmtSelect (StmtIf parsedExpr wrappedStmt))
parseSelection (StmtSelect (StmtIfElse expr stmt1 stmt2)) env errs = do
    (env1, errs1, parsedExpr) <- parseExpression env errs expr
    (newEnv1, newErrs1, parsedStmt1) <- parseStatement stmt1 env1 errs1
    let wrappedStmt1 = wrapInBeginEnd parsedStmt1 newEnv1
    (newEnv2, newErrs2, parsedStmt2) <- parseStatement stmt2 newEnv1 newErrs1
    let wrappedStmt2 = wrapInBeginEnd parsedStmt2 newEnv2
    if getTypeFromExpression parsedExpr == TypeBaseType BaseType_boolean
        then return (newEnv2, newErrs2, StmtSelect (StmtIfElse parsedExpr wrappedStmt1 wrappedStmt2))
        else return (newEnv2, newErrs2 ++ ["ERROR: condition of if-else statement is not boolean"], StmtSelect (StmtIfElse parsedExpr wrappedStmt1 wrappedStmt2))

-- If the provided statement is not insiede a begin-end block, wraps it in one
-- This is needed to simplify TAC generation by having to deal only with begin-end blocks containing statements
-- With the current grammar the body of a iteration (or an if-else) statement can be eather a single statement or a begin-end block with other statements
wrapInBeginEnd :: Stmt Env infType -> Env -> Stmt Env infType
wrapInBeginEnd (StmtComp (BegEndBlock stmts begEnv)) _ = StmtComp (BegEndBlock stmts begEnv)
wrapInBeginEnd stmt stmEnv = StmtComp (BegEndBlock [stmt] stmEnv)

parseReturn :: Stmt env infType -> Env -> Errors -> StateCount (Env, Errors, Stmt Env Type)
parseReturn (StmtReturn (Ret expr)) env errs = do
    (newEnv, newErrs, parsedExpr) <- parseExpression env errs expr
    case Env.lookup "return" env of 
        Just (Return expectedType funName funPos) ->
            if sup expectedType (getTypeFromExpression parsedExpr) /= expectedType
                then return ( newEnv,
                        ("Function " ++ funName ++ " at " ++ show funPos ++ 
                        " expects a " ++ show expectedType ++ " to be returned " ++
                        "but the expression following the return statement has type " ++ show (getTypeFromExpression parsedExpr) ) : newErrs, 
                        StmtReturn (Ret parsedExpr))
                else
                    -- everything is ok, return the parsed expression 
                    return (newEnv, newErrs, StmtReturn (Ret parsedExpr))
        Nothing -> 
            -- Theoretically this should never happen, 
            -- since the return type of the function is saved in the environment when the function prototype is parsed
            return (newEnv,
            "Internal type checking error: can't find expected return type of current function in the environment": newErrs,
            StmtReturn (Ret parsedExpr)) 
        

parseAssignment :: EXPR infType -> EXPR infType -> Env -> Errors -> StateCount (Env, Errors, Stmt Env Type)
parseAssignment expr1 expr2 env errs = case (expr1, expr2) of
            -- Assegno a variabile un letterale
            ( (BaseExpr (Identifier tId) tp), (ExprLiteral literal) ) -> parseLitAssignment tId literal env errs
            -- Assegno a variabile valore espressione generica: 1) parsing dell'espressione e trovo il tipo; 2) controllo compatibilità con letterale in assegnamento
            ( (BaseExpr (Identifier tId) tp), expr ) -> do
                (env2, errs2, parsedexpr) <- parseExpression env errs expr
                parseIdExprAssignment tId parsedexpr env2 errs2
            -- Puntatore è un l-value valido
            ( (UnaryExpression Dereference expr1 t), expr2 ) -> do
                (env2, err2, parsedexpr1) <- parseExpression env errs (UnaryExpression Dereference expr1 t)
                (env3, err3, parsedexpr2) <- parseExpression env2 err2 expr2
                parseExprExprAssignment parsedexpr1 parsedexpr2 env3 err3
            -- Riferimento ad un puntatore è un l-value valido            
            ( (UnaryExpression Reference expr1 t), expr2 ) -> do
                (env2, err2, parsedexpr1) <- parseExpression env errs (UnaryExpression Reference expr1 t)
                (env3, err3, parsedexpr2) <- parseExpression env2 err2 expr2                
                parseExprExprAssignment parsedexpr1 parsedexpr2 env3 err3
            -- Array elements are valid l-values
            ( (BaseExpr (ArrayElem idexpr iexpr) t), expr ) -> do
                (env2, err2, parsedexpr) <- parseExpression env errs expr
                traceM $ "Parsed expr to assign to array elem: " ++ show parsedexpr ++ "\n"
                
                --TODO: credo non ti serva fare qui il parse dell'indice; lo fai già in parseBaseExpression quando
                -- chiami parseExpression su un ArrayElem come fai nell'istruzione dove passi env3 ed err3.
                -- Per come ho modificato parseArrayAssignment non serve passare più l'indice, ma lascio comunque qui
                -- in quanto devo capire se intendevi fare qualcos altro
                -- NOTA: usa traceM per stampare in modo da fare debugging
                (env3, err3, parsediexpr) <- parseExpression env2 err2 iexpr
                traceM $ "Parsed expr used as index of array: " ++ show parsediexpr ++ "\n"
                
                (env4, err4, parsedidexpr) <- parseExpression env3 err3 (BaseExpr (ArrayElem idexpr iexpr) t)
                traceM $ "Parsed lexpr: "++show parsedidexpr ++ "\n"
                
                parseArrayAssignment parsedidexpr (getTypeFromExpression parsedidexpr) parsediexpr parsedexpr env4 err4
            -- Il resto dei possibili l-value non è valido
            ( expr1, expr2 ) -> do
                (env2, err2, parsedexpr1) <- parseExpression env errs expr1
                (env3, err3, parsedexpr2) <- parseExpression env2 err2 expr2
                -- TODO: includere posizione e stringa della espressione sinistra nel messaggio di errore
                return (env3, ("Error: invalid l-value in assignment"):err3, (StmtAssign parsedexpr1 parsedexpr2) )

-- Checks if r-expression matches typing with an l-expression that is an element of an array
parseArrayAssignment:: EXPR Type -> Type -> EXPR Type -> EXPR Type -> Env -> Errors -> StateCount (Env, Errors, Stmt Env Type)
-- base case: tokid[iexpr] = expr
{-parseArrayAssignment (BaseExpr (Identifier (TokIdent (tokpos, tokid))) t) ltype iexpr expr env errs = if ltype == getTypeFromExpression expr --TODO: risolvere errore assegnamento array ad array di array (vedere testfile4)
    then
        return (env, errs, (StmtAssign (BaseExpr (ArrayElem (BaseExpr (Identifier (TokIdent (tokpos, tokid))) (getTypeFromBaseExpression (Identifier (TokIdent (tokpos, tokid))) env) ) iexpr) ltype) expr ) )
    else
        -- 3 cases: a) int to real, b) real to int, c) incompatible types -> error
        case (ltype, getTypeFromExpression expr) of
            (TypeBaseType BaseType_real, TypeBaseType BaseType_integer) -> return (env, errs, (StmtAssign (BaseExpr (ArrayElem (BaseExpr (Identifier (TokIdent (tokpos, tokid))) (getTypeFromBaseExpression (Identifier (TokIdent (tokpos, tokid))) env) ) iexpr) ltype) (IntToReal expr) ) )

            --TODO: credo che i casting da real a int non si possano fare in nessun caso, considerando anche il nostro schema delle relazioni fra tipi
            (TypeBaseType BaseType_integer, TypeBaseType BaseType_real) -> return (env, errs, (StmtAssign (BaseExpr (ArrayElem (BaseExpr (Identifier (TokIdent (tokpos, tokid))) (getTypeFromBaseExpression (Identifier (TokIdent (tokpos, tokid))) env) ) iexpr) ltype) (RealToInt expr) ) )
            otherwise -> return (env, ("Error at " ++ show tokpos ++ ". l-Expression is of type " ++ show ltype ++ " but it is assigned value of type "++show (getTypeFromExpression expr) ++"."):errs, (StmtAssign (BaseExpr (ArrayElem (BaseExpr (Identifier (TokIdent (tokpos, tokid))) (getTypeFromBaseExpression (Identifier (TokIdent (tokpos, tokid))) env) ) iexpr) ltype) (IntToReal expr) ) )
-}
-- recursive case until id is found
parseArrayAssignment bExpr@(BaseExpr (ArrayElem bbexpr iiexpr) t) ltype iexpr expr env errs = do
    if t == rtype
        then return (env, errs, (StmtAssign bExpr expr))
            --parseArrayAssignment bbexpr ltype iiexpr expr env errs
        else 
            -- 3 cases: a) int to real, b) real to int, c) incompatible types -> error
            case (t, rtype) of
                (TypeBaseType BaseType_real, TypeBaseType BaseType_integer) -> return (env, errs, (StmtAssign bExpr (IntToReal expr) ) )

                --TODO: credo che i casting da real a int non si possano fare in nessun caso, considerando anche il nostro schema delle relazioni fra tipi
                --(TypeBaseType BaseType_integer, TypeBaseType BaseType_real) -> return (env, errs, (StmtAssign (BaseExpr (ArrayElem (BaseExpr (Identifier (TokIdent (tokpos, tokid))) (getTypeFromBaseExpression (Identifier (TokIdent (tokpos, tokid))) env) ) iexpr) ltype) (RealToInt expr) ) )
                
                --TODO: implementare posizione con il modo che avevamo discusso
                -- Durante il parsing le funzioni restituiscono anche 2 posizioni (quella più a sx e quella più a dx)
                otherwise -> return (env, ("Error at " ++ "TODO"{-show tokpos-} ++ ". l-Expression is of type " ++ show t ++ " but it is assigned value of type "++show rtype ++"."):errs, StmtAssign bExpr (IntToReal expr) )

    where
        -- Tipo del valore che si vuole assegnare
        rtype = getTypeFromExpression expr

-- generic expression that is not a base expression
parseArrayAssignment idexpr ltype iexpr expr env errs = if ltype == getTypeFromExpression expr --TODO: risolvere errore assegnamento array ad array di array (vedere testfile4)
    then
        return (env, errs, (StmtAssign idexpr expr) )
    else
        -- 2 cases: a) int to real, b) incompatible types -> error
        case (ltype, getTypeFromExpression expr) of
            (TypeBaseType BaseType_real, TypeBaseType BaseType_integer) -> return (env, errs, (StmtAssign idexpr (IntToReal expr) ) )
            otherwise -> return (env, ("Error. l-Expression is of type " ++ show ltype  ++ " but it is assigned value of type "++show (getTypeFromExpression expr) ++"."):errs, (StmtAssign idexpr expr) )


-- check if literal type matches with the one saved in the environment. 
-- If it doesn't return current environment and a new error message
parseLitAssignment:: TokIdent -> Literal -> Env -> Errors -> StateCount (Env, Errors, Stmt Env Type)
parseLitAssignment (TokIdent (idPos, idVal)) literal env errors = case Env.lookup idVal env of
    Just (VarType mod envPos envType addr) ->
        if envType == TypeBaseType (getTypeFromLiteral literal)
            then return (
                env,
                errors,
                -- NOTICE HOW WE ANNOTATE THE TREE, saving info about type of expr!
                StmtAssign (BaseExpr (Identifier (TokIdent (idPos, idVal))) envType) (ExprLiteral literal)
                )
            else case (envType, getTypeFromLiteral literal ) of
                -- 2 cases: casting int->real or incompatible types
                (TypeBaseType BaseType_real, BaseType_integer) -> return (env, errors, StmtAssign (BaseExpr (Identifier (TokIdent (idPos, idVal))) envType) (IntToReal (ExprLiteral literal)) )
                -- In case of errors the tree is not annotated. 
                -- TODO: maybe we should annotate it with the type of the literal? or don't annotate it at all?
                -- Per il momento ho aggiunto un come tipo envType
                (_, _)  -> return (env, ("Error at " ++ show idPos ++ ". Incompatible types: you can't assign a value of type " ++ show (getTypeFromLiteral literal) ++ " to " ++ idVal ++ " because it has type " ++ show envType) :errors, StmtAssign (BaseExpr (Identifier (TokIdent (idPos, idVal))) envType) (ExprLiteral literal) )
    Nothing -> return (env,
                ("Error at " ++ show idPos ++
                ". Unknown identifier: " ++ idVal ++
                " is used but has never been declared."):errors,
                StmtAssign (BaseExpr (Identifier (TokIdent (idPos, idVal))) (TypeBaseType BaseType_error)) (ExprLiteral literal))

-- Given identifier and expression (already parsed with inferred type!) assigns type to token of identifier
parseIdExprAssignment :: TokIdent -> EXPR Type -> Env -> Errors -> StateCount (Env, Errors, Stmt Env Type)
parseIdExprAssignment (TokIdent (idPos, idVal)) expr env errors = case Env.lookup idVal env of
    Just (VarType mod envPos envType addr) ->
        if envType == getTypeFromExpression expr
            then return (
                env,
                errors,
                StmtAssign (BaseExpr (Identifier (TokIdent (idPos, idVal))) envType) expr -- annoto literal con il tipo corretto
                )
            else case (envType, getTypeFromExpression expr ) of
                -- 2 cases: 1) casting int->real, 2) incompatible types
                (TypeBaseType BaseType_real, TypeBaseType BaseType_integer) -> return (env, errors, StmtAssign (BaseExpr (Identifier (TokIdent (idPos, idVal))) envType) (IntToReal expr) )
                (_, _)  -> return (env, ("Error at " ++ show idPos ++ ". Incompatible types: you can't assign a value of type " ++ show (getTypeFromExpression expr) ++ " to " ++ idVal ++ " because it has type " ++ show envType) :errors, StmtAssign (BaseExpr (Identifier (TokIdent (idPos, idVal))) envType) expr )

    Nothing -> return (env,
                ("Error at " ++ show idPos ++
                ". Unknown identifier: " ++ idVal ++
                " is used but has never been declared."):errors,
                StmtAssign (BaseExpr (Identifier (TokIdent (idPos, idVal))) (TypeBaseType BaseType_error)) expr)


parseExprExprAssignment :: EXPR Type -> EXPR Type -> Env -> Errors -> StateCount (Env, Errors, Stmt Env Type)
parseExprExprAssignment expr1 expr2 env errs = 
    -- TODO: includere posizione e stringa dell'espressione sinistra nel messaggio di errore
    if getTypeFromExpression expr1 == getTypeFromExpression expr2
        then return (env, errs, (StmtAssign expr1 expr2) )
        else case (getTypeFromExpression expr1, getTypeFromExpression expr2) of
            (TypeBaseType BaseType_real, TypeBaseType BaseType_integer) -> return (env, errs, (StmtAssign expr1 (IntToReal expr2)))
            otherwise -> return (env, ("Error. Incompatible types in assignment: you can't assign a value of type " ++ show (getTypeFromExpression expr2) ++ " to value of type " ++ show (getTypeFromExpression expr1) ++ "."):errs, (StmtAssign expr1 expr2) )


-- needed in rangeFromExpr to distinguish between leftmost and rightmost position
data TypeOfPos = PosLeft | PosRight

-- returns a tuple with the starting position and the ending position of the expression in input
-- TODO: refactor this function to make it less verbose!
-- TODO: test this function with many different expressions (including function calls, array elements, etc. )
-- TODO: call this function from all error messages involing expressions
rangeFromExpr :: EXPR infType -> (Position, Position)
rangeFromExpr expr = (getLeftmostPos expr, getRightmostPos expr) where

    getLeftmostPos :: EXPR infType -> Position
    getLeftmostPos (UnaryExpression _ exp _) = getLeftmostPos exp
    getLeftmostPos (BinaryExpression _ exp1 _ _) = getLeftmostPos exp1
    getLeftmostPos (ExprLiteral l) = getPosFromLiteral l PosLeft
    getLeftmostPos (ExprCall call _) = getPosFromCall call
    getLeftmostPos (BaseExpr (Identifier (TokIdent (pos, _))) _) = pos
    getLeftmostPos (BaseExpr (ArrayElem _ expr) _) = getLeftmostPos expr
    getLeftmostPos (IntToReal expr) = getLeftmostPos expr
    getLeftmostPos (RealToInt expr) = getLeftmostPos expr

    -- get position of the rightmost token with the length of the token added to the second component (the column)
    getRightmostPos :: EXPR infType -> Position
    getRightmostPos (UnaryExpression _ exp _) = getRightmostPos exp
    getRightmostPos (BinaryExpression _ _ exp2 _) = getRightmostPos exp2
    getRightmostPos (ExprLiteral l) = getPosFromLiteral l PosRight
    getRightmostPos (ExprCall call _) = getPosFromCall call
    getRightmostPos (BaseExpr (Identifier (TokIdent (pos, _))) _) = pos
    getRightmostPos (BaseExpr (ArrayElem _ expr) _) = getRightmostPos expr
    getRightmostPos (IntToReal expr) = getRightmostPos expr
    getRightmostPos (RealToInt expr) = getRightmostPos expr

    getPosFromLiteral :: Literal -> TypeOfPos -> Position
    getPosFromLiteral (LiteralInteger (TokInteger (pos, _))) PosLeft = pos
    getPosFromLiteral (LiteralInteger (TokInteger ((x, y), str))) PosRight = (x, y + length str)
    getPosFromLiteral (LiteralDouble (TokDouble (pos, _))) PosLeft = pos
    getPosFromLiteral (LiteralDouble (TokDouble ((x, y), str))) PosRight = (x, y + length str)
    getPosFromLiteral (LiteralChar (TokChar (pos, _))) PosLeft = pos
    getPosFromLiteral (LiteralChar (TokChar ((x, y), str))) PosRight = (x, y + length str)
    getPosFromLiteral (LiteralString (TokString (pos, _))) PosLeft = pos
    getPosFromLiteral (LiteralString (TokString ((x, y), str))) PosRight = (x, y + length str)
    getPosFromLiteral (LiteralBoolean (TokBoolean (pos, _))) PosLeft = pos
    getPosFromLiteral (LiteralBoolean (TokBoolean ((x, y), str))) PosRight = (x, y + length str)
    
    -- TODO: implement. returns position of last argument
    getPosFromCall :: Call infType -> Position
    getPosFromCall _ = (-1,-1)



-- Given current environment, errors and syntax tree, returns annotated tree and updated environment and errors
parseExpression :: Env -> Errors -> EXPR infType -> StateCount (Env, Errors, EXPR Type)

--TODO: ottenere informazione sulla posizione per stamparla nel messaggio di errore per tutti i casi
--TODO: evitare messaggi di errore indotti (conseguenza di assegnazioni del tipo error)

-- Boolean Unary Negation
parseExpression env errs (UnaryExpression Not exp t) = do
    (env2, errs2, parsedexp) <- parseExpression env errs exp
    if getTypeFromExpression parsedexp == TypeBaseType BaseType_boolean
        then
            return (env2, errs2, (UnaryExpression Not parsedexp (TypeBaseType BaseType_boolean) ))
        else
            return (env2, ("Error"++". Boolean negation 'not' applied to type " ++ show (getTypeFromExpression parsedexp) ++ " instead of boolean type."):errs2, (UnaryExpression Not parsedexp (TypeBaseType BaseType_error) ) )
        

-- Arithmetic Unary Negation
parseExpression env errs (UnaryExpression Negation exp t) = do
    (env2, errs2, parsedexp) <- parseExpression env errs exp
    if getTypeFromExpression parsedexp == TypeBaseType BaseType_integer || getTypeFromExpression parsedexp == TypeBaseType BaseType_real
        then
            return (env2, errs2, (UnaryExpression Negation parsedexp (getTypeFromExpression parsedexp) ))
        else
            return (env2, ("Error"++". Arithmetic unary minus '-' applied to type " ++ show (getTypeFromExpression parsedexp) ++ " instead of numeric type."):errs2, (UnaryExpression Negation parsedexp (TypeBaseType BaseType_error) ) )
        

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
parseExpression env errs (UnaryExpression Dereference exp t) = do 
    (env2, errs2, parsedexp) <- parseExpression env errs exp
    return (env2, errs2, (UnaryExpression Dereference parsedexp (TypeCompType (Pointer (getTypeFromExpression parsedexp))) ))
--    case getTypeFromExpression parsedexp of
--        TypeBaseType basetype -> return (env2, errs2, (UnaryExpression Dereference parsedexp (TypeCompType (Pointer basetype)) ))
--        _ -> return (env2, ("Error. Dereference operation on type "++ show (getTypeFromExpression parsedexp) ++" is not allowed because it is not a base type."):errs2, (UnaryExpression Dereference parsedexp (TypeBaseType BaseType_error) ))
        

-- Reference
parseExpression env errs (UnaryExpression Reference exp t) = do
    (env2, errs2, parsedexp) <- parseExpression env errs exp
    case getTypeFromExpression parsedexp of
        TypeCompType (Pointer t) -> return (env2, errs2, (UnaryExpression Reference parsedexp t) )
        _ -> return (env2, ("Error. Invalid reference '@' operation on type" ++ show (getTypeFromExpression parsedexp) ++ "."):errs2, (UnaryExpression Reference parsedexp (TypeBaseType BaseType_error) ))
    

-- Literals (base case of recursions)
parseExpression env errs (ExprLiteral literal) = return (env, errs, (ExprLiteral literal) )

-- Function calls
parseExpression env errs (ExprCall call t) = parseExpressionCall env errs call

-- Base Expressions: identifies or array elements
parseExpression env errs (BaseExpr bexpr t) = do
    (env2, err2, parsedbexpr, t) <- parseBaseExpression env errs bexpr
    return (env2, err2, (BaseExpr parsedbexpr t))
        

-- Type casted expressions: verify that types match
-- Integer to Real
parseExpression env errs (IntToReal expr) = do
    (env2, errs2, parsedexpr) <- parseExpression env errs expr
    case getTypeFromExpression parsedexpr of
        TypeBaseType BaseType_integer -> return (env2, errs2, (IntToReal parsedexpr) )
        TypeBaseType BaseType_real -> return (env2, ("Warning: removed unneeded implicit type casting"):errs2, parsedexpr) -- expression is real already: remove type casting wrapper
        otherwise -> return (env2, ("Error: type casting from Integer to Real applied to type " ++ show (getTypeFromExpression parsedexpr) ++ "."):errs2, (IntToReal parsedexpr))


-- parseExpression env errs expr = return (env, errs, (ExprLiteral (LiteralInteger (TokInteger ((0,0), "10")))) ) -- temporaneamente ogni espressione non specificata diventa il numero 10 (ora ridondante)

parseStatementCall :: Env -> Errors -> Call infType -> StateCount (Env, Errors, Stmt Env Type)
parseStatementCall env errs (CallArgs (TokIdent (tokpos,tokid)) args ) = do
    (env2, err2, parsedcall, t) <- parseCall env errs (CallArgs (TokIdent (tokpos,tokid)) args )
    return (env2, err2, (StmtCall parsedcall) )

parseExpressionCall :: Env -> Errors -> Call infType -> StateCount (Env, Errors, EXPR Type)
parseExpressionCall env errs (CallArgs (TokIdent (tokpos,tokid)) args ) = do
    (env2, err2, parsedcall, t) <- parseCall env errs (CallArgs (TokIdent (tokpos,tokid)) args )
    return (env2, err2, (ExprCall parsedcall t) )

parseCall :: Env -> Errors -> Call infType -> StateCount (Env, Errors, Call Type, Type)
parseCall env errs (CallArgs (TokIdent (tokpos,tokid)) args ) = do 
    (env2, err2, parsedargs) <- parseArguments env errs args []
    case Env.lookup tokid env of
        Just (Function pos parameters t _) -> parseFunction env errs (CallArgs (TokIdent (tokpos,tokid)) parsedargs ) parameters t []
        Just (Procedure pos parameters _) -> parseProcedure env errs (CallArgs (TokIdent (tokpos,tokid)) parsedargs ) parameters []
        Just (DefaultProc t) -> return (env, errs, (CallArgs (TokIdent (tokpos,tokid)) parsedargs ), t  ) --TODO: refactoring procedure default nell'environment
        Just (Constant pos t addr) -> return (env, ("Error at " ++ show tokpos ++". Identifier " ++ tokid ++" is used as a function/procedure but it is a constant."):errs,
                    (CallArgs (TokIdent (tokpos,tokid)) parsedargs ), (TypeBaseType BaseType_error) )
        Just (VarType mod pos t addr) -> return (env, ("Error at " ++ show tokpos ++". Identifier " ++ tokid ++" is used as a function/procedure but it is a variable."):errs,
                    (CallArgs (TokIdent (tokpos,tokid)) parsedargs ), (TypeBaseType BaseType_error) ) 
        Nothing -> return (env, ("Error at " ++ show tokpos ++". Unknown identifier: " ++ tokid ++" is used but has never been declared."):errs,
                    (CallArgs (TokIdent (tokpos,tokid)) parsedargs ), (TypeBaseType BaseType_error) ) 
        

parseArguments :: Env -> Errors -> [EXPR infType] -> [EXPR Type] -> StateCount (Env, Errors, [EXPR Type])
parseArguments env errs [] res = return (env, errs, res)
parseArguments env errs (arg:args) res = do
    (env2, err2, parsedexpr) <- parseExpression env errs arg
    parseArguments env2 err2 args (res++[parsedexpr])
        

-- Last parameter is list of parsed expressions (call arguments) that have been type casted if needed
-- Parameters: env, errors, call, params, parsedargs --TODO: dire nel messaggio di errore di mismatch quanti parametri sono previsti?
parseFunction :: Env -> Errors -> Call Type -> Prms -> Type -> [EXPR Type] -> StateCount (Env, Errors, Call Type, Type)
parseFunction env errs (CallArgs (TokIdent (tokpos,tokid)) [] ) NoParams t pargs = return (env, errs, (CallArgs (TokIdent (tokpos,tokid)) pargs ), t )
parseFunction env errs (CallArgs (TokIdent (tokpos,tokid)) [] ) (Params prms) t pargs = return (env, ("Error at " ++ show tokpos ++ " in function "++tokid++": mismatch in number of arguments"):errs, (CallArgs (TokIdent (tokpos,tokid)) pargs ), (TypeBaseType BaseType_error) )
parseFunction env errs (CallArgs (TokIdent (tokpos,tokid)) args ) NoParams t pargs = return (env, ("Error at " ++ show tokpos ++ " in function "++tokid++": mismatch in number of arguments"):errs, (CallArgs (TokIdent (tokpos,tokid)) (pargs++args) ), (TypeBaseType BaseType_error) )
parseFunction env errs (CallArgs (TokIdent (tokpos,tokid)) args ) (Params (prm:prms) ) t pargs = do
    (env2, err2, args2, compatible, pargs2) <- compareArguments env errs tokpos args prm True pargs
    newparams <- case prms of
                    [] -> return NoParams
                    _ -> return (Params prms)
    if compatible
        then
            parseFunction env2 err2 (CallArgs (TokIdent (tokpos,tokid)) args2 ) newparams t pargs2
        else
            parseFunction env2 err2 (CallArgs (TokIdent (tokpos,tokid)) args2 ) newparams (TypeBaseType BaseType_error) pargs2
        
parseProcedure :: Env -> Errors -> Call Type -> Prms -> [EXPR Type] -> StateCount (Env, Errors, Call Type, Type)
parseProcedure env errs (CallArgs (TokIdent (tokpos,tokid)) [] ) NoParams pargs = return (env, errs, (CallArgs (TokIdent (tokpos,tokid)) pargs ), (TypeBaseType BaseType_void) )
parseProcedure env errs (CallArgs (TokIdent (tokpos,tokid)) [] ) (Params prms) pargs = return (env, ("Error at " ++ show tokpos ++ " in procedure "++tokid++": mismatch in number of arguments"):errs, (CallArgs (TokIdent (tokpos,tokid)) pargs ), (TypeBaseType BaseType_error) )
parseProcedure env errs (CallArgs (TokIdent (tokpos,tokid)) args ) NoParams pargs = return (env, ("Error at " ++ show tokpos ++ " in procedure "++tokid++": mismatch in number of arguments"):errs, (CallArgs (TokIdent (tokpos,tokid)) (pargs++args) ), (TypeBaseType BaseType_error) )
parseProcedure env errs (CallArgs (TokIdent (tokpos,tokid)) args ) (Params (prm:prms) ) pargs = do
    (env2, err2, args2, compatible, pargs2) <- compareArguments env errs tokpos args prm True pargs
    newparams <- case prms of
                [] -> return NoParams
                _ -> return $ Params prms
    if compatible
        then
            parseProcedure env2 err2 (CallArgs (TokIdent (tokpos,tokid)) args2 ) newparams pargs2
        else
            parseProcedure env2 (("Error at " ++ show tokpos ++ ": arguments not compatible"):err2) (CallArgs (TokIdent (tokpos,tokid)) args ) newparams pargs2

-- Parses arguments of the same type defined together and print accurate error messages with position of arguments
-- Boolean parameter keeps track of whether all arguments are of correct type, Last list of expressions are function call arguments type casted if needed
compareArguments :: Env -> Errors -> Position -> [EXPR Type] -> Prm -> Bool -> [EXPR Type] -> StateCount (Env, Errors, [EXPR Type], Bool, [EXPR Type])
compareArguments env errs p [] (Param _ [] t) comp pargs = return (env, errs, [], comp, pargs)
compareArguments env errs p args (Param _ [] t) comp pargs = return (env, errs, args, comp, pargs)
compareArguments env errs p [] (Param _ toks t) comp pargs = return (env, errs, [], False, pargs)
compareArguments env errs p (expr:args) (Param m ((IdElement (TokIdent (_,parid))):toks) t) comp pargs = do
    (env2, err2, parsedexpr) <- parseExpression env errs expr
    -- confronto tra parametri, diversi casi: 1) int to real, 2) real to int, 3) incompatibile --TODO: type casting CharToString e StringToChar?
    if (getTypeFromExpression parsedexpr) == t
        then
            compareArguments env2 err2 p args (Param m toks t) comp (pargs++[parsedexpr])
        else
            case ((getTypeFromExpression parsedexpr),t) of
                (TypeBaseType BaseType_integer, TypeBaseType BaseType_real) -> compareArguments env2 err2 p args (Param m toks t) comp (pargs++[(IntToReal parsedexpr)])
                otherwise -> compareArguments env2 (("Error at "++ show p ++": parameter "++ parid ++" is assigned type " ++ show (getTypeFromExpression parsedexpr) ++ " but it should be of type " ++ show t):err2) p args (Param m toks t) False (pargs++[parsedexpr])
            
        
parseBinaryBooleanExpression :: Env -> Errors -> BinaryOperator -> EXPR infType -> EXPR infType -> StateCount (Env, Errors, EXPR Type)
parseBinaryBooleanExpression env errs op exp1 exp2 = do
    (env2, errs2, parsedexp1) <- parseExpression env errs exp1
    (env3, errs3, parsedexp2) <- parseExpression env2 errs2 exp2
    if getTypeFromExpression parsedexp1 /= TypeBaseType BaseType_boolean 
        then return (env3, ("Error. " ++ "First argument of "++ getStringFromOperator op ++" operator is of type " ++ show (getTypeFromExpression parsedexp1) ++ " instead of boolean."):errs3, (BinaryExpression op parsedexp1 parsedexp2 (TypeBaseType BaseType_error) ))
        else if getTypeFromExpression parsedexp2 /= TypeBaseType BaseType_boolean
            then return (env3, ("Error. " ++ "Second argument of "++ getStringFromOperator op ++" operator is of type " ++ show (getTypeFromExpression parsedexp2) ++ " instead of boolean."):errs3, (BinaryExpression op parsedexp1 parsedexp2 (TypeBaseType BaseType_error) ))
            else return (env3, errs3, (BinaryExpression op parsedexp1 parsedexp2 (TypeBaseType BaseType_boolean) ))
    where
        -- string form of operator for printing error messages
        getStringFromOperator :: BinaryOperator -> [Char]
        getStringFromOperator And = "'And'"
        getStringFromOperator Or = "'Or'"
        getStringFromOperator _ = ""

parseBinaryArithmeticExpression :: Env -> Errors -> BinaryOperator -> EXPR infType -> EXPR infType -> StateCount (Env, Errors, EXPR Type)
parseBinaryArithmeticExpression env errs op exp1 exp2 = do
    (env2, errs2, parsedexp1) <- parseExpression env errs exp1
    (env3, errs3, parsedexp2) <- parseExpression env2 errs2 exp2
    -- 3 cases: 1) first element not numeric, 2) second argument not numeric, 3) no errors
    if getTypeFromExpression parsedexp1 /= TypeBaseType BaseType_integer && getTypeFromExpression parsedexp1 /= TypeBaseType BaseType_real 
        then return (env3, ("Error. " ++ "First argument of "++ getStringFromOperator op ++ "operator is of type " ++ show (getTypeFromExpression parsedexp1) ++ " instead of numeric (integer or real)."):errs3, (BinaryExpression op parsedexp1 parsedexp2 (TypeBaseType BaseType_error) ))
        else if getTypeFromExpression parsedexp2 /= TypeBaseType BaseType_integer && getTypeFromExpression parsedexp2 /= TypeBaseType BaseType_real 
            then return (env3, ("Error. " ++ "Second argument of "++getStringFromOperator op++" operator is of type " ++ show (getTypeFromExpression parsedexp2) ++ " instead of numeric (integer or real)."):errs3, (BinaryExpression op parsedexp1 parsedexp2 (TypeBaseType BaseType_error) ))
            -- 3 casi: 1) primo int secondo real, 2) primo real secondo int, 3) no type casting
            else case (getTypeFromExpression parsedexp1, getTypeFromExpression parsedexp2) of
                    (TypeBaseType BaseType_integer, TypeBaseType BaseType_real) -> return (env3, errs3, (BinaryExpression op (IntToReal parsedexp1) parsedexp2 (getType op parsedexp1 parsedexp2) ))
                    (TypeBaseType BaseType_real, TypeBaseType BaseType_integer) -> return (env3, errs3, (BinaryExpression op parsedexp1 (IntToReal parsedexp2) (getType op parsedexp1 parsedexp2) ))
                    otherwise -> return (env3, errs3, (BinaryExpression op parsedexp1 parsedexp2 (getType op parsedexp1 parsedexp2) ))                   
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

parseBinaryRelationExpression :: Env -> Errors -> BinaryOperator -> EXPR infType -> EXPR infType -> StateCount (Env, Errors, EXPR Type)
parseBinaryRelationExpression env errs op exp1 exp2 = do
    (env2, errs2, parsedexp1) <- parseExpression env errs exp1
    (env3, errs3, parsedexp2) <- parseExpression env2 errs2 exp2
    -- 3 cases: 1) first element not numeric, 2) second argument not numeric, 3) no errors
    if getTypeFromExpression parsedexp1 /= TypeBaseType BaseType_integer && getTypeFromExpression parsedexp1 /= TypeBaseType BaseType_real 
        then return (env3, ("Error. " ++ "First argument of "++ getStringFromOperator op ++ "operator is of type " ++ show (getTypeFromExpression parsedexp1) ++ " instead of numeric (integer or real)."):errs3, (BinaryExpression op parsedexp1 parsedexp2 (TypeBaseType BaseType_error) ))
        else if getTypeFromExpression parsedexp2 /= TypeBaseType BaseType_integer && getTypeFromExpression parsedexp2 /= TypeBaseType BaseType_real 
            then return (env3, ("Error. " ++ "Second argument of "++getStringFromOperator op++" operator is of type " ++ show (getTypeFromExpression parsedexp2) ++ " instead of numeric (integer or real)."):errs3, (BinaryExpression op parsedexp1 parsedexp2 (TypeBaseType BaseType_error) ))
            else return (env3, errs3, (BinaryExpression op parsedexp1 parsedexp2 (TypeBaseType BaseType_boolean) )) -- result is of boolean type
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
parseBaseExpression:: Env -> Errors -> BEXPR infType -> StateCount (Env, Errors, BEXPR Type, Type)

parseBaseExpression env errs (Identifier (TokIdent (tokpos,tokid)) ) = 
    case Env.lookup tokid env of
        Just (VarType mod _ envType addr) -> return (env, errs, (Identifier (TokIdent (tokpos,tokid)) ), envType )
        Just (Constant _ envType addr) -> return (env, errs, (Identifier (TokIdent (tokpos,tokid)) ), envType )
        Nothing -> return (env, ("Error at " ++ show tokpos ++". Unknown identifier: " ++ tokid ++" is used but has never been declared."):errs, (Identifier (TokIdent (tokpos,tokid))), TypeBaseType BaseType_error)

parseBaseExpression env errs (ArrayElem bexpr iexpr) = do
    (env2, err2, parsediexpr) <- parseExpression env errs iexpr -- parsing of index for type checking (and casting if needed)
    --TODO: con parsediexpr sarebbe necessario verifiicare se l'indice rientra entro i limiti dell'array

    (env3, err3, parsedbexpr) <- parseExpression env2 err2 bexpr -- parsing of base expression to get its type: if it is an array type, return type of element of that array; otherwise it is an error
    
    case (getTypeFromExpression parsedbexpr, getTypeFromExpression parsediexpr) of -- TODO: refactoring possibile di questa parte di codice? --TODO: posizione nei messaggi di errore
        -- 4 cases: 1) array and integer; 2) array and error; 3) error and integer; 4) error and error (distinction necessary to generate appropriate error messages)
        (TypeCompType (Array i1 i2 basetype), TypeBaseType BaseType_integer) -> return (env3, err3, (ArrayElem parsedbexpr parsediexpr), basetype)
        (TypeCompType (Array i1 i2 basetype), _) -> return (env3, ("Error. Array index is not of numeric type but it is of type "++show (getTypeFromExpression parsediexpr)++"."):err3, (ArrayElem parsedbexpr parsediexpr), TypeBaseType BaseType_error)
        (_, TypeBaseType BaseType_integer) -> return (env3, ("Error. Expression " ++ show parsedbexpr ++ " is treated as an array but it is of type " ++ show (getTypeFromExpression parsedbexpr) ++ "."):err3, (ArrayElem parsedbexpr parsediexpr), TypeBaseType BaseType_error)
        otherwise -> return (env3, ("Error. Expression " ++ show parsedbexpr ++ " is treated as an array but it is of type" ++ show (getTypeFromExpression parsedbexpr) ++ "."):("Error. Array index is not of numeric type but it is of type "++show (getTypeFromExpression parsediexpr)++"."):err3, (ArrayElem parsedbexpr parsediexpr), TypeBaseType BaseType_error)


-- Returns annotated type for expressions
getTypeFromExpression :: EXPR Type -> Type
getTypeFromExpression (UnaryExpression op exp t) = t
getTypeFromExpression (BinaryExpression op exp1 exp2 t) = t
getTypeFromExpression (ExprLiteral literal) = (TypeBaseType (getTypeFromLiteral literal) )
getTypeFromExpression (ExprCall call t) = t
getTypeFromExpression (BaseExpr bexp t) = t
getTypeFromExpression (IntToReal _) = TypeBaseType BaseType_real

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



