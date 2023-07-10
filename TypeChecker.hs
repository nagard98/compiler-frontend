module TypeChecker where
import AbsGrammar
import Env
import Text.Parsec (parserReturn)
import Data.Text.Array (new)
import HelperTAC
import Control.Monad.State.Lazy
import System.Process (CreateProcess(env))

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
                (tmpEnv, tmpErrs, _) <- parseIds idElements t env []
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
            (tmpEnv, tmpErrs, _) <- parseIds idList typ env errs
            (pEnv,pErrs, pPrms) <- parseParams (Params ps) (p:accPrms) tmpEnv tmpErrs
            return (pEnv, pErrs, pPrms)
                
        (Params []) -> return (env, errs, Params accPrms)

        NoParams -> return (env, errs, prms)


parseIds :: [IdElem] -> Type -> Env -> Errors -> StateCount (Env, Errors, [IdElem])
parseIds [] typ env errs = return (env, errs, [])
parseIds ( idElem@(IdElement (TokIdent (pos, id))):ids) typ env errs = do
    idAddr <- newIdAddr id
    tmpEnv <- Env.insert id (VarType pos typ idAddr) env
    (newEnv, newErrs, newIds) <- parseIds ids typ tmpEnv errs
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


-- Esempi provvisori di statement per cui non è ancora stato definito il parsing
exStmtCall = StmtCall (CallArgs (TokIdent ((0,0),"funzioneDiEsempio")) [])
exStmtSelect = StmtSelect (StmtIf (ExprLiteral (LiteralInteger (TokInteger ((36,30), "10")))) (StmtReturn (Ret (ExprLiteral (LiteralInteger (TokInteger ((36,30), "10")) ) ) )) )
exStmtIter = StmtIter (StmtWhileDo (ExprLiteral (LiteralInteger (TokInteger ((36,30), "10")))) (StmtReturn (Ret (ExprLiteral (LiteralInteger (TokInteger ((36,30), "10")) ) ) )) )


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

            --TODO: fare vero parsing senza utilizzare nodi generici per il resto dei casi

            -- Chiamata funzione
            (StmtCall call) -> return (env, errs, exStmtCall )
            -- Select
            (StmtSelect sel) -> return (env, errs, exStmtSelect )
            -- Iterazione
            (StmtIter iter) -> return (env, errs, exStmtIter )
            -- Return
            (StmtReturn return)  -> parseReturn (StmtReturn return) env errs
            -------------------------------------------------------------

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
            -- Il resto dei possibili l-value non è valido
            ( expr1, expr2 ) -> do
                (env2, err2, parsedexpr1) <- parseExpression env errs expr1
                (env3, err3, parsedexpr2) <- parseExpression env2 err2 expr2
                -- TODO: includere posizione e stringa della espressione sinistra nel messaggio di errore
                return (env3, ("Error: invalid l-value in assignment"):err3, (StmtAssign parsedexpr1 parsedexpr2) )


-- check if literal type matches with the one saved in the environment. 
-- If it doesn't return current environment and a new error message
parseLitAssignment:: TokIdent -> Literal -> Env -> Errors -> StateCount (Env, Errors, Stmt Env Type)
parseLitAssignment (TokIdent (idPos, idVal)) literal env errors = case Env.lookup idVal env of
    Just (VarType envPos envType addr) ->
        if envType == TypeBaseType (getTypeFromLiteral literal)
            then return (
                env,
                errors,
                -- NOTICE HOW WE ANNOTATE THE TREE, saving info about type of expr!
                StmtAssign (BaseExpr (Identifier (TokIdent (idPos, idVal))) envType) (ExprLiteral literal)
                )
            else case (envType, getTypeFromLiteral literal ) of
                -- 3 cases: casting int->real, real->int or incompatible types
                (TypeBaseType BaseType_real, BaseType_integer) -> return (env, errors, StmtAssign (BaseExpr (Identifier (TokIdent (idPos, idVal))) envType) (ExprLiteral (IntToReal literal) ))
                (TypeBaseType BaseType_integer, BaseType_real) -> return (env, errors, StmtAssign (BaseExpr (Identifier (TokIdent (idPos, idVal))) envType) (ExprLiteral (RealToInt literal) ))
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
    Just (VarType envPos envType addr) ->
        -- TODO: qui andrebbe applicato il sup e non verificata uguaglianza: risolvere problema wrapper IntToFloat e FloatToInt solo su literal
        if envType == getTypeFromExpression expr
            then return (
                env,
                errors,
                StmtAssign (BaseExpr (Identifier (TokIdent (idPos, idVal))) envType) expr -- annoto literal con il tipo corretto
                )
            else case (envType, getTypeFromExpression expr ) of
                -- 3 cases: 1) casting int->real, 2) casting real->int, 3) incompatible types
                (TypeBaseType BaseType_real, TypeBaseType BaseType_integer) -> return (env, errors, StmtAssign (BaseExpr (Identifier (TokIdent (idPos, idVal))) envType) expr )
                (TypeBaseType BaseType_integer, TypeBaseType BaseType_real) -> return (env, errors, StmtAssign (BaseExpr (Identifier (TokIdent (idPos, idVal))) envType) expr )
                (_, _)  -> return (env, ("Error at " ++ show idPos ++ ". Incompatible types: you can't assign a value of type " ++ show (getTypeFromExpression expr) ++ " to " ++ idVal ++ " because it has type " ++ show envType) :errors, StmtAssign (BaseExpr (Identifier (TokIdent (idPos, idVal))) envType) expr )

    Nothing -> return (env,
                ("Error at " ++ show idPos ++
                ". Unknown identifier: " ++ idVal ++
                " is used but has never been declared."):errors,
                StmtAssign (BaseExpr (Identifier (TokIdent (idPos, idVal))) (TypeBaseType BaseType_error)) expr)


parseExprExprAssignment :: EXPR Type -> EXPR Type -> Env -> Errors -> StateCount (Env, Errors, Stmt Env Type)
parseExprExprAssignment expr1 expr2 env errs = 
    -- TODO: qui andrebbe applicato il sup e non verificata uguaglianza: risolvere problema wrapper IntToFloat e FloatToInt solo su literal
    -- TODO: includere posizione e stringa della espressione sinistra nel messaggio di errore
    if getTypeFromExpression expr1 == getTypeFromExpression expr2
        then return (env, errs, (StmtAssign expr1 expr2) )
        else return (env, ("Error. Incompatible types in assignment: you can't assign a value of type " ++ show (getTypeFromExpression expr2) ++ " to value of type " ++ show (getTypeFromExpression expr1) ++ "."):errs, (StmtAssign expr1 expr2) )


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

-- Dereference --TODO: consentire puntatori ad array e ad altri puntatori?
parseExpression env errs (UnaryExpression Dereference exp t) = do 
    (env2, errs2, parsedexp) <- parseExpression env errs exp
    case getTypeFromExpression parsedexp of
        TypeBaseType basetype -> return (env2, errs2, (UnaryExpression Dereference parsedexp (TypeCompType (Pointer basetype)) ))
        _ -> return (env2, ("Error. Dereference operation on type "++ show (getTypeFromExpression parsedexp) ++" is not allowed because it is not a base type."):errs2, (UnaryExpression Dereference parsedexp (TypeBaseType BaseType_error) ))
        

-- Reference
parseExpression env errs (UnaryExpression Reference exp t) = do
    (env2, errs2, parsedexp) <- parseExpression env errs exp
    case getTypeFromExpression parsedexp of
        TypeCompType (Pointer innertype) -> return (env2, errs2, (UnaryExpression Reference parsedexp (TypeBaseType innertype)) )
        _ -> return (env2, ("Error. Invalid reference '@' operation on type" ++ show (getTypeFromExpression parsedexp) ++ "."):errs2, (UnaryExpression Reference parsedexp (TypeBaseType BaseType_error) ))
    

-- Literals (base case of recursions)
parseExpression env errs (ExprLiteral literal) = return (env, errs, (ExprLiteral literal) )

-- Function calls
parseExpression env errs (ExprCall call t) = parseFunctionCall env errs call

-- Base Expressions: identifies or array elements
parseExpression env errs (BaseExpr bexpr t) = parseBaseExpression env errs bexpr

-- parseExpression env errs expr = (env, errs, (ExprLiteral (LiteralInteger (TokInteger ((0,0), "10")))) ) -- temporaneamente ogni espressione non specificata diventa il numero 10 (ora ridondante)


parseFunctionCall :: Env -> Errors -> Call infType -> StateCount (Env, Errors, EXPR Type)
parseFunctionCall env errs (CallArgs (TokIdent (tokpos,tokid)) args ) = do 
    (env2, err2, parsedargs) <- parseArguments env errs args []
    case Env.lookup tokid env of
        Just (Function pos parameters t _) -> parseFunction env errs (CallArgs (TokIdent (tokpos,tokid)) args ) parameters t
        Just (Procedure pos parameters _) -> parseProcedure env errs (CallArgs (TokIdent (tokpos,tokid)) args ) parameters
        Just (DefaultProc t) -> return (env, errs, (ExprCall (CallArgs (TokIdent (tokpos,tokid)) parsedargs ) t ) ) --TODO: refactoring procedure default nell'environment
        Just (Constant pos t addr) -> return (env, ("Error at " ++ show tokpos ++". Identifier " ++ tokid ++" is used as a function/procedure but it is a constant."):errs,
                    (ExprCall (CallArgs (TokIdent (tokpos,tokid)) parsedargs ) (TypeBaseType BaseType_error) ) )
        Just (VarType pos t addr) -> return (env, ("Error at " ++ show tokpos ++". Identifier " ++ tokid ++" is used as a function/procedure but it is a variable."):errs,
                    (ExprCall (CallArgs (TokIdent (tokpos,tokid)) parsedargs ) (TypeBaseType BaseType_error) ) ) 
        Nothing -> return (env, ("Error at " ++ show tokpos ++". Unknown identifier: " ++ tokid ++" is used but has never been declared."):errs,
                    (ExprCall (CallArgs (TokIdent (tokpos,tokid)) parsedargs ) (TypeBaseType BaseType_error) ) ) 
        

parseArguments :: Env -> Errors -> [EXPR infType] -> [EXPR Type] -> StateCount (Env, Errors, [EXPR Type])
parseArguments env errs [] res = return (env, errs, res)
parseArguments env errs (arg:args) res = do
    (env2, err2, parsedexpr) <- parseExpression env errs arg
    parseArguments env2 err2 args (res++[parsedexpr])
        

parseFunction :: Env -> Errors -> Call infType -> Prms -> Type -> StateCount (Env, Errors, EXPR Type)
parseFunction env errs (CallArgs (TokIdent (tokpos,tokid)) [] ) NoParams t = return (env, errs, (ExprCall (CallArgs (TokIdent (tokpos,tokid)) [] ) t ) )
parseFunction env errs (CallArgs (TokIdent (tokpos,tokid)) [] ) (Params prms) t = return (env, ("Error at " ++ show tokpos ++ ": mismatch in number of arguments"):errs, (ExprCall (CallArgs (TokIdent (tokpos,tokid)) [] ) (TypeBaseType BaseType_error) ) )
parseFunction env errs (CallArgs (TokIdent (tokpos,tokid)) args ) NoParams t = return (env, ("Error at " ++ show tokpos ++ ": mismatch in number of arguments"):errs, (ExprCall (CallArgs (TokIdent (tokpos,tokid)) [] ) (TypeBaseType BaseType_error) ) )
parseFunction env errs (CallArgs (TokIdent (tokpos,tokid)) args ) (Params (prm:prms) ) t = do
    (env2, err2, args2, compatible) <- compareArguments env errs tokpos args prm True
    newparams <- case prms of
                    [] -> return NoParams
                    _ -> return (Params prms)
    if compatible
        then
            parseFunction env2 err2 (CallArgs (TokIdent (tokpos,tokid)) args2 ) newparams t
        else
            parseFunction env2 err2 (CallArgs (TokIdent (tokpos,tokid)) args2 ) newparams (TypeBaseType BaseType_error)
        

parseProcedure :: Env -> Errors -> Call infType -> Prms -> StateCount (Env, Errors, EXPR Type)
parseProcedure env errs (CallArgs (TokIdent (tokpos,tokid)) [] ) NoParams = return (env, errs, (ExprCall (CallArgs (TokIdent (tokpos,tokid)) [] ) (TypeBaseType BaseType_void) ) )
parseProcedure env errs (CallArgs (TokIdent (tokpos,tokid)) [] ) (Params prms) = return (env, ("Error at " ++ show tokpos ++ ": mismatch in number of arguments"):errs, (ExprCall (CallArgs (TokIdent (tokpos,tokid)) [] ) (TypeBaseType BaseType_error) ) )
parseProcedure env errs (CallArgs (TokIdent (tokpos,tokid)) args ) NoParams = return (env, ("Error at " ++ show tokpos ++ ": mismatch in number of arguments"):errs, (ExprCall (CallArgs (TokIdent (tokpos,tokid)) [] ) (TypeBaseType BaseType_error) ) )
parseProcedure env errs (CallArgs (TokIdent (tokpos,tokid)) args ) (Params (prm:prms) ) = do
    (env2, err2, args2, compatible) <- compareArguments env errs tokpos args prm True
    newparams <- case prms of
                [] -> return NoParams
                _ -> return $ Params prms
    if compatible
        then
            parseProcedure env2 err2 (CallArgs (TokIdent (tokpos,tokid)) args2 ) newparams
        else
            parseProcedure env2 (("Error at " ++ show tokpos ++ ": arguments not compatible"):err2) (CallArgs (TokIdent (tokpos,tokid)) args ) newparams

-- Parses arguments of the same type defined together and print accurate error messages with position of arguments
-- Boolean parameter keeps track of whether all arguments are of correct type
compareArguments :: Env -> Errors -> Position -> [EXPR infType] -> Prm -> Bool -> StateCount (Env, Errors, [EXPR infType], Bool)
compareArguments env errs p [] (Param _ [] t) comp = return (env, errs, [], comp)
compareArguments env errs p args (Param _ [] t) comp = return (env, errs, args, comp)
compareArguments env errs p [] (Param _ toks t) comp = return (env, errs, [], False)
compareArguments env errs p (expr:args) (Param m ((IdElement (TokIdent (_,parid))):toks) t) comp = do
    (env2, err2, parsedexpr) <- parseExpression env errs expr
    if (getTypeFromExpression parsedexpr) == t
        then
            compareArguments env2 err2 p args (Param m toks t) comp
        else
            compareArguments env2 (("Error at "++ show p ++": parameter "++ parid ++" is assigned type " ++ show (getTypeFromExpression parsedexpr) ++ " but it should be of type " ++ show t):err2) p args (Param m toks t) False
        

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
        getStringFromOperator :: BinaryOperator -> [Char]
        getStringFromOperator And = "'And'"
        getStringFromOperator Or = "'Or'"
        getStringFromOperator _ = ""

-- TODO: nodo di casting implicito nel caso di operazione tra numeri interi e reali? (es. 10*1.1)
parseBinaryArithmeticExpression :: Env -> Errors -> BinaryOperator -> EXPR infType -> EXPR infType -> StateCount (Env, Errors, EXPR Type)
parseBinaryArithmeticExpression env errs op exp1 exp2 = do
    (env2, errs2, parsedexp1) <- parseExpression env errs exp1
    (env3, errs3, parsedexp2) <- parseExpression env2 errs2 exp2
    -- 3 cases: 1) first element not numeric, 2) second argument not numeric, 3) no errors
    if getTypeFromExpression parsedexp1 /= TypeBaseType BaseType_integer && getTypeFromExpression parsedexp1 /= TypeBaseType BaseType_real 
        then return (env3, ("Error. " ++ "First argument of "++ getStringFromOperator op ++ "operator is of type " ++ show (getTypeFromExpression parsedexp1) ++ " instead of numeric (integer or real)."):errs3, (BinaryExpression op parsedexp1 parsedexp2 (TypeBaseType BaseType_error) ))
        else if getTypeFromExpression parsedexp2 /= TypeBaseType BaseType_integer && getTypeFromExpression parsedexp2 /= TypeBaseType BaseType_real 
            then return (env3, ("Error. " ++ "Second argument of "++getStringFromOperator op++" operator is of type " ++ show (getTypeFromExpression parsedexp2) ++ " instead of numeric (integer or real)."):errs3, (BinaryExpression op parsedexp1 parsedexp2 (TypeBaseType BaseType_error) ))
            else return (env3, errs3, (BinaryExpression op parsedexp1 parsedexp2 (getType op parsedexp1 parsedexp2) ))
    where
        -- string for of operator for printing error messages
        getStringFromOperator :: BinaryOperator -> [Char]
        getStringFromOperator Add = "'+'"
        getStringFromOperator Sub = "'-'"
        getStringFromOperator Mul = "'*'"
        getStringFromOperator Div = "'/'"
        getStringFromOperator Mod = "'mod'"
        getStringFromOperator _ = ""
        -- division has always result of real type
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

        -- string for of operator for printing error messages 
        getStringFromOperator :: BinaryOperator -> [Char]
        getStringFromOperator Eq = "'='"
        getStringFromOperator NotEq = "'<>'"
        getStringFromOperator LessT = "'<'"
        getStringFromOperator EqLessT = "'<='"
        getStringFromOperator GreatT = "'>'"
        getStringFromOperator EqGreatT = "'>='"
        getStringFromOperator _ = ""


parseBaseExpression :: Env -> Errors -> BEXPR infType -> StateCount (Env, Errors, EXPR Type)
-- parse identifiers
parseBaseExpression env errs (Identifier (TokIdent (tokpos,tokid)) ) = case Env.lookup tokid env of
    Just (VarType _ envType addr) -> return (env, errs, (BaseExpr (Identifier (TokIdent (tokpos,tokid)) ) envType ) )
    Just (Constant _ envType addr) -> return (env, errs, (BaseExpr (Identifier (TokIdent (tokpos,tokid)) ) envType ) )
    Nothing -> return (env, ("Error at " ++ show tokpos ++". Unknown identifier: " ++ tokid ++" is used but has never been declared."):errs,
                (BaseExpr (Identifier (TokIdent (tokpos,tokid)) ) (TypeBaseType BaseType_error) ) )

-- parse elements of an array
parseBaseExpression env errs (ArrayElem bexpr expr) = do
    (env2, err2, parsedexpr) <- parseExpression env errs expr
    --TODO: controllare se modifiche fatto con introduzione do abbiano creato problema
    -- (BaseExpr parsedbexpr (TypeCompType (Array _ _ t)) ) 
    (env3, err3, pbexpr) <- parseBaseExpression env2 err2 bexpr
    if getTypeFromExpression parsedexpr /= TypeBaseType BaseType_integer 
        then return (env3, ("Error. Array index is of type "++show (getTypeFromExpression parsedexpr)++" instead of integer."):err3, (BaseExpr (ArrayElem (getBaseExpr pbexpr) parsedexpr) (TypeBaseType BaseType_error)) )
        else return (env3, err3, (BaseExpr (ArrayElem (getBaseExpr pbexpr) parsedexpr) (getBaseExprType pbexpr) ) )     
        where
            getBaseExpr :: EXPR Type ->  BEXPR Type
            getBaseExpr (BaseExpr bexpr _) = bexpr
            getBaseExpr _ = error "TODO: errore in getBaseExpr in parseBaseExpression"
            getBaseExprType :: EXPR Type ->  Type
            getBaseExprType (BaseExpr _ (TypeCompType (Array _ _ t))) = t
            getBaseExprType _ = error "TODO: errore in getBaseExprType in parseBaseExpression"

-- Returns annotated type for expressions
getTypeFromExpression :: EXPR Type -> Type
getTypeFromExpression (UnaryExpression op exp t) = t
getTypeFromExpression (BinaryExpression op exp1 exp2 t) = t
getTypeFromExpression (ExprLiteral literal) = (TypeBaseType (getTypeFromLiteral literal) )
getTypeFromExpression (ExprCall call t) = t
getTypeFromExpression (BaseExpr exp t) = t

-- Type compatibility for operations
sup :: Type -> Type -> Type
sup t1 t2
    | t1 == t2 = t1
    | t1 == (TypeBaseType BaseType_integer) && t2 == (TypeBaseType BaseType_real) = (TypeBaseType BaseType_real)
    | t1 == (TypeBaseType BaseType_real) && t2 == (TypeBaseType BaseType_integer) = (TypeBaseType BaseType_real)
    | t1 == (TypeBaseType BaseType_char) && t2 == (TypeBaseType BaseType_string) = (TypeBaseType BaseType_string)
    | t1 == (TypeBaseType BaseType_string) && t2 == (TypeBaseType BaseType_char) = (TypeBaseType BaseType_string)
    | otherwise = (TypeBaseType BaseType_error)



