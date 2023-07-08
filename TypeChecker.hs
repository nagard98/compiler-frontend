module TypeChecker where
import AbsGrammar
import Env
import Text.Parsec (parserReturn)
import Data.Text.Array (new)

type Errors = [String]
emptyErrors :: [String]
emptyErrors = []

-- Type Checking starting point
parseTree :: P env infType -> Env -> Errors -> (Env, Errors, P Env Type)
-- TODO: anche qui valutare se restituire newEnv o env1; Probabilmente dovremmo aggiungere anche un
-- campo env a Prog che indichi l'environment globale
parseTree (Prog pBlock dclBlock beBlock _) env errs = (newEnv, newErrors, Prog pBlock dBlks beBlks globEnv)
    where
        (globEnv, errors1, dBlks) = parseDclBlocks env errs dclBlock
        -- errors and env are propagated from declaration block into beginEnd Block!
        -- notice that globEnv is the env after parsing declaration blocks
        (newEnv, newErrors, beBlks) = parseBEBlock globEnv errors1 beBlock

-- Navigates syntax tree and saves info about variables type (declared in a Declaration block) in the global environment
parseDclBlocks:: Env -> Errors -> [DclBlock env infType] -> (Env, Errors, [DclBlock Env Type])
parseDclBlocks env errors (x:xs) = (finalEnv, finalErrors, newBlock : newBlocks)
    where
        (env1, errors1, newBlock) = parseSingleDclBlock env errors x
        (finalEnv, finalErrors, newBlocks) = parseDclBlocks env1 errors1 xs
parseDclBlocks env errors [] = (env, errors, [])

 -- TODO: make sure errores are updated after parsing declaration blocks
 -- e.g. redefining a variable could produce a warning and redefinig a constant an error
parseSingleDclBlock :: Env -> Errors -> DclBlock env infType  -> (Env, Errors, DclBlock Env Type)
parseSingleDclBlock env errors blk = case blk of

    -- add info about variables to the environment
    DclBlockVrBlock (VarBlock vrDefs) -> (newEnv, errors, DclBlockVrBlock (VarBlock vrDefs))
        where
            -- savese info about variables type in env 
            parseVrDefs :: [VrDef] -> Env -> Env
            parseVrDefs [] env = env
            parseVrDefs (def:vrDefs) env = case def of
                VarDefinition idElements t -> parseVrDefs vrDefs newEnv
                    where
                        -- TODO : probabilmente necessaria gestione errori
                        (newEnv, newErrs, _) = parseIds idElements t env []

            newEnv = parseVrDefs vrDefs env

    -- add info about constants to the environment
    DclBlockCsBlock (ConstBlock csDefs) -> (newEnv, errors, DclBlockCsBlock (ConstBlock csDefs))
        where
            -- savese info about constants type in env 
            parseConsDefs :: [CsDef] -> Env -> Env
            parseConsDefs [] env = env
            parseConsDefs (c:cs) env = case c of
                ConstDefinition (IdElement (TokIdent (pos, id))) literal ->
                    parseConsDefs cs (Env.insert id (Constant pos (TypeBaseType (getTypeFromLiteral literal))) env)

            newEnv = parseConsDefs csDefs env

    -- add info about functions to the environment (position, name, parameters, return type) and parse function body
    DclBlockFcBlock fB@(FuncBlock idTok@(TokIdent (pos, id)) params retType beb) -> 
        (finalEnv, finalErrors, DclBlockFcBlock (FuncBlock idTok params retType annotatedBEB))
        where
            -- add to env return type (needed for type checking of the return statement) and function info
            tmpEnv = Env.mergeEnvs env (Env.fromList [(id, Function pos params retType), ("return", Return retType)])

            (tmpEnv2, tmpErrors2, annotatedParams) = parseParams params [] tmpEnv errors

            (finalEnv, finalErrors, annotatedBEB) = parseBEBlock tmpEnv2 tmpErrors2 beb

    -- add info about procedures to the environment. Same as functions but without return type
    DclBlockPcBlock pB@(ProcBlock idTok@(TokIdent (pos, id)) params beb) -> (tmpEnv, errors, DclBlockPcBlock (ProcBlock idTok params annBEB))
        where
            tmpEnv = Env.insert id (Procedure pos params) env

            (pEnv, pErrs, pPrms) = parseParams params [] tmpEnv errors
            (fEnv, fErrs, annBEB) = parseBEBlock pEnv errors beb


parseParams :: Prms -> [Prm] -> Env -> Errors -> (Env, Errors, Prms)
parseParams prms accPrms env errs = case prms of

    (Params ( p@(Param mod idList typ):ps )) -> parseParams (Params ps) (p:accPrms) tmpEnv tmpErrs
        where
            (tmpEnv, tmpErrs, _) = parseIds idList typ env errs

    (Params []) -> (env, errs, Params accPrms)

    NoParams -> (env, errs, prms)


parseIds :: [IdElem] -> Type -> Env -> Errors -> (Env, Errors, [IdElem])
parseIds [] typ env errs = (env, errs, [])
parseIds ( idElem@(IdElement (TokIdent (pos, id))):ids) typ env errs = (newEnv, errs, idElem:newIds)
    where
        (newEnv, newErrs, newIds) = parseIds ids typ (Env.insert id (VarType pos typ) env) errs

-- parse the begin-end block and check the statements for type errors
parseBEBlock:: Env -> Errors -> BEBlock env infType -> (Env, Errors, BEBlock Env Type)
parseBEBlock env errors (BegEndBlock statements annEnv) = (newEnv, newErrors, BegEndBlock newStatements newEnv)
    where
        (newEnv, newErrors, newStatements) = parseStatements env errors statements

parseStatements:: Env -> Errors -> [Stmt env infType] ->  (Env, Errors, [Stmt Env Type])
parseStatements env errors [] = (env, errors, [])
parseStatements env errors allStmts =  q env errors allStmts []
        where
            q::Env -> Errors -> [Stmt env infType] -> [Stmt Env Type] ->  (Env, Errors, [Stmt Env Type])
            q env errors [] annStmts = (env, errors, annStmts)
            q env errors (s:xs) annStmts = q env1 errors1 xs (annStmts++[annStmt])
                where
                    (env1, errors1, annStmt) = parseStatement s env errors


-- Esempi provvisori di statement per cui non è ancora stato definito il parsing
exStmtCall = StmtCall (CallArgs (TokIdent ((0,0),"funzioneDiEsempio")) [])
exStmtSelect = StmtSelect (StmtIf (ExprLiteral (LiteralInteger (TokInteger ((36,30), "10")))) (StmtReturn (Ret (ExprLiteral (LiteralInteger (TokInteger ((36,30), "10")) ) ) )) )
exStmtIter = StmtIter (StmtWhileDo (ExprLiteral (LiteralInteger (TokInteger ((36,30), "10")))) (StmtReturn (Ret (ExprLiteral (LiteralInteger (TokInteger ((36,30), "10")) ) ) )) )
exStmtReturn = StmtReturn (Ret (ExprLiteral (LiteralInteger (TokInteger ((36,30), "10")) ) ) )


parseStatement :: Stmt stmtenv infType -> Env -> Errors -> (Env, Errors, Stmt Env Type)
parseStatement stmt env errs = case stmt of
            -- tipologie di statement: dichiarazione, blocco, assegnamento, chiamata funzione, if-else, iterazione, return
            -- Dichiarazione
            (StmtDecl dclblock) -> (env2, err2, (StmtDecl block))
                where
                    (env2, err2, block) = parseSingleDclBlock env errs dclblock
            -- Blocco
            (StmtComp beblock) -> (env2, err2, (StmtComp block))
                where
                    (env2, err2, block) = parseBEBlock env errs beblock

            -- Assegnamento
            (StmtAssign expr1 expr2) -> parseAssignment (StmtAssign expr1 expr2) env errs

            --TODO: fare vero parsing senza utilizzare nodi generici per il resto dei casi

            -- Chiamata funzione
            (StmtCall call) -> (env, errs, exStmtCall )
            -- Select
            (StmtSelect sel) -> (env, errs, exStmtSelect )
            -- Iterazione
            (StmtIter iter) -> (env, errs, exStmtIter )
            -- Return
            -- (StmtReturn return)  -> parseReturn (StmtReturn return) env errs
            (StmtReturn return)  ->  (env, "THIS IS A NEW ERROR":errs, exStmtReturn )

-- parseReturn :: Stmt env infType -> Env -> Errors -> (Env, Errors, Stmt Env Type)
-- parseReturn (StmtReturn (Ret expr)) env errs = 
--     case Env.lookup "return" env of 
--     Just (Return expectedType) ->
--         if sup expectedType (getTypeFromExpression parsedExpr) /= expectedType
--             then ( newEnv,
--                     "returned type not compatibile with return type of function": newErrs, 
--                      StmtReturn (Ret parsedExpr))
--             else
--                 -- everything is ok, return the parsed expression 
--                 (newEnv, newErrs, StmtReturn (Ret parsedExpr))
--     Nothing -> 
--         -- Theoretically this should never happen, 
--         -- since the return type of the fucntion is saved in the environment when the function is parsed
--         (newEnv,
--         "Internal Type checking error: return type of function x in pos (x, y) was not saved in the environment": newErrs,
--         StmtReturn (Ret parsedExpr))

--     where (newEnv, newErrs, parsedExpr) = parseExpression env errs expr



parseAssignment :: Stmt env infType -> Env -> Errors -> (Env, Errors, Stmt Env Type)
parseAssignment ass env errs = case ass of
            -- Assegno a variabile un letterale
            StmtAssign (BaseExpr (Identifier tId) tp) (ExprLiteral literal) -> parseLitAssignment tId literal env errs
            -- Assegno a variabile valore espressione generica: 1) parsing dell'espressione e trovo il tipo; 2) controllo compatibilità con letterale in assegnamento
            StmtAssign (BaseExpr (Identifier tId) tp) expr -> parseExprAssignment tId parsedexpr env2 errs2
                where
                    (env2, errs2, parsedexpr) = parseExpression env errs expr

            -- TODO: stessa cosa del caso in parseSingleDclBlock, rimuovere caso generico finale
            _ -> ( env, errs, StmtAssign (BaseExpr (Identifier (TokIdent ((0,0),"TODO"))) (TypeBaseType BaseType_real)) (ExprLiteral (LiteralDouble (TokDouble ((0,0),"111.111")))) )


-- check if literal type matches with the one saved in the environment. 
-- If it doesn't return current environment and a new error message
parseLitAssignment:: TokIdent -> Literal -> Env -> Errors -> (Env, Errors, Stmt Env Type)
parseLitAssignment (TokIdent (idPos, idVal)) literal env errors = case Env.lookup idVal env of
    Just (VarType envPos envType) ->
        if envType == TypeBaseType (getTypeFromLiteral literal)
            then (
                env,
                errors,
                -- NOTICE HOW WE ANNOTATE THE TREE, saving info about type of expr!
                StmtAssign (BaseExpr (Identifier (TokIdent (idPos, idVal))) envType) (ExprLiteral literal)
                )
            else case (envType, getTypeFromLiteral literal ) of
                -- 3 cases: casting int->real, real->int or incompatible types
                (TypeBaseType BaseType_real, BaseType_integer) -> (env, errors, StmtAssign (BaseExpr (Identifier (TokIdent (idPos, idVal))) envType) (ExprLiteral (IntToReal literal) ))
                (TypeBaseType BaseType_integer, BaseType_real) -> (env, errors, StmtAssign (BaseExpr (Identifier (TokIdent (idPos, idVal))) envType) (ExprLiteral (RealToInt literal) ))
                -- In case of errors the tree is not annotated. 
                -- TODO: maybe we should annotate it with the type of the literal? or don't annotate it at all?
                -- Per il momento ho aggiunto un come tipo envType
                (_, _)  -> (env, ("Error at " ++ show idPos ++ ". Incompatible types: you can't assign a value of type " ++ show (getTypeFromLiteral literal) ++ " to " ++ idVal ++ " because it has type " ++ show envType) :errors, StmtAssign (BaseExpr (Identifier (TokIdent (idPos, idVal))) envType) (ExprLiteral literal) )
    Nothing -> (env,
                ("Error at " ++ show idPos ++
                ". Unknown identifier: " ++ idVal ++
                " is used but has never been declared."):errors,
                StmtAssign (BaseExpr (Identifier (TokIdent (idPos, idVal))) (TypeBaseType BaseType_error)) (ExprLiteral literal))

-- Given identifier and expression (already parsed with inferred type!) assigns type to token of identifier
parseExprAssignment :: TokIdent -> EXPR Type -> Env -> Errors -> (Env, Errors, Stmt Env Type)
parseExprAssignment (TokIdent (idPos, idVal)) expr env errors = case Env.lookup idVal env of
    Just (VarType envPos envType) ->
        if envType == getTypeFromExpression expr
            then (
                env,
                errors,
                StmtAssign (BaseExpr (Identifier (TokIdent (idPos, idVal))) envType) expr -- annoto literal con il tipo corretto
                )
            else case (envType, getTypeFromExpression expr ) of
                -- 3 cases: 1) casting int->real, 2) casting real->int, 3) incompatible types
                (TypeBaseType BaseType_real, TypeBaseType BaseType_integer) -> (env, errors, StmtAssign (BaseExpr (Identifier (TokIdent (idPos, idVal))) envType) expr )
                (TypeBaseType BaseType_integer, TypeBaseType BaseType_real) -> (env, errors, StmtAssign (BaseExpr (Identifier (TokIdent (idPos, idVal))) envType) expr )
                (_, _)  -> (env, ("Error at " ++ show idPos ++ ". Incompatible types: you can't assign a value of type " ++ show (getTypeFromExpression expr) ++ " to " ++ idVal ++ " because it has type " ++ show envType) :errors, StmtAssign (BaseExpr (Identifier (TokIdent (idPos, idVal))) envType) expr )

    Nothing -> (env,
                ("Error at " ++ show idPos ++
                ". Unknown identifier: " ++ idVal ++
                " is used but has never been declared."):errors,
                StmtAssign (BaseExpr (Identifier (TokIdent (idPos, idVal))) (TypeBaseType BaseType_error)) expr)


-- Given current environment, errors and syntax tree, returns annotated tree and updated environment and errors
parseExpression :: Env -> Errors -> EXPR infType -> (Env, Errors, EXPR Type)

--TODO: ottenere informazione sulla posizione per stamparla nel messaggio di errore per tutti i casi
--TODO: evitare messaggi di errore indotti (conseguenza di assegnazioni del tipo error)

-- Boolean Unary Negation
parseExpression env errs (UnaryExpression Not exp t) =
    if getTypeFromExpression parsedexp == TypeBaseType BaseType_boolean
        then
            (env2, errs2, (UnaryExpression Not parsedexp (TypeBaseType BaseType_boolean) ))
        else
            (env2, ("Error"++". Boolean negation 'not' applied to type " ++ show (getTypeFromExpression parsedexp) ++ " instead of boolean type."):errs2, (UnaryExpression Not parsedexp (TypeBaseType BaseType_error) ) )
    where
        (env2, errs2, parsedexp) = parseExpression env errs exp

-- Arithmetic Unary Negation
parseExpression env errs (UnaryExpression Negation exp t) =
    if getTypeFromExpression parsedexp == TypeBaseType BaseType_integer || getTypeFromExpression parsedexp == TypeBaseType BaseType_real
        then
            (env2, errs2, (UnaryExpression Negation parsedexp (getTypeFromExpression parsedexp) ))
        else
            (env2, ("Error"++". Arithmetic unary minus '-' applied to type " ++ show (getTypeFromExpression parsedexp) ++ " instead of numeric type."):errs2, (UnaryExpression Negation parsedexp (TypeBaseType BaseType_error) ) )
    where
        (env2, errs2, parsedexp) = parseExpression env errs exp

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
parseExpression env errs (UnaryExpression Dereference exp t) = case getTypeFromExpression parsedexp of
    TypeBaseType basetype -> (env2, errs2, (UnaryExpression Dereference parsedexp (TypeCompType (Pointer basetype)) ))
    otherwise -> (env2, ("Error. Dereference operation on type "++ show (getTypeFromExpression parsedexp) ++" is not allowed because it is not a base type."):errs2, (UnaryExpression Dereference parsedexp (TypeBaseType BaseType_error) ))
    where
        (env2, errs2, parsedexp) = parseExpression env errs exp

-- Reference
parseExpression env errs (UnaryExpression Reference exp t) = case getTypeFromExpression parsedexp of
    TypeCompType (Pointer innertype) -> (env2, errs2, (UnaryExpression Reference parsedexp (TypeBaseType innertype)) )
    otherwise -> (env2, ("Error. Invalid reference '@' operation on type" ++ show (getTypeFromExpression parsedexp) ++ "."):errs2, (UnaryExpression Reference parsedexp (TypeBaseType BaseType_error) ))
    where
        (env2, errs2, parsedexp) = parseExpression env errs exp

-- Literals (base case of recursions)
parseExpression env errs (ExprLiteral literal) = (env, errs, (ExprLiteral literal) )

-- Function calls
parseExpression env errs (ExprCall call t) = parseFunctionCall env errs call

-- Base Expressions: identifies or array elements
parseExpression env errs (BaseExpr bexpr t) = parseBaseExpression env errs bexpr

-- parseExpression env errs expr = (env, errs, (ExprLiteral (LiteralInteger (TokInteger ((0,0), "10")))) ) -- temporaneamente ogni espressione non specificata diventa il numero 10 (ora ridondante)


parseFunctionCall :: Env -> Errors -> Call infType -> (Env, Errors, EXPR Type)
parseFunctionCall env errs (CallArgs (TokIdent (tokpos,tokid)) args ) = case Env.lookup tokid env of
    Just (Function pos parameters t) -> parseFunction env errs (CallArgs (TokIdent (tokpos,tokid)) args ) parameters t
    Just (Procedure pos parameters) -> parseProcedure env errs (CallArgs (TokIdent (tokpos,tokid)) args ) parameters
    Just (DefaultProc t) -> (env, errs, (ExprCall (CallArgs (TokIdent (tokpos,tokid)) parsedargs ) t ) ) --TODO: refactoring procedure default nell'environment
    Just (Constant pos t) -> (env, ("Error at " ++ show tokpos ++". Identifier " ++ tokid ++" is used as a function/procedure but it is a constant."):errs,
                (ExprCall (CallArgs (TokIdent (tokpos,tokid)) parsedargs ) (TypeBaseType BaseType_error) ) )
    Just (VarType pos t) -> (env, ("Error at " ++ show tokpos ++". Identifier " ++ tokid ++" is used as a function/procedure but it is a variable."):errs,
                (ExprCall (CallArgs (TokIdent (tokpos,tokid)) parsedargs ) (TypeBaseType BaseType_error) ) ) 
    Nothing -> (env, ("Error at " ++ show tokpos ++". Unknown identifier: " ++ tokid ++" is used but has never been declared."):errs,
                (ExprCall (CallArgs (TokIdent (tokpos,tokid)) parsedargs ) (TypeBaseType BaseType_error) ) ) 
    where
        (env2, err2, parsedargs) = parseArguments env errs args []

parseArguments :: Env -> Errors -> [EXPR infType] -> [EXPR Type] -> (Env, Errors, [EXPR Type])
parseArguments env errs [] res = (env, errs, res)
parseArguments env errs (arg:args) res = parseArguments env2 err2 args (res++[parsedexpr])
    where
        (env2, err2, parsedexpr) = parseExpression env errs arg

parseFunction :: Env -> Errors -> Call infType -> Prms -> Type -> (Env, Errors, EXPR Type)
parseFunction env errs (CallArgs (TokIdent (tokpos,tokid)) [] ) NoParams t = (env, errs, (ExprCall (CallArgs (TokIdent (tokpos,tokid)) [] ) t ) )
parseFunction env errs (CallArgs (TokIdent (tokpos,tokid)) [] ) (Params prms) t = (env, ("Error at " ++ show tokpos ++ ": mismatch in number of arguments"):errs, (ExprCall (CallArgs (TokIdent (tokpos,tokid)) [] ) (TypeBaseType BaseType_error) ) )
parseFunction env errs (CallArgs (TokIdent (tokpos,tokid)) args ) NoParams t = (env, ("Error at " ++ show tokpos ++ ": mismatch in number of arguments"):errs, (ExprCall (CallArgs (TokIdent (tokpos,tokid)) [] ) (TypeBaseType BaseType_error) ) )
parseFunction env errs (CallArgs (TokIdent (tokpos,tokid)) args ) (Params (prm:prms) ) t = 
    if compatible
        then
            parseFunction env2 err2 (CallArgs (TokIdent (tokpos,tokid)) args2 ) newparams t
        else
            parseFunction env2 err2 (CallArgs (TokIdent (tokpos,tokid)) args2 ) newparams (TypeBaseType BaseType_error)
    where
        (env2, err2, args2, compatible) = compareArguments env errs tokpos args prm True
        newparams = case prms of
                        [] -> NoParams
                        _ -> Params prms

parseProcedure :: Env -> Errors -> Call infType -> Prms -> (Env, Errors, EXPR Type)
parseProcedure env errs (CallArgs (TokIdent (tokpos,tokid)) [] ) NoParams = (env, errs, (ExprCall (CallArgs (TokIdent (tokpos,tokid)) [] ) (TypeBaseType BaseType_void) ) )
parseProcedure env errs (CallArgs (TokIdent (tokpos,tokid)) [] ) (Params prms) = (env, ("Error at " ++ show tokpos ++ ": mismatch in number of arguments"):errs, (ExprCall (CallArgs (TokIdent (tokpos,tokid)) [] ) (TypeBaseType BaseType_error) ) )
parseProcedure env errs (CallArgs (TokIdent (tokpos,tokid)) args ) NoParams = (env, ("Error at " ++ show tokpos ++ ": mismatch in number of arguments"):errs, (ExprCall (CallArgs (TokIdent (tokpos,tokid)) [] ) (TypeBaseType BaseType_error) ) )
parseProcedure env errs (CallArgs (TokIdent (tokpos,tokid)) args ) (Params (prm:prms) ) = 
    if compatible
        then
            parseProcedure env2 err2 (CallArgs (TokIdent (tokpos,tokid)) args2 ) newparams
        else
            parseProcedure env2 err2 (CallArgs (TokIdent (tokpos,tokid)) args2 ) newparams
    where
        (env2, err2, args2, compatible) = compareArguments env errs tokpos args prm True
        newparams = case prms of
                        [] -> NoParams
                        _ -> Params prms

-- Parses arguments of the same type defined together and print accurate error messages with position of arguments
-- Boolean parameter keeps track of whether all arguments are of correct type
compareArguments :: Env -> Errors -> Position -> [EXPR infType] -> Prm -> Bool -> (Env, Errors, [EXPR infType], Bool)
compareArguments env errs p [] (Param _ [] t) comp = (env, errs, [], comp)
compareArguments env errs p args (Param _ [] t) comp = (env, errs, args, comp)
compareArguments env errs p [] (Param _ toks t) comp = (env, errs, [], False)
compareArguments env errs p (expr:args) (Param m ((IdElement (TokIdent (_,parid))):toks) t) comp = 
    if (getTypeFromExpression parsedexpr) == t
    then
        compareArguments env2 err2 p args (Param m toks t) comp
    else
        compareArguments env2 (("Error at "++ show p ++": parameter "++ parid ++" is assigned type " ++ show (getTypeFromExpression parsedexpr) ++ " but it should be of type " ++ show t):err2) p args (Param m toks t) False
    where
        (env2, err2, parsedexpr) = parseExpression env errs expr

parseBinaryBooleanExpression :: Env -> Errors -> BinaryOperator -> EXPR infType -> EXPR infType -> (Env, Errors, EXPR Type)
parseBinaryBooleanExpression env errs op exp1 exp2
    | getTypeFromExpression parsedexp1 /= TypeBaseType BaseType_boolean = (env3, ("Error. " ++ "First argument of "++ getStringFromOperator op ++" operator is of type " ++ show (getTypeFromExpression parsedexp1) ++ " instead of boolean."):errs3, (BinaryExpression op parsedexp1 parsedexp2 (TypeBaseType BaseType_error) ))
    | getTypeFromExpression parsedexp2 /= TypeBaseType BaseType_boolean = (env3, ("Error. " ++ "Second argument of "++ getStringFromOperator op ++" operator is of type " ++ show (getTypeFromExpression parsedexp2) ++ " instead of boolean."):errs3, (BinaryExpression op parsedexp1 parsedexp2 (TypeBaseType BaseType_error) ))
    | otherwise = (env3, errs3, (BinaryExpression op parsedexp1 parsedexp2 (TypeBaseType BaseType_boolean) ))
    where
        (env2, errs2, parsedexp1) = parseExpression env errs exp1
        (env3, errs3, parsedexp2) = parseExpression env2 errs2 exp2
        getStringFromOperator :: BinaryOperator -> [Char]
        getStringFromOperator And = "'And'"
        getStringFromOperator Or = "'Or'"
        getStringFromOperator _ = ""

-- TODO: nodo di casting implicito nel caso di operazione tra numeri interi e reali? (es. 10*1.1)
parseBinaryArithmeticExpression :: Env -> Errors -> BinaryOperator -> EXPR infType -> EXPR infType -> (Env, Errors, EXPR Type)
parseBinaryArithmeticExpression env errs op exp1 exp2
    -- 3 cases: 1) first element not numeric, 2) second argument not numeric, 3) no errors
    | getTypeFromExpression parsedexp1 /= TypeBaseType BaseType_integer && getTypeFromExpression parsedexp1 /= TypeBaseType BaseType_real = (env3, ("Error. " ++ "First argument of "++ getStringFromOperator op ++ "operator is of type " ++ show (getTypeFromExpression parsedexp1) ++ " instead of numeric (integer or real)."):errs3, (BinaryExpression op parsedexp1 parsedexp2 (TypeBaseType BaseType_error) ))
    | getTypeFromExpression parsedexp2 /= TypeBaseType BaseType_integer && getTypeFromExpression parsedexp2 /= TypeBaseType BaseType_real = (env3, ("Error. " ++ "Second argument of "++getStringFromOperator op++" operator is of type " ++ show (getTypeFromExpression parsedexp2) ++ " instead of numeric (integer or real)."):errs3, (BinaryExpression op parsedexp1 parsedexp2 (TypeBaseType BaseType_error) ))
    | otherwise = (env3, errs3, (BinaryExpression op parsedexp1 parsedexp2 (getType op parsedexp1 parsedexp2) ))
    where
        (env2, errs2, parsedexp1) = parseExpression env errs exp1
        (env3, errs3, parsedexp2) = parseExpression env2 errs2 exp2
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

parseBinaryRelationExpression :: Env -> Errors -> BinaryOperator -> EXPR infType -> EXPR infType -> (Env, Errors, EXPR Type)
parseBinaryRelationExpression env errs op exp1 exp2
    -- 3 cases: 1) first element not numeric, 2) second argument not numeric, 3) no errors
    | getTypeFromExpression parsedexp1 /= TypeBaseType BaseType_integer && getTypeFromExpression parsedexp1 /= TypeBaseType BaseType_real = (env3, ("Error. " ++ "First argument of "++ getStringFromOperator op ++ "operator is of type " ++ show (getTypeFromExpression parsedexp1) ++ " instead of numeric (integer or real)."):errs3, (BinaryExpression op parsedexp1 parsedexp2 (TypeBaseType BaseType_error) ))
    | getTypeFromExpression parsedexp2 /= TypeBaseType BaseType_integer && getTypeFromExpression parsedexp2 /= TypeBaseType BaseType_real = (env3, ("Error. " ++ "Second argument of "++getStringFromOperator op++" operator is of type " ++ show (getTypeFromExpression parsedexp2) ++ " instead of numeric (integer or real)."):errs3, (BinaryExpression op parsedexp1 parsedexp2 (TypeBaseType BaseType_error) ))
    | otherwise = (env3, errs3, (BinaryExpression op parsedexp1 parsedexp2 (TypeBaseType BaseType_boolean) )) -- result is of boolean type
    where
        (env2, errs2, parsedexp1) = parseExpression env errs exp1
        (env3, errs3, parsedexp2) = parseExpression env2 errs2 exp2
        -- string for of operator for printing error messages 
        getStringFromOperator :: BinaryOperator -> [Char]
        getStringFromOperator Eq = "'='"
        getStringFromOperator NotEq = "'<>'"
        getStringFromOperator LessT = "'<'"
        getStringFromOperator EqLessT = "'<='"
        getStringFromOperator GreatT = "'>'"
        getStringFromOperator EqGreatT = "'>='"
        getStringFromOperator _ = ""


parseBaseExpression :: Env -> Errors -> BEXPR infType -> (Env, Errors, EXPR Type)
-- parse identifiers
parseBaseExpression env errs (Identifier (TokIdent (tokpos,tokid)) ) = case Env.lookup tokid env of
    Just (VarType _ envType) -> (env, errs, (BaseExpr (Identifier (TokIdent (tokpos,tokid)) ) envType ) )
    Just (Constant _ envType) -> (env, errs, (BaseExpr (Identifier (TokIdent (tokpos,tokid)) ) envType ) )
    Nothing -> (env, ("Error at " ++ show tokpos ++". Unknown identifier: " ++ tokid ++" is used but has never been declared."):errs,
                (BaseExpr (Identifier (TokIdent (tokpos,tokid)) ) (TypeBaseType BaseType_error) ) )
-- parse elements of an array
parseBaseExpression env errs (ArrayElem bexpr expr)
    | getTypeFromExpression parsedexpr /= TypeBaseType BaseType_integer = (env3, ("Error. Array index is of type "++show (getTypeFromExpression parsedexpr)++" instead of integer."):err3, (BaseExpr (ArrayElem parsedbexpr parsedexpr) (TypeBaseType BaseType_error)) )
    | otherwise = (env3, err3, (BaseExpr (ArrayElem parsedbexpr parsedexpr) t ) )
    where
        (env2, err2, parsedexpr) = parseExpression env errs expr
        (env3, err3, (BaseExpr parsedbexpr (TypeCompType (Array _ _ t)) ) ) = parseBaseExpression env2 err2 bexpr

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





