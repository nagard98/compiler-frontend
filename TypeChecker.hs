module TypeChecker where
import AbsGrammar
import Env

type Errors = [String]
emptyErrors :: [String]
emptyErrors = []

-- Type Checking starting point
parseTree :: P env infType -> (Env, Errors, P Env Type)
parseTree (Prog pBlock dclBlock beBlock)= (newEnv, newErrors, Prog pBlock dBlks beBlks)
    where
        (env1, errors1, dBlks) = parseDclBlocks emptyEnv emptyErrors dclBlock
        -- errors and env are propagated from declaration block into beginEnd Block!
        -- notice that env1 is the env after parsing declaration blocks
        (newEnv, newErrors, beBlks) = parseBEBlock env1 errors1 beBlock

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
    DclBlockVrBlock (VarBlock vrDefs) ->
        (newEnv, errors, DclBlockVrBlock (VarBlock vrDefs))
        where newEnv = populateEnvVars vrDefs env

    -- add info about constants to the environment
    DclBlockCsBlock (ConstBlock csDefs) -> (newEnv, errors, DclBlockCsBlock (ConstBlock csDefs))
        where newEnv = populateEnvConsts csDefs env

    -- add info about functions to the environment
    -- info are: function position, function name, parameters, return type
    -- TODO: pass global environment to begin-end block and parse inner statemets
    -- DclBlockFcBlock (FuncBlock idToken params retType beb) ->
    --     (newEnv, errors, DclBlockFcBlock (FuncBlock idToken params retType))
    --     where newEnv = populateEnvFuncs (FuncBlock idToken params retType beb) env
            
    --TODO: gestire gli altri 2 casi di DclBlock
    -- con i costruttori parametrizzati non è più possibile semplicemente passare il blocco ricevuto in input,
    -- ma bisogna crearne uno nuovo (anche se è uguale [vedi il caso sopra])
    -- soluzione temporanea finchè non gestiamo gli altri 2 casi (DclBlockPcBlock e DclBlockFcBlock)
    _ -> (env, errors, DclBlockVrBlock (VarBlock [VarDefinition [IdElement (TokIdent ((1,1),"tmp"))] (TypeBaseType BaseType_integer)]))

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
                    (env1, errors1, annStmt) = parseAssignment s env errors

parseAssignment :: Stmt env infType -> Env -> Errors -> (Env, Errors, Stmt Env Type)
parseAssignment ass env errs = case ass of
            StmtAssign (BaseExpr (Identifier tId) tp) (ExprLiteral literal) -> parseLitAssignment tId literal env errs
            -- TODO: stessa cosa del caso in parseSingleDclBlock
            _ -> ( env, errs, StmtAssign (BaseExpr (Identifier (TokIdent ((6,4),"a"))) (TypeBaseType BaseType_real)) (ExprLiteral (LiteralDouble (TokDouble ((6,8),"2.5555")))) )
            -- TODO: parse other type of statemets here


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