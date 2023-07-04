module TypeChecker where
import AbsGrammar
import Env

type Errors = [String]
emptyErrors :: [String]
emptyErrors = []

-- Type Checking starting point
parseTree :: P env -> (Env, Errors)
parseTree (Prog _ dclBlock beBlock) = parseBEBlock beBlock (parseDclBlock dclBlock defaultEnv, emptyErrors)

parseTree2 :: P env -> Env -> Errors -> (Env, Errors, P Env)
parseTree2 (Prog pBlock dclBlock beBlock) env errors = (newEnv, newErrors, Prog pBlock dBlks beBlks)
    where
        (env1, errors1, dBlks) = parseDclBlocks2 env errors dclBlock
        -- errors and env are propagated from declaration block into beginEnd Block!
        -- notice that env1 is the env after parsing declaration blocks
        (newEnv, newErrors, beBlks) = parseBEBlock2 env1 errors1 beBlock 

parseDclBlocks2:: Env -> Errors -> [DclBlock env] -> (Env, Errors, [DclBlock Env])
parseDclBlocks2 env errors (x:xs) = (finalEnv, finalErrors, newBlock : newBlocks)
    where
        (env1, errors1, newBlock) = parseDclBlock2 env errors x
        (finalEnv, finalErrors, newBlocks) = parseDclBlocks2 env1 errors1 xs
parseDclBlocks2 env errors [] = (env, errors, [])

parseDclBlock2 :: Env -> Errors -> DclBlock env -> (Env, Errors, DclBlock Env)
parseDclBlock2 env errors blk = case blk of
    DclBlockVrBlock (VarBlock [VarDefinition vars varType]) -> 
        (newEnv, errors, DclBlockVrBlock (VarBlock [VarDefinition vars varType]))
    -- TODO: make sure errores are updated after parsing declaration blocks
        where newEnv = populateEnv (extractInfo vars) varType env
    --TODO: gestire gli altri 3 casi di DclBlock
    -- con i costruttori parametrizzati non è più possibile semplicemente passare il blocco ricevuto in input,
    -- ma bisogna crearne uno nuovo (anche se è uguale [vedi il caso sopra])
    -- soluzione temporanea finchè non gestiamo gli altri 3 casi
    _ -> (env, errors, DclBlockVrBlock (VarBlock [VarDefinition [IdElement (TokIdent ((1,1),"tmp"))] (TypeBaseType BaseType_integer)]))


parseBEBlock2:: Env -> Errors -> BEBlock env -> (Env, Errors, BEBlock Env)
parseBEBlock2 env errors (BegEndBlock statements annEnv) = (newEnv, newErrors, BegEndBlock newStatements newEnv)
    where
        (newEnv, newErrors, newStatements) = parseStatements env errors statements
        
        parseStatements:: Env -> Errors -> [Stmt env] ->  (Env, Errors, [Stmt Env])
        parseStatements env errors [] = (env, errors, [])
        parseStatements env errors (s:xs) = case s of
            StmtAssign (BaseExpr (Identifier id)) (ExprLiteral literal) ->
                parseStatements env1 errors1 xs 
                where (env1, errors1, assignStmt) = parseAssignment id literal env errors
            -- TODO: stessa cosa del caso in parseDclBlock2
            --_ -> (env, errors, [s]) -- TODO: parse other type of statemets here

        -- check if literal type matches with the one saved in the environment. 
        -- If it doesn't return current environment and a new error message
        parseAssignment:: TokIdent -> Literal -> Env -> Errors -> (Env, Errors, Stmt env)
        parseAssignment (TokIdent (idPos, idVal)) literal env errors = case Env.lookup idVal env of
            Just (VarType envPos envType) ->
                -- TODO: now if types are different an error is thrown, but casting should be performed for compatibile types!
                if envType == TypeBaseType (getTypeFromLiteral literal)
                    then (
                        env, 
                        errors, 
                        -- NOTICE HOW WE ANNOTATE THE TREE, saving info about type of expr!
                        StmtAssign (AnnotatedExpr (BaseExpr (Identifier (TokIdent (idPos, idVal)))) envType) (ExprLiteral literal))
                    else (env,
                        ("Error at " ++ show idPos ++
                        ". Incompatible types: you can't assign a value of type " ++
                        show (getTypeFromLiteral literal) ++ " to " ++ idVal ++
                        " because it has type " ++ show envType) :errors, 
                        -- In case of errors the tree is not annotated. 
                        -- TODO: maybe we should annotate it with the type of the literal? or don't annotate it at all?
                        StmtAssign (BaseExpr (Identifier (TokIdent (idPos, idVal)))) (ExprLiteral literal))
            Nothing -> (env,
                        ("Error at " ++ show idPos ++
                        ". Unknown identifier: " ++ idVal ++
                        " is used but has never been declared."):errors,
                        StmtAssign (BaseExpr (Identifier (TokIdent (idPos, idVal)))) (ExprLiteral literal))
            where
                getTypeFromLiteral:: Literal -> BaseType
                getTypeFromLiteral (LiteralInteger _) = BaseType_integer
                getTypeFromLiteral (LiteralString _) = BaseType_string
                getTypeFromLiteral (LiteralBoolean _) = BaseType_boolean
                getTypeFromLiteral (LiteralDouble _) = BaseType_real
                getTypeFromLiteral (LiteralChar _) = BaseType_char


-- Navigates syntax tree and saves info about variables type (declared in a Declaration block) in the global environment
-- Output (for now): the Env with info about variable types
parseDclBlock:: [DclBlock env] -> Env -> Env
parseDclBlock (x:xs) env =  case x of
    DclBlockVrBlock (VarBlock [VarDefinition vars varType]) -> populateEnv (extractInfo vars) varType env
    _ -> env

-- parse the begin-end block and check the statements for type errors
parseBEBlock:: BEBlock env -> (Env, Errors) -> (Env, Errors)
parseBEBlock (BegEndBlock statements annEnv) (env, errors) = parseStatements statements (env, errors)
    where
        parseStatements:: [Stmt env] -> (Env, Errors) -> (Env, Errors)
        parseStatements [] (env, errors) = (env, [])
        parseStatements (s:statements) (env, errors) = case s of
            StmtAssign (BaseExpr (Identifier id)) (ExprLiteral literal) ->
                parseStatements statements (parseAssignment id literal (env, errors))
            _ -> (env, errors) -- TODO: parse other type of statemets here

        -- check if literal type matches with the one saved in the environment. 
        -- If it doesn't return current environment and a new error message
        parseAssignment:: TokIdent -> Literal -> (Env, Errors) -> (Env, Errors)
        parseAssignment (TokIdent (idPos, idVal)) literal (env, errors) = case Env.lookup idVal env of
            Just (VarType envPos envType) ->
                -- TODO: now if types are different an error is thrown, but casting should be performed for compatibile types!
                if envType == TypeBaseType (getTypeFromLiteral literal)
                    then (env, errors)
                    else (env,
                        ("Error at " ++ show idPos ++
                        ". Incompatible types: you can't assign a value of type " ++
                        show (getTypeFromLiteral literal) ++ " to " ++ idVal ++
                        " because it has type " ++ show envType) :errors)
            Nothing -> (env,
                        ("Error at " ++ show idPos ++
                        ". Unknown identifier: " ++ idVal ++
                        " is used but has never been declared."):errors)
            where
                getTypeFromLiteral:: Literal -> BaseType
                getTypeFromLiteral (LiteralInteger _) = BaseType_integer
                getTypeFromLiteral (LiteralString _) = BaseType_string
                getTypeFromLiteral (LiteralBoolean _) = BaseType_boolean
                getTypeFromLiteral (LiteralDouble _) = BaseType_real
                getTypeFromLiteral (LiteralChar _) = BaseType_char
