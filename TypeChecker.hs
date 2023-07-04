module TypeChecker where
import AbsGrammar
import Env

type Errors = [String]
emptyErrors :: [String]
emptyErrors = []

-- Type Checking starting point
parseTree :: P -> (Env, Errors)
parseTree (Prog _ dclBlock beBlock) = parseBEBlock beBlock (parseDclBlock dclBlock defaultEnv, emptyErrors)

parseTree2 :: P -> (Env, Errors, P)
parseTree2 (Prog pBlock dclBlock beBlock) = (newEnv, newErrors, Prog pBlock dBlks beBlks)
    where
        (env1, errors1, dBlks) = parseDclBlocks2 emptyEnv emptyErrors dclBlock
        -- errors and env are propagated from declaration block into beginEnd Block!
        -- notice that env1 is the env after parsing declaration blocks
        (newEnv, newErrors, beBlks) = parseBEBlock2 env1 errors1 beBlock 

parseDclBlocks2:: Env -> Errors -> [DclBlock] -> (Env, Errors, [DclBlock])
parseDclBlocks2 env errors (x:xs) = (env1, errors1, newBlock : newBlocks)
    where
        (env1, errors1, newBlock) = parseDclBlock2 env errors x
        (finalEnv, finalErrors, newBlocks) = parseDclBlocks2 env1 errors1 xs
parseDclBlocks2 env errors [] = (env, errors, [])

parseDclBlock2 :: Env -> Errors -> DclBlock -> (Env, Errors, DclBlock)
parseDclBlock2 env errors blk = case blk of
    DclBlockVrBlock (VarBlock [VarDefinition vars varType]) -> (newEnv, errors, blk)
    -- TODO: make sure errores are updated after parsing declaration blocks
        where newEnv = populateEnv (extractInfo vars) varType env
    _ -> (env, errors, blk)

parseBEBlock2:: Env -> Errors -> BEBlock -> (Env, Errors, BEBlock)
parseBEBlock2 env errors (BegEndBlock statements) = (newEnv, newErrors, BegEndBlock newStatements)
    where
        (newEnv, newErrors, newStatements) = parseStatements env errors statements
        
        parseStatements:: Env -> Errors -> [Stmt] ->  (Env, Errors, [Stmt])
        parseStatements env errors [] = (env, errors, [])
        parseStatements env errors (s:xs) = case s of
            StmtAssign (BaseExpr (Identifier id)) (ExprLiteral literal) ->
                parseStatements env1 errors1 xs 
                where (env1, errors1, assignStmt) = parseAssignment id literal env errors
            _ -> (env, errors, [s]) -- TODO: parse other type of statemets here

        -- check if literal type matches with the one saved in the environment. 
        -- If it doesn't return current environment and a new error message
        parseAssignment:: TokIdent -> Literal -> Env -> Errors -> (Env, Errors, Stmt)
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
parseDclBlock:: [DclBlock] -> Env -> Env
parseDclBlock (x:xs) env =  case x of
    DclBlockVrBlock (VarBlock [VarDefinition vars varType]) -> populateEnv (extractInfo vars) varType env
    _ -> env

-- parse the begin-end block and check the statements for type errors
parseBEBlock:: BEBlock -> (Env, Errors) -> (Env, Errors)
parseBEBlock (BegEndBlock statements) (env, errors) = parseStatements statements (env, errors)
    where
        parseStatements:: [Stmt] -> (Env, Errors) -> (Env, Errors)
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
