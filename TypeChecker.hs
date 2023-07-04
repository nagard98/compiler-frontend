module TypeChecker where
import AbsGrammar
import Env

type Errors = [String]
emptyErrors :: [String]
emptyErrors = []

-- Type Checking starting point
parseTree :: P env -> (Env, Errors)
parseTree (Prog _ dclBlock beBlock) = parseBEBlock beBlock (parseDclBlock dclBlock defaultEnv, emptyErrors)

parseTree2 :: P env -> Env -> P env
parseTree2 (Prog pBlock dclBlock beBlock) env = Prog pBlock dBlks beBlks
    where
        (newEnv, dBlks) = parseDclBlocks2 dclBlock env
        beBlks = parseBEBlock2 beBlock newEnv

parseDclBlocks2:: [DclBlock env] -> Env -> (Env, [DclBlock env])
parseDclBlocks2 l@(x:xs) env = (fst (parseDclBlocks2 xs newEnv), l)
    where
        newEnv = parseDclBlock2 x env

parseDclBlocks2 [] env = (env, [])

parseDclBlock2 :: DclBlock env -> Env -> Env
parseDclBlock2 blk env = case blk of
    DclBlockVrBlock (VarBlock [VarDefinition vars varType]) -> populateEnv (extractInfo vars) varType env
    _ -> env

parseBEBlock2:: BEBlock env -> Env -> BEBlock env
parseBEBlock2 block@(BegEndBlock statements annEnv) env = block --parseStatements statements (env, errors)
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


-- (BegEndBlock [
--     BegEndStmt1 (StmtAssign (BaseExpr (Identifier (Ident "a"))) (ExprLiteral (LiteralInteger 2))),
--     BegEndStmt1 (StmtAssign (BaseExpr (Identifier (Ident "b"))) (ExprLiteral (LiteralInteger 5))),
--     BegEndStmt1 (StmtAssign (BaseExpr (Identifier (Ident "c"))) (BinaryExpression {operator2 = Mul, exp1 = Expression (BaseExpr (Identifier (Ident "a"))), exp2 = Expression (BaseExpr (Identifier (Ident "b")))}))
--     ])
