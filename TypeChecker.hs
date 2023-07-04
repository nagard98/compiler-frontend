module TypeChecker where
import AbsGrammar
import Env

type Errors = [String]
emptyErrors :: [String]
emptyErrors = []

-- Type Checking starting point
parseTree :: P env infType -> Env -> Errors -> (Env, Errors, P Env Type)
parseTree (Prog pBlock dclBlock beBlock) env errors = (newEnv, newErrors, Prog pBlock dBlks beBlks)
    where
        (env1, errors1, dBlks) = parseDclBlocks env errors dclBlock
        -- errors and env are propagated from declaration block into beginEnd Block!
        -- notice that env1 is the env after parsing declaration blocks
        (newEnv, newErrors, beBlks) = parseBEBlock env1 errors1 beBlock 

-- Navigates syntax tree and saves info about variables type (declared in a Declaration block) in the global environment
parseDclBlocks:: Env -> Errors -> [DclBlock env infType ] -> (Env, Errors, [DclBlock Env Type])
parseDclBlocks env errors (x:xs) = (finalEnv, finalErrors, newBlock : newBlocks)
    where
        (env1, errors1, newBlock) = parseSingleDclBlock env errors x
        (finalEnv, finalErrors, newBlocks) = parseDclBlocks env1 errors1 xs
parseDclBlocks env errors [] = (env, errors, [])

parseSingleDclBlock :: Env -> Errors -> DclBlock env infType  -> (Env, Errors, DclBlock Env Type)
parseSingleDclBlock env errors blk = case blk of
    DclBlockVrBlock (VarBlock [VarDefinition vars varType]) -> 
        (newEnv, errors, DclBlockVrBlock (VarBlock [VarDefinition vars varType]))
    -- TODO: make sure errores are updated after parsing declaration blocks
        where newEnv = populateEnv (extractInfo vars) varType env
    --TODO: gestire gli altri 3 casi di DclBlock
    -- con i costruttori parametrizzati non è più possibile semplicemente passare il blocco ricevuto in input,
    -- ma bisogna crearne uno nuovo (anche se è uguale [vedi il caso sopra])
    -- soluzione temporanea finchè non gestiamo gli altri 3 casi
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
        -- TODO: now if types are different an error is thrown, but casting should be performed for compatibile types!
        if envType == TypeBaseType (getTypeFromLiteral literal)
            then (
                env, 
                errors, 
                -- NOTICE HOW WE ANNOTATE THE TREE, saving info about type of expr!
                StmtAssign (BaseExpr (Identifier (TokIdent (idPos, idVal))) envType) (ExprLiteral literal)
                )
            else (env,
                ("Error at " ++ show idPos ++
                ". Incompatible types: you can't assign a value of type " ++
                show (getTypeFromLiteral literal) ++ " to " ++ idVal ++
                " because it has type " ++ show envType) :errors, 
                -- In case of errors the tree is not annotated. 
                -- TODO: maybe we should annotate it with the type of the literal? or don't annotate it at all?
                -- Per il momento ho aggiunto un come tipo envType
                StmtAssign (BaseExpr (Identifier (TokIdent (idPos, idVal))) envType) (ExprLiteral literal)
                )
    Nothing -> (env,
                ("Error at " ++ show idPos ++
                ". Unknown identifier: " ++ idVal ++
                " is used but has never been declared."):errors,
                --TODO : per il momento ho usato un tipo a caso, ma dovremmo aggiungere un nuovo tipo
                -- ad AbsGrammar che indichi un errore
                StmtAssign (BaseExpr (Identifier (TokIdent (idPos, idVal))) (TypeBaseType BaseType_boolean)) (ExprLiteral literal))
    where
        getTypeFromLiteral:: Literal -> BaseType
        getTypeFromLiteral (LiteralInteger _) = BaseType_integer
        getTypeFromLiteral (LiteralString _) = BaseType_string
        getTypeFromLiteral (LiteralBoolean _) = BaseType_boolean
        getTypeFromLiteral (LiteralDouble _) = BaseType_real
        getTypeFromLiteral (LiteralChar _) = BaseType_char
