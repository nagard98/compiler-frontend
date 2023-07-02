module TypeChecker where
import AbsGrammar
import qualified Data.Map as Map

-- This is the global environment.
-- first argument is the key type, second one the value type 
type Env = Map.Map String EnvData
emptyEnv:: Env
emptyEnv = Map.empty


type Errors = [String]
emptyErrors :: [String]
emptyErrors = []

-- Needed for modelling how data of the map entries in Env should be
-- E.g. variable types: the key is the name of the variable, data created using VarType constructor
-- TODO: create new constructors as needed
data EnvData = VarType Position Type |
               DefaultProc Type

-- make EnvData printable
instance Show EnvData where
    show (VarType p (TypeBaseType t)) = " at " ++ show p ++ " of type " ++ show t
    show (DefaultProc (TypeBaseType t)) = " default procedure of type " ++ show t
    show (VarType p t) = " at " ++ show p ++ " of type " ++ show t
    show (DefaultProc t) = " default procedure of type " ++ show t

-- Initial environment with default procedures
-- TODO: le procedure "write" quale tipo devono restituire?
defaultEnv = foldl1 (Map.union) [ Map.singleton "writeInt" (DefaultProc (TypeBaseType BaseType_integer) ), 
                                  Map.singleton "writeReal" (DefaultProc (TypeBaseType BaseType_integer) ), 
                                  Map.singleton "writeChar" (DefaultProc (TypeBaseType BaseType_integer) ), 
                                  Map.singleton "writeString" (DefaultProc (TypeBaseType BaseType_integer) ), 
                                  Map.singleton "readInt" (DefaultProc (TypeBaseType BaseType_integer) ), 
                                  Map.singleton "readReal" (DefaultProc (TypeBaseType BaseType_real) ), 
                                  Map.singleton "readChar" (DefaultProc (TypeBaseType BaseType_char) ), 
                                  Map.singleton "readString" (DefaultProc (TypeBaseType BaseType_string) )]

-- Type Checking starting point
parseTree :: P -> (Env, Errors)
parseTree (Prog _ dclBlock beBlock) = parseBEBlock beBlock (parseDclBlock dclBlock defaultEnv, emptyErrors)


-- Navigates syntax tree and saves info about variables type (declared in a Declaration block) in the global environment
-- Output (for now): the Env with info about variable types
parseDclBlock:: [DclBlock] -> Env -> Env
parseDclBlock (x:xs) env =  case x of
    DclBlockVrBlock (VarBlock [VarDefinition vars varType]) -> populateEnv (extractInfo vars) varType env
    _ -> env
    where
        extractInfo :: [IdElem] -> [(Position,String)]
        -- e.g. [IdElement (Ident "a"),IdElement (Ident "b")] -> ["a", "b"]
        extractInfo (x:xs) = case x of IdElement (TokIdent info) -> info:extractInfo xs
        extractInfo [] = []

        -- savese info about variables type in env 
        populateEnv :: [(Position,String)] -> Type -> Env -> Env
        populateEnv [] _ env = env
        populateEnv (v:varNames) t env = populateEnv varNames t (Map.insert (snd v) (VarType (fst v) t) env)

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
        parseAssignment (TokIdent (idPos, idVal)) literal (env, errors) = case Map.lookup idVal env of
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
