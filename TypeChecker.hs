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
data EnvData = VarType Position Type

-- make EnvData printable
instance Show EnvData where
    show (VarType p t) = "at " ++ show p ++ " of type " ++ show t


parseTree :: P -> (Env, Errors)
parseTree (Prog _ dclBlock beBlock) = parseBEBlock beBlock (parseDclBlock dclBlock emptyEnv, emptyErrors)


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
        parseStatements:: [BegEndStmt] -> (Env, Errors) -> (Env, Errors)
        parseStatements [] (env, errors) = (env, [])
        parseStatements (s:statements) (env, errors) = case s of
            BegEndStmt1 (StmtAssign (BaseLExpr (Identifier (TokIdent (_,id) ))) (ExprLiteral literal)) -> 
                parseStatements statements (parseAssignment id literal (env, errors))
            _ -> (env, errors) -- TODO: parse other type of statemets here

        -- check if literal type matches with the one saved in the environment. 
        -- If it doesn't return current environment and a new error message
        parseAssignment:: String -> Literal -> (Env, Errors) -> (Env, Errors)
        parseAssignment id literal (env, errors) = case Map.lookup id env of
            Just (VarType p t) -> if t == TypeBaseType (getTypeFromLiteral literal) then (env, errors) else (env, "Type mismatch":errors)
            Nothing -> (env, "Variable not declared":errors)
            where
                getTypeFromLiteral:: Literal -> BaseType
                getTypeFromLiteral (LiteralInteger _) = BaseType_integer
                getTypeFromLiteral (LiteralString _) = BaseType_string
                getTypeFromLiteral (LiteralBoolean _) = BaseType_boolean
                getTypeFromLiteral (LiteralDouble _) = BaseType_real
                getTypeFromLiteral (LiteralChar _) = BaseType_char


-- (BegEndBlock [
--     BegEndStmt1 (StmtAssign (BaseLExpr (Identifier (Ident "a"))) (ExprLiteral (LiteralInteger 2))),
--     BegEndStmt1 (StmtAssign (BaseLExpr (Identifier (Ident "b"))) (ExprLiteral (LiteralInteger 5))),
--     BegEndStmt1 (StmtAssign (BaseLExpr (Identifier (Ident "c"))) (BinaryExpression {operator2 = Mul, exp1 = LExpression (BaseLExpr (Identifier (Ident "a"))), exp2 = LExpression (BaseLExpr (Identifier (Ident "b")))}))
--     ])
