module TypeChecker where
import AbsGrammar
import qualified Data.Map as Map

-- This is the global environment.
-- first argument is the key type, second one the value type 
type Env = Map.Map String EnvData

emptyEnv:: Env
emptyEnv = Map.empty


-- Needed for modelling how the data of the map entries in Env should be
-- E.g. variable types: the key is the name of the variable, data created using VarType constructor
-- TODO: create new constructors as needed
data EnvData = VarType Position Type


-- make EnvData printable
instance Show EnvData where
    show (VarType p t) = "at " ++ show p ++ " of type " ++ show t


parseTree :: P -> Env
-- parseTree (Prog _ dclBlock beBlock) = parseBEBlock beBlock (parseDclBlock dclBlock emptyEnv)
parseTree (Prog _ dclBlock beBlock) = parseDclBlock dclBlock emptyEnv


-- Navigates syntax tree and saves info about variables type (declared in a Declaration block) in the global environment
-- TODO: save correct info about position ((int, int) with rowIndex and colIndex) where variable was declared. can we get it from the parser?
parseDclBlock:: [DclBlock] -> Env -> Env
parseDclBlock (x:xs) env =  case x of
    DclBlockVrBlock (VarBlock [VarDefinition vars varType]) -> populateEnv (extractNames vars) varType env
    _ -> env
    where
        extractNames :: [IdElem] -> [String] 
        -- e.g. [IdElement (Ident "a"),IdElement (Ident "b")] -> ["a", "b"]
        extractNames (x:xs) = case x of IdElement (Ident id) -> id:extractNames xs
        extractNames [] = []
        
        -- savese info about variables type in env 
        populateEnv :: [String] -> Type -> Env -> Env
        populateEnv [] _ env = env
        populateEnv (v:varNames) t env = populateEnv varNames t (Map.insert v (VarType (-1, -1) t) env)


-- parseBEBlock:: BEBlock -> Env -> Env 
-- TODO: implement function. See point 2 and 3 in testfile3.txt