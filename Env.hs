module Env where

import qualified Data.Map as Map
import AbsGrammar

-- This is the global environment.
-- first argument is the key type, second one the value type 
type Env = Map.Map String EnvData
emptyEnv:: Env
emptyEnv = Map.empty

-- Needed for modelling how data of the map entries in Env should be
-- E.g. variable types: the key is the name of the variable, data created using VarType constructor
-- TODO: create new constructors as needed
data EnvData = VarType Position Type
                | DefaultProc Type
                | Function Position [Parameter] Type
                | Procedure Position [Parameter]
                | Constant Literal

data Parameter = Parameter Position Modality Type TokIdent

-- make EnvData printable
instance Show EnvData where
    show (VarType p (TypeBaseType t)) = " at " ++ show p ++ " of type " ++ show t
    show (DefaultProc (TypeBaseType t)) = " default procedure of type " ++ show t
    show (VarType p t) = " at " ++ show p ++ " of type " ++ show t
    show (DefaultProc t) = " default procedure of type " ++ show t

-- Initial environment with default procedures
-- TODO: le procedure "write" quale tipo devono restituire?
defaultEnv = foldl1 Map.union [ Map.singleton "writeInt" (DefaultProc (TypeBaseType BaseType_integer) ),
                                  Map.singleton "writeReal" (DefaultProc (TypeBaseType BaseType_integer) ),
                                  Map.singleton "writeChar" (DefaultProc (TypeBaseType BaseType_integer) ),
                                  Map.singleton "writeString" (DefaultProc (TypeBaseType BaseType_integer) ),
                                  Map.singleton "readInt" (DefaultProc (TypeBaseType BaseType_integer) ),
                                  Map.singleton "readReal" (DefaultProc (TypeBaseType BaseType_real) ),
                                  Map.singleton "readChar" (DefaultProc (TypeBaseType BaseType_char) ),
                                  Map.singleton "readString" (DefaultProc (TypeBaseType BaseType_string) )]


extractInfo :: [IdElem] -> [(Position,String)]
-- e.g. [IdElement (Ident "a"),IdElement (Ident "b")] -> ["a", "b"]
extractInfo (x:xs) = case x of IdElement (TokIdent info) -> info:extractInfo xs
extractInfo [] = []

-- savese info about variables type in env 
populateEnv :: [(Position,String)] -> Type -> Env -> Env
populateEnv [] _ env = env
populateEnv (v:varNames) t env = populateEnv varNames t (Map.insert (snd v) (VarType (fst v) t) env)

lookup :: String -> Env -> Maybe EnvData
lookup = Map.lookup