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
                | Function Position Prms Type
                | Procedure Position Prms
                | Constant Position Type

-- data Parameter = Parameter TokIdent Modality Type deriving Show

-- TODO: aggiungere gli altri casi per gestire EnvData
-- make EnvData printable
instance Show EnvData where
    show (VarType p (TypeBaseType t)) = "{variable, " ++ show p ++ ", " ++ show t ++ "}"
    show (Constant p (TypeBaseType t)) = "{constant, " ++ show p ++ ", " ++ show t ++ "}"
    show (DefaultProc (TypeBaseType t)) = " default procedure of type " ++ show t
    show (DefaultProc t) = " default procedure of type " ++ show t
    show (Function p prms tp) = "{function," ++ show p ++ ", " ++ show prms ++ ", " ++ show tp ++ "}"

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


-- TODO : aggiungere generazione warning quando un identificatore nel env viene sovrascritto? Forse bisogna passare
-- anche errs come parametro?
mergeEnvs :: Env -> Env -> Env
mergeEnvs locEnv globEnv = Map.union locEnv globEnv

-- TODO : aggiungere generazione warning quando un identificatore nel env viene sovrascritto? Forse bisogna passare
-- anche errs come parametro?
insert :: String -> EnvData -> Env -> Env
insert id entry env = Map.insert id entry env

lookup :: String -> Env -> Maybe EnvData
lookup = Map.lookup

getTypeFromLiteral:: Literal -> BaseType
getTypeFromLiteral (LiteralInteger _) = BaseType_integer
getTypeFromLiteral (LiteralString _) = BaseType_string
getTypeFromLiteral (LiteralBoolean _) = BaseType_boolean
getTypeFromLiteral (LiteralDouble _) = BaseType_real
getTypeFromLiteral (LiteralChar _) = BaseType_char