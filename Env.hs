module Env where

import qualified Data.Map as Map
import AbsGrammar

-- Struttura dati che rappresenta environment
-- Indicizzato con nome identificatore
type Env = Map.Map String EnvData
emptyEnv:: Env
emptyEnv = Map.empty

-- Modella le entry del environment
-- TODO: aggiungere campo Addr dove necessario
data EnvData = VarType Position Type
                | DefaultProc Type
                | Function Position Prms Type
                | Procedure Position Prms
                | Constant Position Type
                | Return Type


-- TODO: aggiungere gli altri casi per gestire EnvData
instance Show EnvData where
    show (VarType p (TypeBaseType t)) = "{variable, " ++ show p ++ ", " ++ show t ++ "}"
    show (VarType p (TypeCompType (Pointer t))) = "{variable, " ++ show p ++ ", pointer to " ++ show t ++ "}"
    show (VarType p (TypeCompType (Array (TokInteger (_,i1)) (TokInteger (_,i2)) t))) = "{variable, " ++ show p ++ ", array ["++ i1++".."++ i2++ "] of " ++ show t ++ "}"
    show (Constant p (TypeBaseType t)) = "{constant, " ++ show p ++ ", " ++ show t ++ "}"
    show (DefaultProc (TypeBaseType t)) = " default procedure of type " ++ show t
    show (DefaultProc t) = " default procedure of type " ++ show t
    show (Function p prms tp) = "{function, " ++ show p ++ ", " ++ show prms ++ ", " ++ show tp ++ "}"
    show (Procedure p prms) = "{procedure, " ++ show p ++ ", " ++ show prms ++  "}"
    show (Return t) = "{exected return type: " ++ show t  ++ "}" 
    -- TODO: this line can be reached if show function is not implemented for all type of params
    -- at the moment, it is not implemented for pointers. This line can be removed when all types can be printed
    show _ = "CANT_SHOW. IMPLEMENT ME!"

-- Environment iniziale con le procedure di default
defaultEnv = foldl1 Map.union [ Map.singleton "writeInt" (DefaultProc (TypeBaseType BaseType_integer) ),
                                  Map.singleton "writeReal" (DefaultProc (TypeBaseType BaseType_integer) ),
                                  Map.singleton "writeChar" (DefaultProc (TypeBaseType BaseType_integer) ),
                                  Map.singleton "writeString" (DefaultProc (TypeBaseType BaseType_integer) ),
                                  Map.singleton "readInt" (DefaultProc (TypeBaseType BaseType_integer) ),
                                  Map.singleton "readReal" (DefaultProc (TypeBaseType BaseType_real) ),
                                  Map.singleton "readChar" (DefaultProc (TypeBaseType BaseType_char) ),
                                  Map.singleton "readString" (DefaultProc (TypeBaseType BaseType_string) )]


mergeEnvs :: Env -> Env -> Env
mergeEnvs = Map.union

insert :: String -> EnvData -> Env -> Env
insert = Map.insert

lookup :: String -> Env -> Maybe EnvData
lookup = Map.lookup

fromList :: [(String, EnvData)] -> Env 
fromList = Map.fromList

getTypeFromLiteral:: Literal -> BaseType
getTypeFromLiteral (LiteralInteger _) = BaseType_integer
getTypeFromLiteral (LiteralString _) = BaseType_string
getTypeFromLiteral (LiteralBoolean _) = BaseType_boolean
getTypeFromLiteral (LiteralDouble _) = BaseType_real
getTypeFromLiteral (LiteralChar _) = BaseType_char
