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
                | Constant Position Type

data Parameter = Parameter TokIdent Modality Type 

-- make EnvData printable
instance Show EnvData where
    show (VarType p (TypeBaseType t)) = "{variable, " ++ show p ++ ", " ++ show t ++ "}"
    show (Constant p (TypeBaseType t)) = "{constant, " ++ show p ++ ", " ++ show t ++ "}"
    show (DefaultProc (TypeBaseType t)) = " default procedure of type " ++ show t
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


-- savese info about variables type in env 
populateEnvVars :: [VrDef] -> Env -> Env
populateEnvVars [] env = env
populateEnvVars (def:vrDefs) env = case def of
    VarDefinition idElements t -> populateEnvVars vrDefs (addToEnv idElements t env)
    where addToEnv:: [IdElem] -> Type -> Env -> Env
          addToEnv [] _ env = env
          addToEnv ((IdElement (TokIdent (pos, id))):xs) t env = addToEnv xs t (Map.insert id (VarType pos t) env)

-- savese info about constants type in env 
populateEnvConsts :: [CsDef] -> Env -> Env
populateEnvConsts [] env = env
populateEnvConsts (c:cs) env = case c of
    ConstDefinition (IdElement (TokIdent (pos, id))) literal ->
        populateEnvConsts cs (Map.insert id (Constant pos (TypeBaseType (getTypeFromLiteral literal))) env)

-- populateEnvFuncs :: FcBlock env infType -> Env -> Env
-- populateEnvFuncs (FuncBlock (TokIdent (pos, id)) params retType _) = Map.insert id (Function pos [] retType)
    
--     -- TODO: implement function and call it from populateEnvFuncs to save info about parameters
--     -- where 
--         -- addParams:: Prms -> [Parameter]


lookup :: String -> Env -> Maybe EnvData
lookup = Map.lookup

getTypeFromLiteral:: Literal -> BaseType
getTypeFromLiteral (LiteralInteger _) = BaseType_integer
getTypeFromLiteral (LiteralString _) = BaseType_string
getTypeFromLiteral (LiteralBoolean _) = BaseType_boolean
getTypeFromLiteral (LiteralDouble _) = BaseType_real
getTypeFromLiteral (LiteralChar _) = BaseType_char