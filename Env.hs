module Env where

import Prelude hiding (lookup)
import qualified Data.Map as Map
import AbsGrammar
import HelperTAC
import Control.Monad.State.Strict

type SSAState = State SSAStateStruct

data SSAStateStruct = SSAStateStruct {
    idCount :: Int,
    errors :: [String]
}

-- This is the global environment.
-- first argument is the key type, second one the value type 
type Env = Map.Map String EnvData
emptyEnv:: Env
emptyEnv = Map.empty

-- Needed for modelling how data of the map entries in Env should be
-- E.g. variable types: the key is the name of the variable, data created using VarType constructor
-- TODO: create new constructors as needed
data EnvData =    VarType Modality Position Type Addr
                --TODO: rimuovere DefaultProc; funz/proc vanno in function o procedure in base a se restituiscono valore o meno
                | DefaultProc Type
                | Function Position Prms Type Addr
                | Procedure Position Prms Addr
                | Constant Position Type Addr
                | Return Type String Position -- (expected return type from current function, function name, function position)

-- data Parameter = Parameter TokIdent Modality Type deriving Show

-- TODO: aggiungere gli altri casi per gestire EnvData
-- TODO: rifare implementazione di procedure default, per le read servono gli argomenti
-- TODO: aggiunti campo addr ad alcuni tipi di EnvData; stampare anche quelli
-- make EnvData printable
instance Show EnvData where
    show (VarType mod p (TypeBaseType t) adr) = "{variable, " ++ show p ++ ", " ++ show t ++ "}"
    show (VarType mod p (TypeCompType (Pointer t)) adr) = "{variable, " ++ show p ++ ", pointer to " ++ show t ++ "}"
    show (VarType mod p (TypeCompType (Array (TokInteger (_,i1)) (TokInteger (_,i2)) t)) adr) = "{variable, " ++ show p ++ ", Array ["++ i1++".."++ i2++ "] of " ++ show t ++ "}"
    show (Constant p (TypeBaseType t) addr) = "{constant, " ++ show p ++ ", " ++ show t ++ "}"
    show (DefaultProc (TypeBaseType t)) = " default procedure of type " ++ show t -- TODO: why two implementation of DefaultProc?
    show (DefaultProc t) = " default procedure of type " ++ show t
    show (Function p prms tp _) = "{function, " ++ show p ++ ", " ++ show prms ++ ", " ++ show tp ++ "}"
    show (Procedure p prms _) = "{procedure, " ++ show p ++ ", " ++ show prms ++  "}"
    show (Return t _ _) = "{exected return type: " ++ show t  ++ "}" 
    -- TODO: this line can be reached if show function is not implemented for all type of params
    -- at the moment, it is not implemented for pointers. This line can be removed when all types can be printed
    show _ = "CANT_SHOW. IMPLEMENT ME!"

-- Initial environment with default procedures
defaultEnv = foldl1 Map.union [ Map.singleton "writeInt" (DefaultProc (TypeBaseType BaseType_void) ),
                                  Map.singleton "writeReal" (DefaultProc (TypeBaseType BaseType_void) ),
                                  Map.singleton "writeChar" (DefaultProc (TypeBaseType BaseType_void) ),
                                  Map.singleton "writeString" (DefaultProc (TypeBaseType BaseType_void) ),
                                  Map.singleton "readInt" (DefaultProc (TypeBaseType BaseType_integer) ),
                                  Map.singleton "readReal" (DefaultProc (TypeBaseType BaseType_real) ),
                                  Map.singleton "readChar" (DefaultProc (TypeBaseType BaseType_char) ),
                                  Map.singleton "readString" (DefaultProc (TypeBaseType BaseType_string) )]


-- TODO : aggiungere generazione warning quando un identificatore nel env viene sovrascritto? Forse bisogna passare
-- anche errs come parametro?
mergeEnvs :: Env -> Env -> SSAState Env
mergeEnvs e1 e2 = return $ Map.union e1 e2

-- TODO : aggiungere generazione warning quando un identificatore nel env viene sovrascritto? Forse bisogna passare
-- anche errs come parametro?
insert :: String -> EnvData -> Env -> SSAState Env
insert id entry env = return $ Map.insert id entry env

lookup :: String -> Env -> Maybe EnvData
lookup = Map.lookup

getIdAddr :: String -> Env -> StateTAC Addr
getIdAddr id env = case lookup id env of
    Just (VarType _ _ _ addr) -> return addr
    Just (Constant _ _ addr) -> return addr
    _ -> error "TODO : errore id non trovato in env per recuper addr; funzione getIdAddr"

-- TODO : implementare correttamente; soluzione solo temporanea
newIdAddr :: String -> Env -> SSAState Addr
newIdAddr id env = 
    if  Map.member id env 
        then do
            state <- get;
            put $ state {idCount = idCount state + 1};
            return (int2IdName id (idCount state))
        else return $ ProgVar id

int2IdName :: String -> Int -> Addr
int2IdName id k = ProgVar (id ++ show k)

fromList :: [(String, EnvData)] -> Env 
fromList = Map.fromList

getTypeFromLiteral:: Literal -> BaseType
getTypeFromLiteral (LiteralInteger _) = BaseType_integer
getTypeFromLiteral (LiteralString _) = BaseType_string
getTypeFromLiteral (LiteralBoolean _) = BaseType_boolean
getTypeFromLiteral (LiteralDouble _) = BaseType_real
getTypeFromLiteral (LiteralChar _) = BaseType_char
