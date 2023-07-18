module Env where

import Prelude hiding (lookup)
import qualified Data.Map as Map
import AbsGrammar
import HelperTAC
import Control.Monad.State.Strict
import Errs
import Data.Bits


-------------------------Environment----------------------------------------------------------------

-- This is the global environment.
-- first argument is the key type, second one the value type 
type Env = Map.Map String EnvData

emptyEnv:: Env
emptyEnv = Map.empty

-- Needed for modelling how data of the map entries in Env should be
-- E.g. variable types: the key is the name of the variable, data created using VarType constructor
-- TODO: create new constructors as needed
data EnvData =    Variable Modality Position Type Addr
                | Function Position Prms Type Addr
                | Procedure Position Prms Addr
                | Constant Position Type Addr
                | Return Type String Position -- (expected return type from current function, function name, function position)
                | InsideLoop TACLabel -- used to check if break/continue are inside a loop

-- data Parameter = Parameter TokIdent Modality Type deriving Show

-- TODO: aggiungere gli altri casi per gestire EnvData
-- TODO: aggiunti campo addr ad alcuni tipi di EnvData; stampare anche quelli
-- make EnvData printable
instance Show EnvData where
    show (Variable mod p (TypeBaseType t) adr) = "{variable, " ++ show p ++ ", " ++ show t ++ "}"
    show (Variable mod p (TypeCompType (Pointer t)) adr) = "{variable, " ++ show p ++ ", pointer to " ++ show t ++ "}"
    show (Variable mod p (TypeCompType (Array (TokInteger (_,i1)) (TokInteger (_,i2)) t)) adr) = "{variable, " ++ show p ++ ", Array ["++ i1++".."++ i2++ "] of " ++ show t ++ "}"
    show (Constant p (TypeBaseType t) addr) = "{constant, " ++ show p ++ ", " ++ show t ++ "}"
    show (Function p prms tp _) = "{function, " ++ show p ++ ", " ++ show prms ++ ", " ++ show tp ++ "}"
    show (Procedure p prms _) = "{procedure, " ++ show p ++ ", " ++ show prms ++  "}"
    show (Return t _ _) = "{exected return type: " ++ show t  ++ "}" 
    show (InsideLoop _) = "inside loop "
    -- TODO: this line can be reached if show function is not implemented for all type of params
    -- at the moment, it is not implemented for pointers. This line can be removed when all types can be printed
    show _ = "CANT_SHOW. IMPLEMENT ME!"

-- Initial environment with default procedures
defaultEnv :: Map.Map String EnvData
defaultEnv = foldl1 Map.union [ Map.singleton "writeInt" (Procedure (0,0) (Params [Param Modality_val [IdElement (TokIdent ((0,0),"val"))] (TypeBaseType BaseType_integer)]) (ProgVar "writeInt") ),
                                  Map.singleton "writeReal" (Procedure (0,0) (Params [Param Modality_val [IdElement (TokIdent ((0,0),"val"))] (TypeBaseType BaseType_real)]) (ProgVar "writeReal") ),
                                  Map.singleton "writeChar" (Procedure (0,0) (Params [Param Modality_val [IdElement (TokIdent ((0,0),"val"))] (TypeBaseType BaseType_char)]) (ProgVar "writeChar") ),
                                  Map.singleton "writeString" (Procedure (0,0) (Params [Param Modality_val [IdElement (TokIdent ((0,0),"val"))] (TypeBaseType BaseType_string)]) (ProgVar "writeString") ),
                                  Map.singleton "readInt" (Procedure (0,0) (Params [Param Modality_ref [IdElement (TokIdent ((0,0),"val"))] (TypeBaseType BaseType_integer)]) (ProgVar "readInt") ),
                                  Map.singleton "readReal" (Procedure (0,0) (Params [Param Modality_ref [IdElement (TokIdent ((0,0),"val"))] (TypeBaseType BaseType_real)]) (ProgVar "readReal") ),
                                  Map.singleton "readChar" (Procedure (0,0) (Params [Param Modality_ref [IdElement (TokIdent ((0,0),"val"))] (TypeBaseType BaseType_char)]) (ProgVar "readChar") ),
                                  Map.singleton "readString" (Procedure (0,0) (Params [Param Modality_ref [IdElement (TokIdent ((0,0),"val"))] (TypeBaseType BaseType_string)]) (ProgVar "readString") ) ]


-- TODO : aggiungere generazione warning quando un identificatore nel env viene sovrascritto? Forse bisogna passare
-- anche errs come parametro?
mergeEnvs :: Env -> Env -> SSAState Env
mergeEnvs e1 e2 = return $ Map.union e1 e2

-- TODO : aggiungere generazione warning quando un identificatore nel env viene sovrascritto? Forse bisogna passare
-- anche errs come parametro?
insert :: String -> EnvData -> Env -> State a Env
insert id entry env = return $ Map.insert id entry env

lookup :: String -> Env -> Maybe EnvData
lookup = Map.lookup

getIdAddr :: String -> Env -> StateTAC Addr
getIdAddr id env = case lookup id env of
    Just (Variable _ _ _ addr) -> return addr
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

newBreakContLabels :: SSAState (TACLabel, TACLabel)
newBreakContLabels = do
    state <- get;
    put $ state {labelCount = labelCount state + 1};
    return (StmtLab (ProgVar ("B" ++ show (labelCount state))) , StmtLab (ProgVar ("C" ++ show (labelCount state))) )

fromList :: [(String, EnvData)] -> Env 
fromList = Map.fromList

getTypeFromLiteral:: Literal -> BaseType
getTypeFromLiteral (LiteralInteger _) = BaseType_integer
getTypeFromLiteral (LiteralString _) = BaseType_string
getTypeFromLiteral (LiteralBoolean _) = BaseType_boolean
getTypeFromLiteral (LiteralDouble _) = BaseType_real
getTypeFromLiteral (LiteralChar _) = BaseType_char

------------------------------------------------------------------------------------------


-------------Static Semantic Analysis Utils-----------------------------------------------

type Problems = [Problem]

emptyProblems :: [Problem]
emptyProblems = []

data PosEnds = PosEnds {
    leftmost :: Position,
    rightmost :: Position
}

instance Show PosEnds where {
    show (PosEnds leftmost rightmost) = show leftmost ++"-"++ show rightmost
}

type SSAState = State SSAStateStruct

data SSAStateStruct = SSAStateStruct {
    idCount :: Int,
    labelCount :: Int,
    errors :: Problems,
    unInitVars :: Stack Env
}

data BitVector
    = BitVector {
      size :: !Int,
      value  :: !Integer  
    } deriving (Show)

zeros :: Int -> BitVector
zeros n = BitVector n 0

isAllOnes :: BitVector -> Bool
isAllOnes bitVector = value bitVector ==  (2 ^ fromIntegral (size bitVector)) - 1

setVectorBit :: BitVector -> Int -> BitVector
setVectorBit bitVector bitIndex = if bitIndex < size bitVector 
    then bitVector { value = setBit (value bitVector) bitIndex} 
    else error "Setting bit in vector with out of bounds index"

-- Returns annotated type for expressions
getTypeFromExpression :: EXPR Type -> SSAState Type
getTypeFromExpression (UnaryExpression op exp t) = return t
getTypeFromExpression (BinaryExpression op exp1 exp2 t) = return t
getTypeFromExpression (ExprLiteral literal) = return (TypeBaseType (getTypeFromLiteral literal) )
getTypeFromExpression (ExprCall call t) = return t
getTypeFromExpression (BaseExpr bexp t) = return t
getTypeFromExpression (SelExpr expcond exp1 exp2 t) = return t
getTypeFromExpression (IntToReal _) = return $ TypeBaseType BaseType_real

getTypeFromBaseExpression:: BEXPR Type -> Env -> SSAState Type
getTypeFromBaseExpression (Identifier (TokIdent (tokpos, tokid)) ) env = 
    case Env.lookup tokid env of
        Just (Variable _ _ envType _ ) -> return envType
        Just (Constant _ envType _) -> return envType
        Just _ -> return $ TypeBaseType BaseType_error
        Nothing -> return $ TypeBaseType BaseType_error
getTypeFromBaseExpression (ArrayElem bexpr iexpr) env = getTypeFromExpression bexpr

-- Type compatibility for operations
sup :: Type -> Type -> Type
sup t1 t2
    | t1 == t2 = t1
    | t1 == (TypeBaseType BaseType_integer) && t2 == (TypeBaseType BaseType_real) = (TypeBaseType BaseType_real)
    | t1 == (TypeBaseType BaseType_real) && t2 == (TypeBaseType BaseType_integer) = (TypeBaseType BaseType_real)
    | otherwise = (TypeBaseType BaseType_error)

getLitPosEnds :: Literal -> PosEnds
getLitPosEnds (LiteralInteger (TokInteger (pos@(x,y), val))) = PosEnds {leftmost=pos, rightmost = (x, y + (length val))}
getLitPosEnds (LiteralChar (TokChar (pos@(x,y), val))) = PosEnds {leftmost=pos, rightmost = (x, y + (length val))}
getLitPosEnds (LiteralString (TokString (pos@(x,y), val))) = PosEnds {leftmost=pos, rightmost = (x, y + (length val))}
getLitPosEnds (LiteralBoolean (TokBoolean (pos@(x,y), val))) = PosEnds {leftmost=pos, rightmost = (x, y + (length val))}
getLitPosEnds (LiteralDouble (TokDouble (pos@(x,y), val))) = PosEnds {leftmost=pos, rightmost = (x, y + (length val))}


getLitValue :: Literal -> String
getLitValue (LiteralInteger tok) = getTokValue (TokI tok)
getLitValue (LiteralChar tok) = getTokValue (TokC tok)
getLitValue (LiteralString tok) = getTokValue (TokS tok)
getLitValue (LiteralBoolean tok) = getTokValue (TokB tok)
getLitValue (LiteralDouble tok) = getTokValue (TokD tok)

getTokValue :: TokWrap -> String
getTokValue (TokId (TokIdent (_,val))) = val
getTokValue (TokI (TokInteger (_,val))) = val
getTokValue (TokD (TokDouble (_,val))) = val
getTokValue (TokS (TokString (_,val))) = val
getTokValue (TokC (TokChar (_,val))) = val
getTokValue (TokB (TokBoolean (_,val))) = val


insertVar :: String -> EnvData -> SSAState ()
insertVar id entry = do
    state <- get
    (locUninit, poppedStack) <- pop $ unInitVars state
    newLocUninit <- Env.insert id entry locUninit
    newStack <- push newLocUninit poppedStack
    put (state {unInitVars = newStack})


removeVar :: String -> SSAState ()
removeVar id = do
    state <- get
    newStack <- recRem id (unInitVars state)
    put $ state{ unInitVars = newStack }
    where
        recRem :: String -> [Env] -> SSAState [Env]
        recRem _ [] = return []
        recRem key (topEnv:rest) = 
            if Map.member key topEnv 
                then do
                    return $ (Map.delete key topEnv):rest
                else do
                    recRest <- recRem key rest
                    return $ topEnv : recRest


pushNewUninit :: SSAState()
pushNewUninit = do
    state <- get
    newStack <- push emptyEnv (unInitVars state)
    put $ state {unInitVars = newStack}

popUninit :: SSAState ()
popUninit = do
    state <- get
    (locUninit, rest) <- pop $ unInitVars state
    put $ state { unInitVars = rest }
    makeUninitErrs (Map.toList locUninit)
    where
        makeUninitErrs :: [(String, EnvData)] -> SSAState ()
        makeUninitErrs [] = return ()
        makeUninitErrs ((id, Variable _ pos@(x,y) _ _ ):rest) = do
            state <- get
            put $ state { errors = (Error, UninitializedVariable (show posEnds) id):errors state}
            makeUninitErrs rest
            where
                posEnds = PosEnds { leftmost = pos, rightmost = (x, y + length id) }


isInRange :: Literal -> CompType -> Bool
isInRange lit@(LiteralInteger _) (Array l_tok r_tok _) =
    (indx >= l_end) && (indx <= r_end)
    where
        indx = read (getLitValue lit) :: Int
        l_end = read (getTokValue (TokI l_tok)) :: Int
        r_end = read (getTokValue (TokI r_tok)) :: Int


lengthForBitVector :: AbsGrammar.Type -> Int
lengthForBitVector (AbsGrammar.TypeCompType cType) = case cType of
    AbsGrammar.Array (TokInteger (_, start)) (TokInteger (_, end)) tp -> (((read end :: Int) - (read start :: Int)) + 1) + lengthForBitVector tp 
    AbsGrammar.Pointer _-> 0

lengthForBitVector _ = 0;


------------------------------------------------------------------------------------------
