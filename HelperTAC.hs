module HelperTAC where
import qualified AbsGrammar
import Control.Monad.Trans.State.Strict
import qualified Data.Sequence as DS
import AbsGrammar (TokInteger(TokInteger))

type Stack a =  [a]

push :: a -> Stack a -> StateTAC (Stack a)
push val stack = return $ val:stack

pop :: Stack a -> StateTAC (a, Stack a)
pop (val:stack) = return (val, stack)
pop [] = error "TODO: trying to pop empty stack" 

peek :: Stack a -> (a, Stack a)
peek stack@(val:_) = (val, stack)
peek [] = error "TODO: trying to pop empty stack" 

newStack :: Stack a
newStack = []

createNewStream :: StateTAC (DS.Seq TACInst)
createNewStream = return DS.empty

closeCurrentStream :: StateTAC ()
closeCurrentStream = do
    (cnt, instrLs, stackStrms) <- get
    (topStream, newStack) <- pop stackStrms
    put (cnt, instrLs DS.>< topStream, newStack)

pushStream :: DS.Seq TACInst -> StateTAC ()
pushStream stream = do
    (cnt, instrLs, stackStrms) <- get
    newStack <- push stream stackStrms
    put (cnt, instrLs, newStack)


data Addr =
      ProgVar { var :: String }
    | TacLit { tacLit :: AbsGrammar.Literal }
    | Temporary { tempInt :: String }
    deriving (Show)

data XAddr =
      Addr Addr
    | ArrayAddr { base :: Addr, offset :: Addr}
    | RefAddr Addr
    deriving Show


type StateTAC = State (Int, DS.Seq TACInst, Stack (DS.Seq TACInst))

--TODO: pensare se è necessario sistemare i label
data TACLabel =
      FuncLab Addr
    | StmtLab String
    | Fall
    deriving (Show)

data TACOp =
          TACAdd
        | TACSub
        | TACDiv
        | TACMul
        | TACMod
        | TACOr
        | TACAnd
        | TACEq
        | TACNotEq
        | TACLessT
        | TACGreatT
        | TACEqLessT
        | TACEqGreatT
        | TACNot
        | TACNeg
        | TACRef
        | TACDeref
        -- TODO: usare un tipo specifico per TAC
        | TACCast AbsGrammar.Type
    deriving (Show)

data TACInst =
      TACBinAss Addr Addr TACOp Addr
    | TACUnAss Addr TACOp Addr
    | TACNulAss Addr Addr
    | TACUncdJmp TACLabel
    | TACCndJmp Addr TACOp Addr TACLabel
    | TACIndxStr Addr Addr Addr
    | TACIndxLd Addr Addr Addr
    | TACAssRef Addr Addr
    | TACAssDeref Addr Addr
    | TACDerefAss Addr Addr
    | TACParam Addr
    | TACPCall Addr Int
    | TACFCall Addr Addr Int
    | TACReturnVoid
    | TACReturn Addr
    | TACLabelledInstr TACLabel TACInst
    | LabelNext TACLabel
    deriving (Show)


newTmpAddr :: StateTAC Addr
newTmpAddr = do
    (k, ls, stackStrm)<-get;
    put (k+1, ls, stackStrm);
    return (int2TmpName k)

int2TmpName :: Int -> Addr
int2TmpName k = Temporary ("t" ++ show k)

--TODO: implementa per bene generazione label
newLabel :: StateTAC TACLabel
newLabel = do
    (k, ls, stackStrm)<-get;
    put (k+1, ls, stackStrm);
    return (StmtLab ("L" ++ show k) )

binToTACOp :: AbsGrammar.BinaryOperator -> TACOp
binToTACOp opr = case opr of
    AbsGrammar.Add -> TACAdd
    AbsGrammar.Sub -> TACSub
    AbsGrammar.Div -> TACDiv
    AbsGrammar.Mul -> TACMul
    AbsGrammar.Mod -> TACMod
    AbsGrammar.Or -> TACOr
    AbsGrammar.And -> TACAnd
    AbsGrammar.Eq -> TACEq
    AbsGrammar.NotEq -> TACNotEq
    AbsGrammar.LessT -> TACLessT
    AbsGrammar.GreatT -> TACGreatT
    AbsGrammar.EqLessT -> TACEqLessT
    AbsGrammar.EqGreatT -> TACEqGreatT

unrToTACOp :: AbsGrammar.UnaryOperator -> TACOp
unrToTACOp opr = case opr of
    AbsGrammar.Not -> TACNot
    AbsGrammar.Negation -> TACNeg
    AbsGrammar.Reference -> TACRef
    AbsGrammar.Dereference -> TACDeref


notRel :: AbsGrammar.BinaryOperator -> TACOp
notRel opr = case opr of
    AbsGrammar.Eq -> TACNotEq
    AbsGrammar.NotEq -> TACEq
    AbsGrammar.LessT -> TACEqGreatT
    AbsGrammar.GreatT -> TACEqLessT
    AbsGrammar.EqLessT -> TACGreatT
    AbsGrammar.EqGreatT -> TACLessT


getVarDefaultVal :: AbsGrammar.Type -> Addr
getVarDefaultVal tp = case tp of
    AbsGrammar.TypeBaseType (AbsGrammar.BaseType_integer) ->
        TacLit (AbsGrammar.LiteralInteger (AbsGrammar.TokInteger ((0,0),"0")))
    AbsGrammar.TypeBaseType (AbsGrammar.BaseType_real) -> 
        TacLit (AbsGrammar.LiteralDouble (AbsGrammar.TokDouble ((0,0),"0.0")))
    _ -> TacLit (AbsGrammar.LiteralInteger (AbsGrammar.TokInteger ((0,0),"0")))


convertIntToExpr :: Int -> AbsGrammar.EXPR AbsGrammar.Type
convertIntToExpr x = AbsGrammar.ExprLiteral (AbsGrammar.LiteralInteger (AbsGrammar.TokInteger ((0,0), show x)))


sizeof :: AbsGrammar.Type -> Int
sizeof (AbsGrammar.TypeBaseType bType) = case bType of
    AbsGrammar.BaseType_boolean -> 1
    AbsGrammar.BaseType_char -> 1
    AbsGrammar.BaseType_integer -> 4
    AbsGrammar.BaseType_real -> 4
    --AbsGrammar.BaseType_string -> 0
    _ -> error "TODO: invalid type for sizeof"

--TODO: spiega nella relazione come vengono fatti accessi con indici, considerando che possono essere sfasati
-- (e.g. non partono da zero)
sizeof (AbsGrammar.TypeCompType cType) = case cType of
    --AbsGrammar.Array (TokInteger (_, start)) (TokInteger (_, end)) tp@(AbsGrammar.TypeBaseType _) -> sizeof tp 
    AbsGrammar.Array (TokInteger (_, start)) (TokInteger (_, end)) tp -> ((read end :: Int) - (read start :: Int)) * sizeof tp 
    AbsGrammar.Pointer _-> error "TODO: sizeof -> implementare pointer; valuta se in grammatica può puntare a Type invece che BaseType "
