module HelperTAC where
import qualified AbsGrammar
import Control.Monad.Trans.State.Strict
import qualified Data.Sequence as DS
import AbsGrammar (TokInteger(TokInteger), TokChar (TokChar), TokDouble (TokDouble), TokString (TokString), TokBoolean (TokBoolean))
import qualified Data.Map as DM
import Debug.Trace (traceM)
import Data.Map (Map)

type Stack a =  [a]

newtype TACQuadSeq = TACQuadSeq (DS.Seq TACQuad)

data TACLiteral = TACString String | TACInt Int | TACReal Float | TACChar Char | TACBool Bool
    deriving (Eq, Ord)

makeTACLit :: AbsGrammar.Literal -> TACLiteral
makeTACLit (AbsGrammar.LiteralInteger (TokInteger (_, val))) = TACInt (read val :: Int)
makeTACLit (AbsGrammar.LiteralChar (TokChar (_, val))) = TACChar (read val :: Char)
makeTACLit (AbsGrammar.LiteralBoolean (TokBoolean (_, val))) = TACBool (read val :: Bool)
makeTACLit (AbsGrammar.LiteralDouble (TokDouble (_, val))) = TACReal (read val :: Float)
makeTACLit (AbsGrammar.LiteralString (TokString (_, val))) = TACString (read val :: String)


appendQuad :: TACQuadSeq -> TACQuad -> TACQuadSeq
appendQuad (TACQuadSeq seq) quad = TACQuadSeq (seq DS.|> quad)

concatQSeq :: TACQuadSeq -> TACQuadSeq -> TACQuadSeq
concatQSeq (TACQuadSeq seq1) (TACQuadSeq seq2) = TACQuadSeq (seq1 DS.>< seq2) 


push :: a -> Stack a -> State b (Stack a)
push val stack = return $ val:stack

pop :: Stack a -> State b (a, Stack a)
pop (val:stack) = return (val, stack)
pop [] = error "TODO: trying to pop empty stack" 

peek :: Stack a -> (a, Stack a)
peek stack@(val:_) = (val, stack)
peek [] = error "TODO: trying to pop empty stack" 

newStack :: Stack a
newStack = []

createNewStream :: StateTAC TACQuadSeq
createNewStream = return $ TACQuadSeq DS.empty

closeCurrentStream :: StateTAC ()
closeCurrentStream = do
    state <- get
    (topStream, newStack) <- pop $ stackStrms state
    put $ state { quads = concatQSeq (quads state) topStream, stackStrms = newStack }

pushStream :: TACQuadSeq -> StateTAC ()
pushStream stream = do
    state <- get
    newStack <- push stream (stackStrms state)
    put $ state {stackStrms = newStack}


data Addr =
      ProgVar { var :: String }
    | TacLit { tacLit :: TACLiteral }
    | Temporary { tempInt :: String }
    deriving (Eq, Ord)

newtype AddrList = AddrList [Addr]

appendAddrList :: Addr -> AddrList -> AddrList
appendAddrList addr (AddrList list) = AddrList (addr:list) 

data XAddr =
      Addr Addr
    | ArrayAddr { base :: Addr, offset :: Addr}
    | RefAddr Addr


type StateTAC = State TACStateStruct

data TACStateStruct = TACStateStruct {
    quads :: TACQuadSeq,
    stackStrms :: Stack TACQuadSeq,
    tmpCount :: Int,
    labCount :: Int,
    labelsNextInstr :: AddrList
}

--TODO: pensare se è necessario sistemare i label
data TACLabel =
      FuncLab Addr
    | StmtLab Addr
    | Fall
    deriving (Eq, Ord)

getLabelAddr :: TACLabel -> Addr
getLabelAddr (FuncLab addr) = addr
getLabelAddr (StmtLab addr) = addr
getLabelAddr Fall = error "TODO: Internal Error: getLabelAddr -> should not be trying to get Fall address"

data TACQuad = TACQuad {
    labels :: AddrList,
    operator :: Maybe TACOp,
    operand1 :: Maybe Addr,
    operand2 :: Maybe Addr,
    result   :: Maybe Addr
}


instrToQuad :: TACInst -> AddrList -> StateTAC TACQuad
instrToQuad (TACBinAss res op1 opr op2) labels = return (TACQuad labels (Just opr) (Just op1) (Just op2) (Just res))
instrToQuad (TACUnAss res opr op1) labels = return (TACQuad labels (Just opr) (Just op1) Nothing (Just res))
instrToQuad (TACNulAss res op1) labels = return (TACQuad labels Nothing (Just op1) Nothing (Just res))
instrToQuad (TACUncdJmp lab) labels = return (TACQuad labels Nothing Nothing Nothing (Just $ getLabelAddr lab ))
instrToQuad (TACCndJmp op1 opr op2 lab) labels = return (TACQuad labels (Just (toJmpOp opr)) (Just op1) (Just op2) (Just $ getLabelAddr lab))
instrToQuad (TACIndxLd dst src indx) labels = return (TACQuad labels (Just TACIdxL) (Just src) (Just indx) (Just dst))
instrToQuad (TACIndxStr dst indx src) labels = return (TACQuad labels (Just TACIdxS) (Just src) (Just indx) (Just dst))
instrToQuad (TACAssDeref res op) labels = return (TACQuad labels (Just TACDeref) (Just op) Nothing (Just res))
instrToQuad (TACAssRef res op) labels = return (TACQuad labels (Just TACRef) (Just op) Nothing (Just res))
instrToQuad (TACDerefAss res op) labels = return (TACQuad labels (Just TACDeref) Nothing (Just op) (Just res))
instrToQuad (TACParam prm) labels = return (TACQuad labels (Just TACPrm) Nothing Nothing (Just prm))
instrToQuad (TACPCall pName nPrm) labels = return (TACQuad labels (Just TACPCl) (Just pName) (Just $ TacLit(TACInt nPrm)) Nothing)
instrToQuad (TACFCall res fName nPrm) labels = return (TACQuad labels (Just TACFCl) (Just fName) (Just $ TacLit(TACInt nPrm)) (Just res))
instrToQuad (TACReturn res) labels = return (TACQuad labels (Just TACRt) Nothing Nothing (Just res))
instrToQuad TACReturnVoid labels = return (TACQuad labels (Just TACRt) Nothing Nothing Nothing)

type LabelRef = DM.Map TACLabel Integer
newLabelRef :: LabelRef
newLabelRef = DM.empty

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

        | TACIdxL
        | TACIdxS

        | TACPrm
        | TACPCl
        | TACFCl
        | TACRt
        -- | TACRtVd

        | TACJmpEq
        | TACJmpNotEq
        | TACJmpLessT
        | TACJmpGreatT
        | TACJmpEqLessT
        | TACJmpEqGreatT
        | TACJmp

        | TACNot
        | TACNeg
        | TACRef
        | TACDeref
        -- TODO: usare un tipo specifico per TAC
        | TACCast AbsGrammar.Type

toJmpOp :: TACOp -> TACOp
toJmpOp TACEq = TACJmpEq
toJmpOp TACNotEq = TACJmpNotEq
toJmpOp TACLessT = TACJmpLessT
toJmpOp TACGreatT = TACJmpGreatT
toJmpOp TACEqLessT = TACJmpEqLessT
toJmpOp TACEqGreatT = TACJmpEqGreatT

isCndJmpOp :: TACOp -> Bool
isCndJmpOp TACJmpEq = True
isCndJmpOp TACJmpNotEq = True
isCndJmpOp TACJmpLessT = True
isCndJmpOp TACJmpGreatT = True
isCndJmpOp TACJmpEqLessT = True
isCndJmpOp TACJmpEqGreatT = True
isCndJmpOp _ = False

data TACInst =
      TACBinAss Addr Addr TACOp Addr
    | TACUnAss Addr TACOp Addr
    | TACNulAss Addr Addr
    | TACUncdJmp TACLabel
    --TODO: implementa boolean valued conditional jump
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
   -- | TACLabelledInstr TACLabel TACInst
   -- | LabelNext TACLabel

newTmpAddr :: StateTAC Addr
newTmpAddr = do
    state <- get;  
    tmpName <- int2TmpName (tmpCount state)
    put $ state {tmpCount = tmpCount state + 1}
    return tmpName

int2TmpName :: Int -> StateTAC Addr
int2TmpName k = return $ Temporary ("t" ++ show k)

--TODO: implementa per bene generazione label
newLabel :: StateTAC TACLabel
newLabel = do
    state <- get;
    put $ state {labCount = labCount state + 1};
    return (StmtLab (ProgVar ("L" ++ show (labCount state))) )

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
    AbsGrammar.TypeBaseType AbsGrammar.BaseType_integer -> TacLit (TACInt 0)
    AbsGrammar.TypeBaseType AbsGrammar.BaseType_real -> TacLit (TACReal 0.0)
    _ -> TacLit (TACInt 0)


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
