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

data TACLiteral = TACIntLit Int | TACRealLit Float | TACCharLit Char | TACBoolLit Bool | TACMemAddrLit  
    deriving (Eq, Ord)

data TACType = TACIntType | TACFloatType | TACCharType | TACBoolType | TACMemAddrType deriving (Show)

makeTACLit :: AbsGrammar.Literal -> TACLiteral
makeTACLit (AbsGrammar.LiteralInteger (TokInteger (_, val))) = TACIntLit (read val :: Int)
makeTACLit (AbsGrammar.LiteralChar (TokChar (_, val))) = TACCharLit (read val :: Char)
makeTACLit (AbsGrammar.LiteralBoolean (TokBoolean (_, val))) = TACBoolLit (if val=="true" then True else False)
makeTACLit (AbsGrammar.LiteralDouble (TokDouble (_, val))) = TACRealLit (read val :: Float)
--makeTACLit (AbsGrammar.LiteralString (TokString (_, val))) = TACString (read val :: String)


appendQuad :: TACQuadSeq -> TACQuad -> TACQuadSeq
appendQuad (TACQuadSeq seq) quad = TACQuadSeq (seq DS.|> quad)

concatQSeq :: TACQuadSeq -> TACQuadSeq -> TACQuadSeq
concatQSeq (TACQuadSeq seq1) (TACQuadSeq seq2) = TACQuadSeq (seq1 DS.>< seq2) 

------ STACK SUPPORT

push :: a -> Stack a -> State b (Stack a)
push val stack = return $ val:stack

pop :: Stack a -> State b (a, Stack a)
pop (val:stack) = return (val, stack)
pop [] = error "TODO: trying to pop empty stack" 

peek :: Stack a -> (a, Stack a)
peek stack@(val:_) = (val, stack)
peek [] = error "TODO: trying to pop empty stack" 

------------------

------------Stream for function declaration parsing
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
---------------------------------------

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
instrToQuad (TACPCall pName nPrm) labels = return (TACQuad labels (Just TACPCl) (Just pName) (Just $ TacLit(TACIntLit nPrm)) Nothing)
instrToQuad (TACFCall res fName nPrm) labels = return (TACQuad labels (Just TACFCl) (Just fName) (Just $ TacLit(TACIntLit nPrm)) (Just res))
instrToQuad (TACReturn res) labels = return (TACQuad labels (Just TACRt) Nothing Nothing (Just res))
instrToQuad TACReturnVoid labels = return (TACQuad labels (Just TACRt) Nothing Nothing Nothing)

type LabelRef = DM.Map TACLabel Integer
newLabelRef :: LabelRef
newLabelRef = DM.empty

data TACOp =
          TACAddInt
        | TACAddFloat
        | TACSubInt
        | TACSubFloat
        | TACDivInt
        | TACDivFloat
        | TACMulInt
        | TACMulFloat
        | TACModInt
        | TACOr
        | TACAnd
        | TACEqBool
        | TACEqChar
        | TACEqInt
        | TACEqFloat
        | TACNotEqBool
        | TACNotEqChar
        | TACNotEqInt
        | TACNotEqFloat
        | TACLessTInt
        | TACLessTFloat
        | TACGreatTInt
        | TACGreatTFloat
        | TACEqLessTInt
        | TACEqLessTFloat
        | TACEqGreatTInt
        | TACEqGreatTFloat

        | TACIdxL
        | TACIdxS

        | TACPrm
        | TACPCl
        | TACFCl
        | TACRt
        -- | TACRtVd

        | TACJmpEqInt
        | TACJmpEqFloat
        | TACJmpEqChar
        | TACJmpEqBool
        | TACJmpNotEqInt
        | TACJmpNotEqFloat
        | TACJmpNotEqChar
        | TACJmpNotEqBool
        | TACJmpLessTInt
        | TACJmpLessTFloat
        | TACJmpGreatTInt
        | TACJmpGreatTFloat
        | TACJmpEqLessTInt
        | TACJmpEqLessTFloat
        | TACJmpEqGreatTInt
        | TACJmpEqGreatTFloat
        | TACJmp

        | TACNot
        | TACNegInt
        | TACNegFloat
        | TACRef
        | TACDeref

        | TACCastIntToReal

toJmpOp :: TACOp -> TACOp
toJmpOp TACEqInt = TACJmpEqInt
toJmpOp TACEqFloat = TACJmpEqFloat
toJmpOp TACEqChar = TACJmpEqChar
toJmpOp TACEqBool = TACJmpEqBool
toJmpOp TACNotEqInt = TACJmpNotEqInt
toJmpOp TACNotEqFloat = TACJmpNotEqFloat
toJmpOp TACNotEqChar = TACJmpNotEqChar
toJmpOp TACNotEqBool = TACJmpNotEqBool
toJmpOp TACLessTInt = TACJmpLessTInt
toJmpOp TACLessTFloat = TACJmpLessTFloat
toJmpOp TACGreatTInt = TACJmpGreatTInt
toJmpOp TACGreatTFloat = TACJmpGreatTFloat
toJmpOp TACEqLessTInt = TACJmpEqLessTInt
toJmpOp TACEqLessTFloat = TACJmpEqLessTFloat
toJmpOp TACEqGreatTInt = TACJmpEqGreatTInt
toJmpOp TACEqGreatTFloat = TACJmpEqGreatTFloat

isCndJmpOp :: TACOp -> Bool
isCndJmpOp TACJmpEqInt = True
isCndJmpOp TACJmpEqFloat = True
isCndJmpOp TACJmpEqChar = True
isCndJmpOp TACJmpEqBool = True
isCndJmpOp TACJmpNotEqInt = True
isCndJmpOp TACJmpNotEqFloat = True
isCndJmpOp TACJmpNotEqChar = True
isCndJmpOp TACJmpNotEqBool = True
isCndJmpOp TACJmpLessTInt = True
isCndJmpOp TACJmpLessTFloat = True
isCndJmpOp TACJmpGreatTInt = True
isCndJmpOp TACJmpGreatTFloat = True
isCndJmpOp TACJmpEqLessTInt = True
isCndJmpOp TACJmpEqLessTFloat = True
isCndJmpOp TACJmpEqGreatTInt = True
isCndJmpOp TACJmpEqGreatTFloat = True
isCndJmpOp _ = False

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

binToTACOp :: AbsGrammar.BinaryOperator -> TACType -> TACOp
binToTACOp opr tacTp = case (opr, tacTp) of
    (AbsGrammar.Add , TACIntType) -> TACAddInt
    (AbsGrammar.Add , TACFloatType) -> TACAddFloat
    (AbsGrammar.Sub, TACIntType) -> TACSubInt
    (AbsGrammar.Sub, TACFloatType) -> TACSubFloat
    (AbsGrammar.Div, TACIntType) -> TACDivInt
    (AbsGrammar.Div, TACFloatType) -> TACDivFloat
    (AbsGrammar.Mul, TACIntType) -> TACMulInt
    (AbsGrammar.Mul, TACFloatType) -> TACMulFloat
    (AbsGrammar.Mod, TACIntType) -> TACModInt
    (AbsGrammar.Or, _) -> TACOr
    (AbsGrammar.And, _) -> TACAnd
    (AbsGrammar.Eq, TACIntType) -> TACEqInt
    (AbsGrammar.Eq, TACFloatType) -> TACEqFloat
    (AbsGrammar.Eq, TACCharType) -> TACEqChar
    (AbsGrammar.Eq, TACBoolType) -> TACEqBool
    (AbsGrammar.NotEq, TACIntType) -> TACNotEqInt
    (AbsGrammar.NotEq, TACFloatType) -> TACNotEqFloat
    (AbsGrammar.NotEq, TACCharType) -> TACNotEqChar
    (AbsGrammar.NotEq, TACBoolType) -> TACNotEqBool
    (AbsGrammar.LessT, TACIntType) -> TACLessTInt
    (AbsGrammar.LessT, TACFloatType) -> TACLessTFloat    
    (AbsGrammar.GreatT, TACIntType) -> TACGreatTInt
    (AbsGrammar.GreatT, TACFloatType) -> TACGreatTFloat
    (AbsGrammar.EqLessT, TACIntType) -> TACEqLessTInt
    (AbsGrammar.EqLessT, TACFloatType) -> TACEqLessTFloat
    (AbsGrammar.EqGreatT, TACIntType) -> TACEqGreatTInt
    (AbsGrammar.EqGreatT, TACFloatType) -> TACEqGreatTFloat

unrToTACOp :: AbsGrammar.UnaryOperator -> TACType -> TACOp
unrToTACOp opr tacTp = case (opr, tacTp) of
    (AbsGrammar.Not, _) -> TACNot
    (AbsGrammar.Negation, TACFloatType) -> TACNegFloat
    (AbsGrammar.Negation, TACIntType) -> TACNegInt
    (AbsGrammar.Reference, _) -> TACRef
    (AbsGrammar.Dereference, _) -> TACDeref

isGuardBinOp :: AbsGrammar.BinaryOperator -> Bool
isGuardBinOp opr = case opr of
    AbsGrammar.Eq -> True
    AbsGrammar.NotEq -> True
    AbsGrammar.EqLessT-> True
    AbsGrammar.EqGreatT-> True
    AbsGrammar.GreatT-> True
    AbsGrammar.LessT-> True
    AbsGrammar.And -> True
    AbsGrammar.Or -> True
    _ -> False

notRel :: AbsGrammar.BinaryOperator -> TACType -> TACOp
notRel opr tacTp = case (opr,tacTp) of
    (AbsGrammar.Eq, TACIntType) -> TACNotEqInt
    (AbsGrammar.Eq, TACFloatType) -> TACNotEqFloat
    (AbsGrammar.Eq, TACCharType) -> TACNotEqChar
    (AbsGrammar.Eq, TACBoolType) -> TACNotEqBool
    (AbsGrammar.NotEq, TACIntType) -> TACEqInt
    (AbsGrammar.NotEq, TACFloatType) -> TACEqFloat
    (AbsGrammar.NotEq, TACCharType) -> TACEqChar
    (AbsGrammar.NotEq, TACBoolType) -> TACEqBool
    (AbsGrammar.EqLessT, TACIntType) -> TACGreatTInt
    (AbsGrammar.EqLessT, TACFloatType) -> TACGreatTFloat
    (AbsGrammar.EqGreatT, TACIntType) -> TACLessTInt
    (AbsGrammar.EqGreatT, TACFloatType) -> TACLessTFloat
    (AbsGrammar.GreatT, TACIntType) -> TACEqLessTInt
    (AbsGrammar.GreatT, TACFloatType) -> TACEqLessTFloat
    (AbsGrammar.LessT, TACIntType) -> TACEqGreatTInt
    (AbsGrammar.LessT, TACFloatType) -> TACEqGreatTFloat
    

gramTypeToTACType :: AbsGrammar.Type -> TACType
gramTypeToTACType (AbsGrammar.TypeBaseType AbsGrammar.BaseType_boolean) = TACBoolType
gramTypeToTACType (AbsGrammar.TypeBaseType AbsGrammar.BaseType_char) = TACCharType
gramTypeToTACType (AbsGrammar.TypeBaseType AbsGrammar.BaseType_integer) = TACIntType
gramTypeToTACType (AbsGrammar.TypeBaseType AbsGrammar.BaseType_real) = TACFloatType
gramTypeToTACType (AbsGrammar.TypeBaseType AbsGrammar.BaseType_string) = TACMemAddrType
gramTypeToTACType (AbsGrammar.TypeCompType _) = TACMemAddrType
gramTypeToTACType _ = error "TODO: gramTypeToTACType: internal error"


getVarDefaultVal :: AbsGrammar.Type -> Addr
getVarDefaultVal tp = case tp of
    AbsGrammar.TypeBaseType AbsGrammar.BaseType_integer -> TacLit (TACIntLit 0)
    AbsGrammar.TypeBaseType AbsGrammar.BaseType_real -> TacLit (TACRealLit 0.0)
    _ -> TacLit (TACIntLit 0)


convertIntToExpr :: Int -> AbsGrammar.EXPR AbsGrammar.Type
convertIntToExpr x = AbsGrammar.ExprLiteral (AbsGrammar.LiteralInteger (AbsGrammar.TokInteger ((0,0), show x)))


sizeof :: AbsGrammar.Type -> Int
sizeof (AbsGrammar.TypeBaseType bType) = case bType of
    AbsGrammar.BaseType_boolean -> 1
    AbsGrammar.BaseType_char -> 1
    AbsGrammar.BaseType_integer -> 4
    AbsGrammar.BaseType_real -> 4
    AbsGrammar.BaseType_string -> 4
    _ -> error "TODO: invalid type for sizeof"


sizeof (AbsGrammar.TypeCompType cType) = case cType of
    AbsGrammar.Array (TokInteger (_, start)) (TokInteger (_, end)) tp -> ((read end :: Int) - (read start :: Int)) * sizeof tp 
    AbsGrammar.Pointer tp-> sizeof tp


getArrayRange :: AbsGrammar.CompType -> (TACLiteral, TACLiteral)
getArrayRange (AbsGrammar.Array (TokInteger (_, l_tok)) (TokInteger (_, r_tok)) _) =
    (TACIntLit l_end, TACIntLit r_end)
    where
        l_end = read l_tok :: Int
        r_end = read r_tok :: Int