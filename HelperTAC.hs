module HelperTAC where
import qualified AbsGrammar
import Control.Monad.Trans.State
import qualified Data.Sequence as DS

data Addr =
      ProgVar { var :: String }
    | TacLit { tacLit :: AbsGrammar.Literal }
    | Temporary { tempInt :: String }
    deriving (Show)

type StateTAC = State (Int, DS.Seq TACInst)

data TACLabel =
      FuncLab
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
    | TACParam Addr
    | TACPCall Addr Int
    | TACFCall Addr Addr Int
    | TACReturnVoid
    | TACReturn Addr
    deriving (Show)


newTmpAddr :: StateTAC Addr
newTmpAddr = do
    (k, ls)<-get;
    put (k+1, ls);
    return (int2TmpName k)

int2TmpName :: Int -> Addr
int2TmpName k = Temporary ("t" ++ show k)

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
