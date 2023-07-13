module PrintTAC where

import HelperTAC
import AbsGrammar (Literal(LiteralInteger), TokInteger(TokInteger))
import qualified Data.Sequence as DS


instance Show TACQuad where
    show (TACQuad labels (Just TACPCl) (Just pName) (Just nPrm) Nothing) = show labels ++ show TACPCl ++ show pName ++ "," ++ show nPrm ++"\n"
    show (TACQuad labels (Just TACFCl) (Just fName) (Just nPrm) (Just res)) = show labels ++ show res ++ " = " ++ show TACPCl ++ show fName ++ "," ++ show nPrm ++"\n"
    show (TACQuad labels (Just TACPrm) Nothing Nothing (Just prm)) = show labels ++ show TACPrm ++ show prm ++ "\n"
    show (TACQuad labels (Just TACRt) Nothing Nothing (Just rtVal)) = show labels ++ show TACRt ++ show rtVal ++ "\n"
    show (TACQuad labels (Just TACDeref) (Just op) Nothing (Just res)) = show labels ++ show res ++ " = " ++ show TACDeref ++ show op ++ "\n"
    show (TACQuad labels (Just TACRef) (Just op) Nothing (Just res)) = show labels ++ show res ++ " = " ++ show TACRef ++ show op ++ "\n"
    show (TACQuad labels (Just TACDeref) Nothing (Just op) (Just res)) = show labels ++ show TACDeref ++ show res ++ " = " ++  show op ++ "\n"
    show (TACQuad labels (Just opr) (Just op1) Nothing (Just res)) = show labels ++ show res ++ " = " ++show opr ++ show op1 ++ "\n"
    show (TACQuad labels Nothing (Just op1) Nothing (Just res)) = show labels ++ show res ++ " = " ++ show op1 ++ "\n"
    show (TACQuad labels Nothing Nothing Nothing (Just labAddr)) = show labels ++ "goto " ++ show labAddr ++ "\n"
    show (TACQuad labels (Just TACIdxL) (Just src) (Just indx) (Just dst)) = show labels ++ show dst ++ " = " ++ show src ++ "[" ++ show indx ++ "]" ++ "\n"
    show (TACQuad labels (Just TACIdxS) (Just src) (Just indx) (Just dst)) = show labels ++ show dst ++  "[" ++ show indx ++ "] = " ++ show src ++ "\n"
    show (TACQuad labels (Just opr) (Just op1) (Just op2) (Just res)) = if isCndJmpOp opr
        then show labels ++ "if " ++ show op1 ++ show opr ++ show op2 ++ " " ++ show TACJmp ++ show res ++ "\n"
        else show labels ++ show res ++ " = " ++ show op1 ++ show opr ++ show op2 ++ "\n"
    show _ = " ![TODO QUAD]! "

instance Show AddrList where
    show (AddrList (addr:[])) = show addr ++ " :\t"
    show (AddrList (addr:rest)) = show addr ++ ", "++ show rest
    show (AddrList []) = ""

instance Show Addr where
    show (ProgVar var) = var
    show (TacLit lit) = show lit
    show (Temporary tmp) = tmp

instance Show TACLiteral where
    show (TACInt v) = show v 
    show (TACChar v) = show v
    show (TACBool v) = show v 
    show (TACReal v) = show v 
    show (TACString v) = v 

instance Show TACOp where
    show TACAdd = " + "
    show TACMul = " * "
    show TACDiv = " / "
    show TACSub = " - "
    show TACEq = " == "
    show TACJmpEq = " == "
    show TACNotEq = " != "
    show TACJmpNotEq = " != "
    show TACLessT = " < "
    show TACJmpLessT = " < "
    show TACGreatT = " > "
    show TACJmpGreatT = " > "
    show TACEqGreatT = " >= "
    show TACJmpEqGreatT = " >= "
    show TACEqLessT = " <= "
    show TACJmpEqLessT = " <= "
    show TACJmp = "goto "
    show TACNot = "!"
    show TACAnd = " && "
    show TACOr = " || "
    show TACPrm = "param "
    show TACRt = "return "
    --show TACRtVd = "return "
    show TACPCl = "pcall "
    show TACFCl = "fcall "
    show TACNeg = "-"
    show TACDeref = "*"
    show TACRef = "&"
    show (TACCast tp) = "("++ show tp ++")"
    show _ =  " ![TODO OP]! "

instance Show TACQuadSeq where
    show (TACQuadSeq (quad DS.:<| seq)) = show quad ++ show (TACQuadSeq seq)
    show _ = ""