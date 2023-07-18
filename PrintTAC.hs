module PrintTAC where

import HelperTAC
import AbsGrammar (Literal(LiteralInteger), TokInteger(TokInteger))
import qualified Data.Sequence as DS
import Text.Printf



instance Show TACQuad where
    show (TACQuad labels (Just TACPCl) (Just pName) (Just nPrm) Nothing) = printf "%-10s %-30s\n" (show labels) (show TACPCl ++ show pName ++ "," ++ show nPrm)
    show (TACQuad labels (Just TACFCl) (Just fName) (Just nPrm) (Just res)) = printf "%-10s %-30s\n" (show labels) (show res ++ " = " ++ show TACPCl ++ show fName ++ "," ++ show nPrm)
    show (TACQuad labels (Just TACPrm) Nothing Nothing (Just prm)) = printf "%-10s %-30s\n" (show labels) (show TACPrm ++ show prm)
    show (TACQuad labels (Just TACRt) Nothing Nothing (Just rtVal)) = printf "%-10s %-30s\n\n" (show labels) (show TACRt ++ show rtVal)
    show (TACQuad labels (Just TACRt) Nothing Nothing Nothing) = printf "%-10s %-30s\n\n" (show labels) (show TACRt)
    show (TACQuad labels (Just TACDeref) (Just op) Nothing (Just res)) = printf "%-10s %-30s\n" (show labels) (show res ++ " = " ++ show TACDeref ++ show op)
    show (TACQuad labels (Just TACRef) (Just op) Nothing (Just res)) = printf "%-10s %-30s\n" (show labels) (show res ++ " = " ++ show TACRef ++ show op)
    show (TACQuad labels (Just TACDeref) Nothing (Just op) (Just res)) = printf "%-10s %-30s\n" (show labels) (show TACDeref ++ show res ++ " = " ++  show op)
    show (TACQuad labels (Just opr) (Just op1) Nothing (Just res)) = printf "%-10s %-30s\n" (show labels) (show res ++ " = " ++show opr ++ show op1)
    show (TACQuad labels Nothing (Just op1) Nothing (Just res)) = printf "%-10s %-30s\n" (show labels) (show res ++ " = " ++ show op1)
    show (TACQuad labels Nothing Nothing Nothing (Just labAddr)) = printf "%-10s %-30s\n" (show labels) ("goto " ++ show labAddr)
    show (TACQuad labels (Just TACIdxL) (Just src) (Just indx) (Just dst)) = printf "%-10s %-30s\n" (show labels) (show dst ++ " = " ++ show src ++ "[" ++ show indx ++ "]")
    show (TACQuad labels (Just TACIdxS) (Just src) (Just indx) (Just dst)) = printf "%-10s %-30s\n" (show labels) (show dst ++  "[" ++ show indx ++ "] = " ++ show src)
    show (TACQuad labels (Just opr) (Just op1) (Just op2) (Just res)) = if isCndJmpOp opr
        then printf "%-10s %-30s\n" (show labels) ("if " ++ show op1 ++ show opr ++ show op2 ++ " " ++ show TACJmp ++ show res)
        else printf "%-10s %-30s\n" (show labels) (show res ++ " = " ++ show op1 ++ show opr ++ show op2)
    show _ = " ![TODO QUAD]! \n"

instance Show AddrList where
    show (AddrList (addr:[])) = show addr ++ ":"
    show (AddrList (addr:rest)) = show addr ++ ","++ show (AddrList rest)
    show (AddrList []) = ""

instance Show Addr where
    show (ProgVar var) = var
    show (TacLit lit) = show lit
    show (Temporary tmp) = tmp

instance Show TACLiteral where
    show (TACIntLit v) = show v 
    show (TACCharLit v) = show v
    show (TACBoolLit v) = show v 
    show (TACRealLit v) = show v 
    --show (TACString v) = v 

instance Show TACOp where
    show TACAddInt = " +int "
    show TACAddFloat = " +float "
    show TACMulInt = " *int "
    show TACMulFloat = " *float "
    show TACDivInt = " /int "
    show TACDivFloat = " /float "
    show TACSubInt = " -int "
    show TACSubFloat = " -float "
    show TACEqInt = " ==int "
    show TACEqFloat = " ==float "
    show TACEqChar = " ==char "
    show TACEqBool = " ==bool "
    show TACJmpEqInt = " ==int "
    show TACJmpEqFloat = " ==float "
    show TACJmpEqChar = " ==char "
    show TACJmpEqBool = " ==bool "
    show TACNotEqInt = " !=int "
    show TACNotEqFloat = " !=float "
    show TACNotEqChar = " !=char "
    show TACNotEqBool = " !=bool "
    show TACJmpNotEqInt = " !=int "
    show TACJmpNotEqFloat = " !=float "
    show TACJmpNotEqChar = " !=char "
    show TACJmpNotEqBool = " !=bool "
    show TACLessTInt = " <int "
    show TACLessTFloat = " <float "
    show TACJmpLessTInt = " <int "
    show TACJmpLessTFloat = " <float "
    show TACGreatTInt = " >int "
    show TACGreatTFloat = " >float "
    show TACJmpGreatTInt = " >int "
    show TACJmpGreatTFloat = " >float "
    show TACEqGreatTInt = " >=int "
    show TACEqGreatTFloat = " >=float "
    show TACJmpEqGreatTInt = " >=int "
    show TACJmpEqGreatTFloat = " >=float "
    show TACEqLessTInt = " <=int "
    show TACEqLessTFloat = " <=float "
    show TACJmpEqLessTInt = " <=int "
    show TACJmpEqLessTFloat = " <=float "
    show TACJmp = "goto "
    show TACNot = "!"
    show TACAnd = " && "
    show TACOr = " || "
    show TACPrm = "param "
    show TACRt = "return "
    --show TACRtVd = "return "
    show TACPCl = "pcall "
    show TACFCl = "fcall "
    show TACNegInt = "-int"
    show TACNegFloat = "-float"
    show TACDeref = "*"
    show TACRef = "&"
    show TACCastIntToReal = "(int_to_real)"

    show _ =  " ![TODO OP]! "

instance Show TACQuadSeq where
    show (TACQuadSeq (quad DS.:<| seq)) = show quad ++ show (TACQuadSeq seq)
    show _ = ""