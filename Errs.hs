module Errs where
import AbsGrammar (Position, EXPR)


-- ERROR/WARNING name at (Pos)-(Pos): "description"

-- put $ state {errors = ReturnInMain (show posEnds) id : (errors state)} 

data Error = 
    ReturnInMain |
    MissingReturnInFunction Position String |
    UnexpectedReturnInProcedure Position String |
    TypeMissmatchIter String String String | -- posEnds expressionString typeString
    TypeMissmatchSelection String String String | -- posEnds expressionString typeString
    TypeMissmatchReturn String String String String |  -- posEnds funName expecteTp actualTp
    ExpectedTypeNotFound String | -- posEnds
    InvalidLValueAssignment String String | -- posEnds exprStr
    TypeMissmatchArrayAssignment String String String String | -- posEnds expr expectedTp assignedTp
    TypeMissmatchLiteral String String String String | -- posEnds idVal expectedTp actualTp 
    UnknownIdentifier String String | -- posEnds idVal
    TypeMissmatchIdExprAssignment String String String String | -- posEnds idVal expectedTp assignedType
    TypeMissmatchExprExprAssignment String String String String | -- posEnds expr expectedTp assignedTp
    TypeMissmatchUnaryExpr String String | -- posEnds exprType
    TypeMissmatchArithmeticMinus String String | -- posEnds wrongTp
    TypeMissimatchReference String String | -- posEnds -exprType
    UnnecessaryCasting String String String | --posEnds from to
    ImplicitCasting String String String | --posEnds from to
    CallingConstant String String  | --posEnds idVal
    CallingVariable String String | --posEnds idVal
    NumOfArgsMissmatch String String String --posEnds fun/proc name

instance Show Error where
    show ReturnInMain = "ERROR ReturnInMain: cannot have a return statement in the main begin-end block"
    show (MissingReturnInFunction pos name) = "ERROR MissingReturnInFunction at " ++ show pos ++ ": " ++ name ++ " does not have a return statement or not all the paths return a value" 
    show (UnexpectedReturnInProcedure pos name) = "ERROR UnexpectedReturn at " ++ show pos ++ ": " ++ name ++ " is a procedure so it cannot have a return statement. Transofrm it into a function or delete the return statement"
    show (TypeMissmatchIter posStr exprStr tpStr) = "ERROR TypeMissmatchIter at " ++ posStr ++ ": condition " ++ exprStr ++ " must be of type boolean but it is of type " ++ tpStr
    show (TypeMissmatchSelection posStr exprStr tpStr) = "ERROR TypeMissmatchSelection at " ++ posStr ++ ": condition " ++ exprStr ++ " must be of type boolean but it is of type " ++ tpStr
    show (TypeMissmatchReturn posStr funName tpExpected toActual) = "ERROR: TypeMissmatchReturn at " ++ posStr ++ ": function " ++ funName ++ " must return a value of type " ++ tpExpected ++ " but it returns a value of type " ++ toActual
    show (ExpectedTypeNotFound posStr) = "ERROR ExpectedTypeNotFound at " ++ posStr ++ ": can't find expected return type of current function in the environment while parsing the expression following the return"
    show (InvalidLValueAssignment posStr exprStr) = "ERROR InvalidLValueAssignment at " ++ posStr ++ ": expression " ++ exprStr ++ " is not a valid l-value for the assignment"
    show (TypeMissmatchArrayAssignment posStr exprStr tpExpected tpAssigned) = "ERROR TypeMissmatchArrayAssignment at " ++ posStr ++ ": l-expression " ++ exprStr ++ " is of type " ++ tpAssigned ++ " but it is assigned a value of type " ++ tpExpected
    show (TypeMissmatchLiteral posStr idVal tpExpected tpActual) = "ERROR TypeMissmatchLiteral at " ++ posStr ++ ": " ++ idVal ++ " is of type " ++ tpActual ++ " but it is assigned a value of type " ++ tpExpected
    show (UnknownIdentifier posStr idVal) = "ERROR UnknownIdentifier at " ++ posStr ++ ": " ++ idVal ++ " is not defined in the current scope"
    show (TypeMissmatchIdExprAssignment posStr idVal tpExpected tpAssigned) = "ERROR TypeMissmatchIdExprAssignment at " ++ posStr ++ ": " ++ idVal ++ " is of type " ++ tpExpected ++ " but it is assigned a value of type " ++ tpAssigned
    -- same error text as TypeMissmatchArrayAssignment
    show (TypeMissmatchExprExprAssignment posStr exprStr tpExpected tpAssigned) = "ERROR: TypeMissmatchExprExprAssignment at " ++ posStr ++ ": l-expression " ++ exprStr ++ " is of type " ++ tpAssigned ++ " but it is assigned a value of type " ++ tpExpected 
    show (TypeMissmatchUnaryExpr posStr exprStr) = "ERROR TypeMissmatchUnaryExpr at " ++ posStr ++ ": boolean negation 'not' can only be applied to boolean expressions but it is applied to an expression of type " ++ exprStr
    show (TypeMissmatchArithmeticMinus posStr wrongTp) = "ERROR TypeMissmatchArithmeticMinus at " ++ posStr ++ ":  arithmetic unary minus '-' applied to type " ++ wrongTp ++ " but it can only be applied to numeric types"
    show (TypeMissimatchReference posStr exprType) = "ERROR TypeMissimatchReference at " ++ posStr ++ ": invalid reference '@' operation on type " ++ exprType
    show (UnnecessaryCasting posStr from to) = "WARNING at " ++ posStr ++ ": removed unnecessary implicit type casting from " ++ from ++ " to " ++ to
    show (ImplicitCasting posStr from to) = "WARNING at " ++ posStr ++ ": type casting from " ++ from ++ " to " ++ to ++ " is done implicitly"
    show (CallingConstant posStr idVal) = "ERROR CallingConstant at " ++ posStr ++ ": identifier " ++ idVal ++ " is used as a function/procedure but it is a constant"
    -- could be merged with the previous one
    show (CallingVariable posStr idVal) = "ERROR CallingVariable at " ++ posStr ++ ": identifier " ++ idVal ++ " is used as a function/procedure but it is a variable"
    show (NumOfArgsMissmatch posStr funOrProc name) = "ERROR NumOfArgsMissmatch at " ++ posStr ++ ": " ++ funOrProc ++ " " ++ name ++ " is called with a wrong number of arguments"