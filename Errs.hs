module Errs where
import AbsGrammar (Position, EXPR)


-- ERROR/WARNING name at (Pos)-(Pos): "description"

type Problem = (ProblemHeader, ProblemBody)

data ProblemHeader = Error | Warning deriving (Eq, Show)

data ProblemBody = 
    ReturnInMain |
    MissingReturnInFunction Position String |
    UnexpectedReturnInProcedure Position String |
    TypeMismatchIter String String String | -- posEnds expressionString typeString
    TypeMismatchSelection String String String | -- posEnds expressionString typeString
    TypeMismatchReturn String String String String |  -- posEnds funName expecteTp actualTp
    ExpectedTypeNotFound String | -- posEnds
    InvalidLValueAssignment String String | -- posEnds exprStr
    TypeMismatchArrayAssignment String String String String | -- posEnds expr expectedTp assignedTp
    TypeMismatchLiteral String String String String String | -- posEnds idVal expr expectedTp actualTp 
    UnknownIdentifier String String | -- posEnds idVal
    TypeMismatchIdExprAssignment String String String String | -- posEnds idVal expectedTp assignedType
    TypeMismatchExprExprAssignment String String String String | -- posEnds expr expectedTp assignedTp
    TypeMismatchUnaryExpr String String | -- posEnds exprType
    TypeMismatchArithmeticMinus String String | -- posEnds wrongTp
    TypeMismatchReference String String | -- posEnds -exprType
    CallingConstant String String  | --posEnds idVal
    CallingVariable String String | --posEnds idVal
    NumOfArgsMismatch String String String | --posEnds fun/proc name
    TypeMismatchArgument String String String String String String | --posEnds argExpr argTp expectedTp paramId paramPos
    TypeMismatchBinaryExpr String String String String String String | --posEnds first/second expr operatorStr actualTp expectedTp
    TypeIncompatibleBinaryExpr String String String String String String | --posEnds operatorStr exp1 exp2 typeexp1 typeexp2
    TypeMismatchArrayIndex String String | -- posEnds exprTp
    TypeMismatchNotArray String String String |  -- posEnds expr exprTp
    UninitializedVariable String String | -- posEnds idVal
    InvalidLValueReferenceArg String String String String | -- posEnds parid parPosEnds argExpr 
    TypeMismatchReferenceArg String String String String String String | -- posEnds argExpr argTp expectedTp parid parPosEnds
    TypeMismatchPointer String String String  | -- posEnds expr actualTp
    InvalidLExpressionDereference String String | -- posEnds expr
    TypeMismatchBooleanOperator String String String String String | -- posEnds operatorStr leftOrRight exprStr exprType
    TypeErrorConditionSelectionExpression String String String| --posEnds exprcond condtype
    TypeMismatchSelectionExpression String String String String String | --posEnds expr1 expr2 type1 type2
    DuplicateDeclaration String String String | -- posEnds tipoDuplicazione nomeIdDuplicato
    BreakOutsideLoop | -- TODO: add pos info
    ContinueOutsideLoop -- TODO: add pos info
    | UnnecessaryCasting String String String --posEnds from to
    | ImplicitCasting String String String --posEnds from to
    | ForLoopInvalidCounterType String String String -- pos id actualTp
    | ForLoopInvalidLimitType String String String -- posEnds
    | ForLoopInvalidCounterTypeAndLimit String String String String String String -- counterPos counterId counterTp exprPos expr exprType
    | UNIMPLEMENTED_ERROR -- used as a placeholder for errors that we have not implemented yet


instance Show ProblemBody where
    show ReturnInMain = "ERROR ReturnInMain: cannot have a return statement in the main begin-end block"
    show (MissingReturnInFunction pos name) = "ERROR MissingReturnInFunction at " ++ show pos ++ ": " ++ name ++ " does not have a return statement or not all the paths return a value" 
    show (UnexpectedReturnInProcedure pos name) = "ERROR UnexpectedReturn at " ++ show pos ++ ": " ++ name ++ " is a procedure so it cannot have a return statement. Transofrm it into a function or delete the return statement"
    show (TypeMismatchIter posStr exprStr tpStr) = "ERROR TypeMismatchIter at " ++ posStr ++ ": condition " ++ exprStr ++ " must be of type Boolean but it is of type " ++ tpStr
    show (TypeMismatchSelection posStr exprStr tpStr) = "ERROR TypeMismatchSelection at " ++ posStr ++ ": condition " ++ exprStr ++ " must be of type Boolean but it is of type " ++ tpStr
    show (TypeMismatchReturn posStr funName tpExpected toActual) = "ERROR TypeMismatchReturn at " ++ posStr ++ ": function " ++ funName ++ " must return a value of type " ++ tpExpected ++ " but it returns a value of type " ++ toActual
    show (ExpectedTypeNotFound posStr) = "ERROR ExpectedTypeNotFound at " ++ posStr ++ ": can't find expected return type of current function in the environment while parsing the expression following the return"
    show (InvalidLValueAssignment posStr exprStr) = "ERROR InvalidLValueAssignment at " ++ posStr ++ ": expression " ++ exprStr ++ " is not a valid l-value for the assignment"
    show (TypeMismatchArrayAssignment posStr exprStr tpExpected tpAssigned) = "ERROR TypeMismatchArrayAssignment at " ++ posStr ++ ": l-expression " ++ exprStr ++ " is of type " ++ tpAssigned ++ " but it is assigned a value of type " ++ tpExpected
    show (TypeMismatchLiteral posStr idVal expr tpExpr tpActual) = "ERROR TypeMismatchLiteral at " ++ posStr ++ ": " ++ idVal ++ " is of type " ++ tpActual ++ " but it is assigned value " ++ expr ++ " of type " ++ tpExpr
    show (UnknownIdentifier posStr idVal) = "ERROR UnknownIdentifier at " ++ posStr ++ ": " ++ idVal ++ " is not defined in the current scope"
    show (TypeMismatchIdExprAssignment posStr idVal tpExpected tpAssigned) = "ERROR TypeMismatchIdExprAssignment at " ++ posStr ++ ": " ++ idVal ++ " is of type " ++ tpExpected ++ " but it is assigned a value of type " ++ tpAssigned
    show (TypeMismatchExprExprAssignment posStr exprStr tpExpected tpAssigned) = "ERROR: TypeMismatchExprExprAssignment at " ++ posStr ++ ": l-expression " ++ exprStr ++ " is of type " ++ tpAssigned ++ " but it is assigned a value of type " ++ tpExpected 
    show (TypeMismatchUnaryExpr posStr exprStr) = "ERROR TypeMismatchUnaryExpr at " ++ posStr ++ ": boolean negation 'not' can only be applied to boolean expressions but it is applied to an expression of type " ++ exprStr
    show (TypeMismatchArithmeticMinus posStr wrongTp) = "ERROR TypeMismatchArithmeticMinus at " ++ posStr ++ ":  arithmetic unary minus '-' applied to type " ++ wrongTp ++ " but it can only be applied to numeric types"
    show (TypeMismatchReference posStr exprType) = "ERROR TypeMismatchReference at " ++ posStr ++ ": invalid reference '@' operation on type " ++ exprType
    show (CallingConstant posStr idVal) = "ERROR CallingConstant at " ++ posStr ++ ": identifier " ++ idVal ++ " is used as a function/procedure but it is a constant"
    show (CallingVariable posStr idVal) = "ERROR CallingVariable at " ++ posStr ++ ": identifier " ++ idVal ++ " is used as a function/procedure but it is a variable"
    show (NumOfArgsMismatch posStr funOrProc name) = "ERROR NumOfArgsMismatch at " ++ posStr ++ ": " ++ funOrProc ++ " " ++ name ++ " is called with a wrong number of arguments"
    show (TypeMismatchArgument posStr argExpr argTp expectedTp paramId paramPos) = "ERROR TypeMismatchArgument at " ++ posStr ++ ": argument " ++ argExpr ++ " is of type " ++ argTp ++ " but it is passed to parameter " ++ paramId ++ " of type " ++ expectedTp ++ " at " ++ paramPos
    show (TypeMismatchBinaryExpr posStr firstOrSecond expr operatorStr actualTp expectedTp) = "ERROR TypeMismatchBinaryExpr at " ++ posStr ++ ": " ++ firstOrSecond ++ " operand " ++ expr ++ " of operator " ++ operatorStr ++ " is of type " ++ actualTp ++ " but it should be of type " ++ expectedTp
    show (TypeIncompatibleBinaryExpr posStr operatorStr exp1 exp2 type1 type2) = "ERROR TypeIncompatibleBinaryExpr at " ++ posStr ++ ": operands " ++ exp1++ " and "++  exp2 ++" of operator " ++ operatorStr ++ " are not compatible, as they are respectively of types " ++ type1 ++ " and " ++ type2
    show (TypeMismatchArrayIndex posStr exprTp) = "ERROR TypeMismatchArrayIndex at " ++ posStr ++ ": array index must be of numeric type but it is of type " ++ exprTp
    show (TypeMismatchNotArray posStr expr exprTp) = "ERROR TypeMismatchNotArray at " ++ posStr ++ ": expression " ++ expr ++ " is treated as an array but it is of type " ++ exprTp
    show (UninitializedVariable posStr idVal) = "ERROR UninitializedVariable at " ++ posStr ++ ": variable " ++ idVal ++ " has never been initialized"
    show (InvalidLValueReferenceArg posEnds parId parPosEnds argExpr) = "ERROR InvalidLValueReferenceArg at " ++ posEnds ++ ": parameter " ++ parId ++ " is passed by reference as specified at " ++ parPosEnds ++", but argument " ++ argExpr ++ " is not a valid l-value."
    show (TypeMismatchReferenceArg  posEnds argExpr argTp expectedTp parid parPosEnds) = "ERROR TypeMismatchReferenceArg at " ++ posEnds ++ ": the argument " ++ argExpr ++ " is of type " ++ argTp ++ " but it should be of type " ++ expectedTp ++ " as specified by parameter " ++ parid ++ " passed by reference at " ++ parPosEnds
    show (TypeMismatchPointer posEnds expr actualTp) = "ERROR TypeMismatchPointer at " ++ posEnds ++ ": expression " ++ expr ++ " should be of type pointer but it is of type " ++ actualTp
    show (InvalidLExpressionDereference posEnds expr) = "ERROR InvalidLExpressionDereference at " ++ posEnds ++ ": expression " ++ expr ++ " is not a valid l-expression for the dereference '^' operator"
    show (TypeMismatchBooleanOperator posEnds operatorStr leftOrRight exprStr exprType) = "ERROR TypeMismatchBooleanOperator at " ++ posEnds ++ ": cannot apply boolean operator " ++ operatorStr ++ " because " ++ leftOrRight ++ " expression " ++ exprStr ++ " is of type " ++ exprType ++ " instead of Boolean"
    show (TypeErrorConditionSelectionExpression posEnds exprcond condtype) = "ERROR TypeErrorConditionSelectionExpression at "++ posEnds ++": condition "++exprcond++" of expression selection is of type "++ condtype ++ " but it should be Boolean"
    show (TypeMismatchSelectionExpression posEnds expr1 expr2 type1 type2) = "ERROR TypeMismatchSelectionExpression at "++posEnds++": the types of the two branches of expression selection are not matching, as the expressions "++ expr1 ++ " and " ++ expr2 ++ " are respectively of types " ++ type1 ++ " and " ++ type2
    show (DuplicateDeclaration posEnds kind id) = "ERROR DuplicateDeclaration at "++ posEnds ++" : "++ kind ++" "++ id ++" has been declared multiple times"
    show BreakOutsideLoop = "ERROR BreakOutsideLoop at (TODO:implement pos for break stmt): break statemets are allowed only inside while-do and repeat-until loops"
    show ContinueOutsideLoop = "ERROR ContinueOutsideLoop at (TODO:implement pos for break stmt): continue statemets are allowed only inside while-do and repeat-until loops"
    show (UnnecessaryCasting posStr from to) = "WARNING UnnecessaryCasting at " ++ posStr ++ ": removed unnecessary implicit type casting from " ++ from ++ " to " ++ to
    show (ImplicitCasting posStr from to) = "WARNING ImplicitCasting at " ++ posStr ++ ": type casting from " ++ from ++ " to " ++ to ++ " is done implicitly"
    show (ForLoopInvalidCounterType pos id actualTp) = "ERROR ForLoopInvalidCounterType at " ++ pos ++ ": counter " ++ id ++ " is of type " ++ actualTp ++ " but it should be of type integer"
    show (ForLoopInvalidLimitType posEnds strExpr actualTp) = "ERROR ForLoopInvalidLimitType at  " ++ posEnds ++ ": expression " ++ strExpr ++ " is of type " ++ actualTp ++ " but it should be of type integer"
    show (ForLoopInvalidCounterTypeAndLimit counterPos counterId counterTp exprPos expr exprType) = "ERROR ForLoopInvalidCounterTypeAndLimit: counter " ++ counterId ++ " at " ++ counterPos ++ " is of type " ++ counterTp ++ " and expression " ++ expr ++ " at " ++ exprPos ++ " is of type " ++ exprType ++ " but they should both be of type integer"

-- returns the list with all the error string messages
getErrors :: [Problem] -> [String]
getErrors [] = []
getErrors ((Error, body):xs) = show body : getErrors xs
getErrors (x:xs) = getErrors xs -- do not include warnings

-- returns the list with all the warning string messages
getWarnings :: [Problem] -> [String]
getWarnings [] = []
getWarnings ((Warning, body):xs) = show body : getWarnings xs
getWarnings (x:xs) = getWarnings xs -- do not include errors
