import AbsGrammar
import TypeChecker
import Env
import GeneratorTAC
import Control.Monad.State.Lazy


-- -- USAGE EXAMPLE
-- -- 1. Define an environment for the test
-- myEnv = Env.insert "a" (VarType (0,0) (TypeBaseType BaseType_integer)) emptyEnv
-- -- 2. Define the expression to test
-- myExpr = UnaryExpression {operator1 = Negation, AbsGrammar.exp = BaseExpr (Identifier (TokIdent ((6,3),"a"))) (TypeBaseType BaseType_integer), tp = TypeBaseType BaseType_integer}
-- -- 3. Call the testExpr function with the genUnrExpr function, the expression, and the environment
-- testResult = testExpr genUnrExpr myExpr myEnv

-- -- 4. Execute following commands: 
-- -- ghci TestHelper.hs 
-- -- ghci > testResult

ass = [StmtAssign (BaseExpr (Identifier (TokIdent ((6,3),"a"))) ()) (BinaryExpression {operator2 = Add, exp1 = BinaryExpression {operator2 = Mul, exp1 = ExprLiteral (LiteralDouble (TokDouble ((6,8),"2.5555"))), exp2 = ExprLiteral (LiteralInteger (TokInteger ((6,17),"3"))), tp = ()}, exp2 = ExprLiteral (LiteralInteger (TokInteger ((6,21),"5"))), tp = ()})]
ass2 = [StmtAssign (BaseExpr (Identifier (TokIdent ((6,3),"a"))) ()) (BinaryExpression {operator2 = Mul, exp1 = ExprLiteral (LiteralDouble (TokDouble ((6,8),"2.5555"))), exp2 = ExprLiteral (LiteralInteger (TokInteger ((6,17),"3"))), tp = ()})]
ass3 = [StmtAssign (BaseExpr (Identifier (TokIdent ((6,3),"a"))) ()) (ExprLiteral (LiteralDouble (TokDouble ((6,8),"2.5555"))))]
be1 = (BinaryExpression {operator2 = Mul, exp1 = ExprLiteral (LiteralDouble (TokDouble ((6,8),"2.5555"))), exp2 = ExprLiteral (LiteralInteger (TokInteger ((6,17),"3"))), tp = ()})
be2Ann = (BinaryExpression {operator2 = Mul, exp1 = ExprLiteral (LiteralDouble (TokDouble ((6,8),"2.5555"))), exp2 = ExprLiteral (LiteralInteger (TokInteger ((6,17),"3"))), tp = TypeBaseType (BaseType_real)})
ue1 = (UnaryExpression {operator1 = Negation, AbsGrammar.exp = ExprLiteral (LiteralDouble (TokDouble ((6,8),"2.5555"))), tp = ()})
ue1Ann = (UnaryExpression {operator1 = Negation, AbsGrammar.exp = ExprLiteral (LiteralDouble (TokDouble ((6,8),"2.5555"))), tp = TypeBaseType (BaseType_real)}) 
baseExprIdNeg1Ann =  (UnaryExpression {operator1 = Negation, AbsGrammar.exp = BaseExpr (Identifier (TokIdent ((6,3),"a"))) (TypeBaseType (BaseType_integer)), tp = TypeBaseType (BaseType_integer)})
id2 = TokIdent ((6,3),"a")
lit2 = LiteralDouble (TokDouble ((6,8),"2.5555"))

-- il primo argomento è la funzione di genExpr da testare (e.g. genUnrExpr)
testExpr :: (AbsGrammar.EXPR AbsGrammar.Type -> Env -> StateTAC Addr) -> AbsGrammar.EXPR AbsGrammar.Type -> Env -> [TACInst]
testExpr f expr env = returnTAC $ execState (f expr env) (0, [])

returnTAC :: (Int, [TACInst]) -> [TACInst]
returnTAC (_, instrs) = instrs