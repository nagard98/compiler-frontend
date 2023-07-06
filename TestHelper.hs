import AbsGrammar
import TypeChecker
import Env
import GeneratorTAC
import Control.Monad.State.Lazy

ass = [StmtAssign (BaseExpr (Identifier (TokIdent ((6,3),"a"))) ()) (BinaryExpression {operator2 = Add, exp1 = BinaryExpression {operator2 = Mul, exp1 = ExprLiteral (LiteralDouble (TokDouble ((6,8),"2.5555"))), exp2 = ExprLiteral (LiteralInteger (TokInteger ((6,17),"3"))), tp = ()}, exp2 = ExprLiteral (LiteralInteger (TokInteger ((6,21),"5"))), tp = ()})]
assAnn = [StmtAssign (BaseExpr (Identifier (TokIdent ((6,3),"a"))) (TypeBaseType (BaseType_real))) (BinaryExpression {operator2 = Add, exp1 = BinaryExpression {operator2 = Mul, exp1 = ExprLiteral (LiteralDouble (TokDouble ((6,8),"2.5555"))), exp2 = ExprLiteral (LiteralInteger (TokInteger ((6,17),"3"))), tp = TypeBaseType (BaseType_real)}, exp2 = ExprLiteral (LiteralInteger (TokInteger ((6,21),"5"))), tp = TypeBaseType (BaseType_real)})]
ass2 = [StmtAssign (BaseExpr (Identifier (TokIdent ((6,3),"a"))) ()) (BinaryExpression {operator2 = Mul, exp1 = ExprLiteral (LiteralDouble (TokDouble ((6,8),"2.5555"))), exp2 = ExprLiteral (LiteralInteger (TokInteger ((6,17),"3"))), tp = ()})]
ass2Ann = [StmtAssign (BaseExpr (Identifier (TokIdent ((6,3),"a"))) (TypeBaseType (BaseType_real))) (BinaryExpression {operator2 = Mul, exp1 = ExprLiteral (LiteralDouble (TokDouble ((6,8),"2.5555"))), exp2 = ExprLiteral (LiteralInteger (TokInteger ((6,17),"3"))), tp = TypeBaseType (BaseType_real)})]
ass3 = [StmtAssign (BaseExpr (Identifier (TokIdent ((6,3),"a"))) ()) (ExprLiteral (LiteralDouble (TokDouble ((6,8),"2.5555"))))]
ass3Ann = [StmtAssign (BaseExpr (Identifier (TokIdent ((6,3),"a"))) (TypeBaseType (BaseType_real))) (ExprLiteral (LiteralDouble (TokDouble ((6,8),"2.5555"))))]
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

testStmt :: (AbsGrammar.Stmt Env AbsGrammar.Type -> Env -> StateTAC ()) -> AbsGrammar.Stmt Env AbsGrammar.Type -> Env -> [TACInst]
testStmt f stmt env = returnTAC $ execState (f stmt env) (0, [])

returnTAC :: (Int, [TACInst]) -> [TACInst]
returnTAC (_, instrs) = instrs