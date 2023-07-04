import AbsGrammar
import TypeChecker
import Env

ass = [StmtAssign (BaseExpr (Identifier (TokIdent ((6,3),"a"))) ()) (BinaryExpression {operator2 = Add, exp1 = BinaryExpression {operator2 = Mul, exp1 = ExprLiteral (LiteralDouble (TokDouble ((6,8),"2.5555"))), exp2 = ExprLiteral (LiteralInteger (TokInteger ((6,17),"3"))), tp = ()}, exp2 = ExprLiteral (LiteralInteger (TokInteger ((6,21),"5"))), tp = ()})]
ass2 = [StmtAssign (BaseExpr (Identifier (TokIdent ((6,3),"a"))) ()) (BinaryExpression {operator2 = Mul, exp1 = ExprLiteral (LiteralDouble (TokDouble ((6,8),"2.5555"))), exp2 = ExprLiteral (LiteralInteger (TokInteger ((6,17),"3"))), tp = ()})]
ass3 = [StmtAssign (BaseExpr (Identifier (TokIdent ((6,3),"a"))) ()) (ExprLiteral (LiteralDouble (TokDouble ((6,8),"2.5555"))))]
id2 = (TokIdent ((6,3),"a"))
lit2 = (LiteralDouble (TokDouble ((6,8),"2.5555")))
env2 = populateEnv [((6,3), "a")] (TypeBaseType BaseType_real) emptyEnv