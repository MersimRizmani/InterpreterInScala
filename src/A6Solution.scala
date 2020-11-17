
class A6Solution {

  def evaluate(c: Expression): Value = {
    c.getClass.getSimpleName match {
      case "IntConstant" =>
        val cst: IntConstant = c.asInstanceOf[IntConstant]
        val ret: IntValue = new IntValue(cst.c)
        ret
      case "BooleanConstant" =>
        val cst: BooleanConstant = c.asInstanceOf[BooleanConstant]
        val ret: BooleanValue = new BooleanValue(cst.c)
        ret
      case "BinaryOperationExpression" =>
        val op: BinaryOperationExpression = c.asInstanceOf[BinaryOperationExpression]
        val leftValue: IntValue = evaluate(op.left).asInstanceOf[IntValue]
        val rightValue: IntValue = evaluate(op.right).asInstanceOf[IntValue]

        op.oper match {
          case Operator.PLUS => new IntValue(leftValue.v + rightValue.v)
          case Operator.MINUS =>
          case Operator.TIMES =>
          case Operator.DIV =>
          case _ => throw new Error("Unknown binary operator: ")
        }

      case _ => throw new Error("Unknown expression: " + c.getClass.getSimpleName)
    }
  }

}

object Main {
  def main(args: Array[String]): Unit = {
    println("Hello, world!")
  }
}

abstract class Expression{}

object Operator extends Enumeration {
  type Operator = Value
  val PLUS, MINUS, TIMES, DIV = Value
}

class BinaryOperationExpression(val oper: Operator.Operator, val left: Expression, val right: Expression) extends Expression{}

class IntConstant(val c: Int) extends Expression {}

class BooleanConstant(val c: Boolean) extends Expression {}

abstract class Value {}

class IntValue(v: Int) extends Value {}

class BooleanValue(b: Boolean) extends Value {}

