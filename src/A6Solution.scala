import java.util.Objects

/*
 * Mersim Rizmani - CS 474
 * Assignment 6 - Interpreter in new language of choice
 * Chosen Language: Scala
 */

/* ========================= A6Solution CLASS ========================= */
class A6Solution {

  /* =========== EVALUATE METHOD ============= */
  def evaluate(c: Expression): Value = {
    c.getClass.getSimpleName match {
      case "IntConstant" =>
        val cst: IntConstant = c.asInstanceOf[IntConstant]
        new IntValue(cst.c)
      case "BooleanConstant" =>
        val cst: BooleanConstant = c.asInstanceOf[BooleanConstant]
        new BooleanValue(cst.c)
      case "BinaryOperationExpression" =>
        val op: BinaryOperationExpression = c.asInstanceOf[BinaryOperationExpression]
        val leftValue: IntValue = evaluate(op.left).asInstanceOf[IntValue]
        val rightValue: IntValue = evaluate(op.right).asInstanceOf[IntValue]

        op.oper match {
          case Operator.PLUS => new IntValue(leftValue.v + rightValue.v)
          case Operator.MINUS => new IntValue(leftValue.v - rightValue.v)
          case Operator.TIMES => new IntValue(leftValue.v * rightValue.v)
          case Operator.DIV => new IntValue(leftValue.v / rightValue.v)
          case _ => throw new Error("Unknown binary operator: ")
        }
      case "ComparisonExpression" =>
        val comp: ComparisonExpression = c.asInstanceOf[ComparisonExpression]
        val left: Value = evaluate(comp.left)
        val right: Value = evaluate(comp.right)

        comp.t match {
          case Type.EQ =>
            val result: Boolean = ((left.asInstanceOf[IntValue].v) == (right.asInstanceOf[IntValue].v))
            new BooleanValue(result)
          case _ => throw new Error("Unknown comparison type: " + comp.t)
        }
      case "IfExpression" =>
        val ife: IfExpression = c.asInstanceOf[IfExpression]
        val cond: BooleanValue = evaluate(ife.condition).asInstanceOf[BooleanValue]

        if(cond.b) {
          evaluate(ife.thenSide)
        }
        else{
          evaluate(ife.elseSide)
        }
      case "LetExpression" => null
      case "VariableExpression" => null
      case _ => throw new Error("Unknown expression: " + c.getClass.getSimpleName)
    }
  }
  /* =========== EVALUATE METHOD ENDS ============= */
}
/* ========================= END OF A6Solution CLASS ========================= */

/* ===================== PROGRAMS FOR TESTING INTERPRETER ====================== */
object Programs {
  /* 474 */
  val p1: Expression = new IntConstant(474)

  /* (400 + 74) / 3 */
  val p2: Expression = new BinaryOperationExpression(
    Operator.DIV,
    new BinaryOperationExpression(
      Operator.PLUS,
      new IntConstant(400),
      new IntConstant(74)
    ),
    new IntConstant(3)
  )

  /* ((400 + 74) / 3) == 158 */
  val p3: Expression = new ComparisonExpression(
    Type.EQ,
    new BinaryOperationExpression(
      Operator.DIV,
      new BinaryOperationExpression(
        Operator.PLUS,
        new IntConstant(400),
        new IntConstant(74)
      ),
      new IntConstant(3)
    ),
    new IntConstant(158)
  )

  /* if (((400 + 74) / 3) == 158)
        then 474
     else
        474 / 0
  */
  val p4: Expression = new IfExpression(
    new ComparisonExpression(
      Type.EQ,
      new BinaryOperationExpression(
        Operator.DIV,
        new BinaryOperationExpression(
          Operator.PLUS,
          new IntConstant(400),
          new IntConstant(74)
        ),
        new IntConstant(3)
      ),
      new IntConstant(158)
    ),
    new IntConstant(474),
    new BinaryOperationExpression(
      Operator.DIV,
      new IntConstant(474),
      new IntConstant(0)
    )
  )
}
/* ==================== PROGRAMS FOR TESTING INTERPRETER END ======================= */

/* ======================= MAIN METHOD ================================= */
object Main {
  def main(args: Array[String]): Unit = {
    println("Result of running p1: " + new A6Solution().evaluate(Programs.p1)) // expected : 474
    println("Result of running p2: " + new A6Solution().evaluate(Programs.p2)) // expected : 158
    println("Result of running p3: " + new A6Solution().evaluate(Programs.p3)) // expected : true
    println("Result of running p4: " + new A6Solution().evaluate(Programs.p4)) // expected : 474, no div by zero error
  }
}
/* ===================== MAIN METHOD ENDS ================================= */

/* ========================= EXPRESSION CLASSES ================================== */
abstract class Expression{}

object Operator extends Enumeration { type Operator = Value; val PLUS, MINUS, TIMES, DIV = Value }

class BinaryOperationExpression(val oper: Operator.Operator, val left: Expression, val right: Expression) extends Expression{}

class IntConstant(val c: Int) extends Expression {}

class BooleanConstant(val c: Boolean) extends Expression {}

class IfExpression(val condition: Expression, val thenSide: Expression, val elseSide: Expression) extends Expression {}

object Type extends Enumeration {type Type = Value; val EQ: Type.Value = Value}

class ComparisonExpression(val t: Type.Type, val left: Expression, val right: Expression) extends Expression {}

class LetExpression(val variable: Name, val value: Expression, val body: Expression) extends Expression {}

class VariableExpression(val variable: Name) extends Expression {}
/* ========================= EXPRESSION CLASSES END ================================== */

/* ============================= VALUE CLASSES ====================================== */
abstract class Value {}

class IntValue(val v: Int) extends Value { override def toString: String = "IntValue{" + "v=" + v + '}' }

class BooleanValue(val b: Boolean) extends Value { override def toString: String = "BooleanValue{" + "b=" + b + '}' }

class Name(val theName: String) {
  override def toString: String = theName

  override def equals(obj: Any): Boolean = {
    if(this == obj){ return true }
    if(obj == null || getClass != obj.getClass){ return false }
    val name: Name = obj.asInstanceOf[Name]
    Objects.equals(theName, name.theName)
  }

  override def hashCode(): Int = Objects.hash(theName)
}
/* ============================= VALUE CLASSES END ====================================== */

/* ============================= ENVIRONMENT CLASSES ========================================== */
class Binding(val name: Name, val value: Value) { override def toString: String = "{" + "name=" + name + ", value=" + value + '}' }

class List {}

abstract class Environment{}

class LexicalScopedEnvironment() extends Environment {}
/* ============================= ENVIRONMENT CLASSES END ========================================== */