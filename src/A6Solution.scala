import java.util.Objects

import scala.annotation.tailrec

/*
 * Mersim Rizmani - CS 474
 * Assignment 6 - Interpreter in new language of choice
 * Chosen Language: Scala
 */

/* ========================= A6Solution CLASS ========================= */
class A6Solution {

  /* =========== EVALUATE METHOD ============= */
  def evaluate(c: Expression, e: Environment, functions: List[Function]): Value = {
    c.getClass.getSimpleName match {
      case "IntConstant" =>
        val cst: IntConstant = c.asInstanceOf[IntConstant]
        new IntValue(cst.c)
      case "BooleanConstant" =>
        val cst: BooleanConstant = c.asInstanceOf[BooleanConstant]
        new BooleanValue(cst.c)
      case "BinaryOperationExpression" =>
        val op: BinaryOperationExpression = c.asInstanceOf[BinaryOperationExpression]
        val leftValue: IntValue = evaluate(op.left, e, functions).asInstanceOf[IntValue]
        val rightValue: IntValue = evaluate(op.right, e, functions).asInstanceOf[IntValue]

        op.oper match {
          case Operator.PLUS => new IntValue(leftValue.v + rightValue.v)
          case Operator.MINUS => new IntValue(leftValue.v - rightValue.v)
          case Operator.TIMES => new IntValue(leftValue.v * rightValue.v)
          case Operator.DIV => new IntValue(leftValue.v / rightValue.v)
          case _ => throw new Error("Unknown binary operator: ")
        }
      case "ComparisonExpression" =>
        val comp: ComparisonExpression = c.asInstanceOf[ComparisonExpression]
        val left: Value = evaluate(comp.left, e, functions)
        val right: Value = evaluate(comp.right, e, functions)

        comp.t match {
          case Type.EQ =>
            val result: Boolean = ((left.asInstanceOf[IntValue].v) == (right.asInstanceOf[IntValue].v))
            new BooleanValue(result)
          case _ => throw new Error("Unknown comparison type: " + comp.t)
        }
      case "IfExpression" =>
        val ife: IfExpression = c.asInstanceOf[IfExpression]
        val cond: BooleanValue = evaluate(ife.condition, e, functions).asInstanceOf[BooleanValue]

        if(cond.b) {
          evaluate(ife.thenSide, e, functions)
        }
        else{
          evaluate(ife.elseSide, e, functions)
        }
      case "LetExpression" =>
        val let: LetExpression = c.asInstanceOf[LetExpression]
        val v: Value = evaluate(let.value, e, functions)
        val newE: Environment = e.bind(let.variable, v)
        evaluate(let.body, newE, functions)
      case "VariableExpression" =>
        val varEx: VariableExpression = c.asInstanceOf[VariableExpression]
        e.lookup(varEx.variable)
      case "FunctionCallExpression" =>
        val call: FunctionCallExpression = c.asInstanceOf[FunctionCallExpression]
        val f: Function = findFunction(call.functionName, functions)

        var evaluationEnvironment: Environment = e

        for(i <- 0 until f.formalNamesOfArguments.length){
          val actualArgumentValue: Value = evaluate(call.actualArguments(i), e, functions)
          evaluationEnvironment = evaluationEnvironment.bind(f.formalNamesOfArguments(i), actualArgumentValue)
        }

        evaluate(f.body, evaluationEnvironment, functions)
      case _ => throw new Error("Unknown expression: " + c.getClass.getSimpleName)
    }
  }
  /* =========== EVALUATE METHOD ENDS ============= */

  def findFunction(name: Name, toSearch: List[Function]): Function = {
    if(toSearch == List.empty){
      throw new Error("No such function: " + name)
    }

    if(toSearch.head.name.equals(name)){
      toSearch.head
    }
    else {
      findFunction(name, toSearch.tail)
    }
  }
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

  /* let bot = 3 in
       (let bot = 2 in bot)
        +
       (if (bot == 0) then 474/0 else (400+74)/bot)
  */
  val p5: Expression = new LetExpression(
    new Name("bot"),
    new IntConstant(3),
    new BinaryOperationExpression(
      Operator.PLUS,
      new LetExpression(
        new Name("bot"),
        new IntConstant(2),
        new VariableExpression(new Name("bot"))
      ),
      new IfExpression(
        new ComparisonExpression(
          Type.EQ,
          new VariableExpression(new Name("bot")),
          new IntConstant(0)
        ),
        new BinaryOperationExpression(
          Operator.DIV,
          new IntConstant(474),
          new IntConstant(0)
        ),
        new BinaryOperationExpression(
          Operator.DIV,
          new BinaryOperationExpression(
            Operator.PLUS,
            new IntConstant(400),
            new IntConstant(74)
          ),
          new VariableExpression(new Name("bot"))
        )
      )
    )
  )

  /* function f(top,bot):
        if(bot==0) then 0 else top/bot
   */
  val f: Function = new Function(
    new Name("f"),
    new IfExpression(
      new ComparisonExpression(
        Type.EQ,
        new VariableExpression(new Name("bot")),
        new IntConstant(0)
      ),
      new IntConstant(0),
      new BinaryOperationExpression(
        Operator.DIV,
        new VariableExpression(new Name("top")),
        new VariableExpression(new Name("bot"))
      )
    ),
    new Name("top"),
    new Name("bot")
  )

  /* let bot = 3 in
       (let bot = 2 in bot)
        +
       (f(400+74,bot) + f(470+4,0))
  */
  val p6: Expression = new LetExpression(
    new Name("bot"),
    new IntConstant(3),
    new BinaryOperationExpression(
      Operator.PLUS,
      new LetExpression(
        new Name("bot"),
        new IntConstant(2),
        new VariableExpression(new Name("bot"))
      ),
      new BinaryOperationExpression(
        Operator.PLUS,
        new FunctionCallExpression(
          new Name("f"),
          new BinaryOperationExpression(
            Operator.PLUS,
            new IntConstant(400),
            new IntConstant(74)
          ),
          new VariableExpression(new Name("bot"))
        ),
        new FunctionCallExpression(
          new Name("f"),
          new BinaryOperationExpression(
            Operator.PLUS,
            new IntConstant(470),
            new IntConstant(4)
          ),
          new IntConstant(0)
        )
      )
    )
  )
}
/* ==================== PROGRAMS FOR TESTING INTERPRETER END ======================= */

/* ======================= MAIN METHOD ================================= */
object Main {
  def main(args: Array[String]): Unit = {
    val e: Environment = new LexicalScopedEnvironment()
    val funcs: List[Function] = List.empty
    println("Result of running p1: " + new A6Solution().evaluate(Programs.p1, e, Programs.f +: funcs)) // expected : 474
    println("Result of running p2: " + new A6Solution().evaluate(Programs.p2, e, Programs.f +: funcs)) // expected : 158
    println("Result of running p3: " + new A6Solution().evaluate(Programs.p3, e, Programs.f +: funcs)) // expected : true
    println("Result of running p4: " + new A6Solution().evaluate(Programs.p4, e, Programs.f +: funcs)) // expected : 474, no div by zero error
    println("Result of running p5: " + new A6Solution().evaluate(Programs.p5, e, Programs.f +: funcs)) // expected : 160 or 239, no div by zero error
    println("Result of running p6: " + new A6Solution().evaluate(Programs.p6, e, Programs.f +: funcs)) // expected : 160 or 239, no div by zero error
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

class FunctionCallExpression(val functionName: Name, val actualArguments: Expression*) extends Expression {}
/* ========================= EXPRESSION CLASSES END ================================== */

/* ============================= VALUE CLASSES ====================================== */
abstract class Value {}

class IntValue(val v: Int) extends Value { override def toString: String = "IntValue{" + "v=" + v + '}' }

class BooleanValue(val b: Boolean) extends Value { override def toString: String = "BooleanValue{" + "b=" + b + '}' }

class Name(val theName: String) {
  override def toString: String = theName

  override def equals(o: Any): Boolean = {
    val name = o.asInstanceOf[Name]
    Objects.equals(theName, name.theName)
  }

  override def hashCode(): Int = Objects.hash(theName)
}
/* ============================= VALUE CLASSES END ====================================== */

/* ============================= ENVIRONMENT CLASSES ========================================== */
class Binding(val name: Name, val value: Value) { override def toString: String = "{" + "name=" + name + ", value=" + value + '}' }

abstract class Environment{
  var bindings: List[Binding];

  def bind(name: Name, value: Value): Environment;

  final def lookup(name: Name): Value = { lookup(name,bindings) }

  @tailrec
  private def lookup(name: Name, search: List[Binding]): Value = {
    if(search == List.empty){ throw new Error("Name " + name + " not found in environment") }

    if(search.head.name.equals(name)){
      search.head.value
    }
    else {
      lookup(name, search.tail)
    }
  }

  override def toString: String = "Environment{" + bindings + '}'
}

class LexicalScopedEnvironment(var bindings: List[Binding] = List.empty) extends Environment {
  override def bind(name: Name, value: Value): Environment = {
    val b: Binding = new Binding(name, value)
    new LexicalScopedEnvironment(b +: bindings)
  }
}
/* ============================= ENVIRONMENT CLASSES END ========================================== */

/* ==================== FUNCTION CLASSES ============================= */
class Function(val name: Name, val body: Expression, val formalNamesOfArguments: Name*) {}
/* ==================== FUNCTION CLASSES END ============================= */