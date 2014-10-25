/**
 * Created by kate on 04.10.14.
 */
object Task3 extends App {

  abstract class LogicalType {
    def evaluate(expr: Boolean): Boolean

    def ==(expr: LogicalType): Boolean =
      this.evaluate(true).equals(expr.evaluate(true)) && this.evaluate(false).equals(expr.evaluate(false))
  }

  case class And(exprLeft: LogicalType, exprRight: LogicalType) extends LogicalType {
    override def evaluate(expr: Boolean): Boolean = exprLeft.evaluate(expr) && exprRight.evaluate(expr)
  }

  case class Or(exprLeft: LogicalType, exprRight: LogicalType) extends LogicalType {
    override def evaluate(expr: Boolean): Boolean = exprLeft.evaluate(expr) || exprRight.evaluate(expr)
  }

  case class Not(exprIn: LogicalType) extends LogicalType {
    override def evaluate(expr: Boolean): Boolean = !exprIn.evaluate(expr)
  }

  case class True() extends LogicalType {
    override def evaluate(expr: Boolean): Boolean = true
  }

  case class False() extends LogicalType {
    override def evaluate(expr: Boolean): Boolean = false
  }

  case class X() extends LogicalType {
    override def evaluate(expr: Boolean): Boolean = expr
  }


  def result() = {
    val myX = True()
    val myY = False()
    val myAnd = And(myX, myY)
    val myOr = Or(myX, myY)


    println("expected: false false true")
    println("real: " + (myAnd == myOr) + " " + (myX == myY) + " " + (myAnd == myY))
  }

  result()
}
