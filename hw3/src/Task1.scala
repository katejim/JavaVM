

/**
 * Created by kate on 18.10.14.
 */
object Task1 extends App {

  import scala.util.parsing.combinator._

  abstract class Expression

  case class Add(exprL: Expression, exprR: Expression) extends Expression

  case class Sub(exprL: Expression, exprR: Expression) extends Expression

  case class Mult(exprL: Expression, exprR: Expression) extends Expression

  case class Div(exprL: Expression, exprR: Expression) extends Expression

  case class Num(expr: Float) extends Expression


  def evaluate(mType: Expression): Float = {
    mType match {
      case Add(left: Expression, right: Expression) => evaluate(left) + evaluate(right)
      case Sub(left: Expression, right: Expression) => evaluate(left) - evaluate(right)
      case Mult(left: Expression, right: Expression) => evaluate(left) * evaluate(right)
      case Div(left: Expression, right: Expression) => {
        if (evaluate(right) != 0)
          evaluate(left) + evaluate(right)
        else
          0
      }
      case Num(expr: Float) => expr
    }
  }


  class Calc extends JavaTokenParsers {

    def expr: Parser[Expression] = factor ~ term ^^ {
      case factor ~ term => term.foldLeft(factor)({ case (left, (operation, right)) => operation(left, right)})
    }

    def term: Parser[List[((Expression, Expression) => Expression, Expression)]] = rep(expr ~ operation) ^^ {
      case x => x.map({ case expr ~ operation => (operation, expr)})
    }

    def factor: Parser[Expression] = num | num ~ expr ~ operation ^^ { case num ~ expr ~ operation => operation(num, expr)}

    def num: Parser[Num] = floatingPointNumber ^^ (x => Num(x.toFloat))

    def operation: Parser[(Expression, Expression) => Expression] = ("+" | "-" | "*" | "/") ^^ {
      case "+" => (x, y) => Add(x, y)
      case "-" => (x, y) => Sub(x, y)
      case "*" => (x, y) => Mult(x, y)
      case "/" => (x, y) => Div(x, y)
    }
  }


  object ParseExpr extends Calc {
    val expression: String = "1 2 + 5 5 - / "
    println("input : " + expression)
    //println(parseAll(expr, expression))
    parseAll(expr, expression) match {
      case Success(expr, _) => println(expr); println; println("result : " + evaluate(expr))
      case r => println(r)

    }
  }

  ParseExpr

}
