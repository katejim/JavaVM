import scala.util.parsing.combinator.JavaTokenParsers

object Task2 extends App {

  abstract  class JSONObject
  case class JObject(expr: Map[JString, JSONObject]) extends JSONObject
  case class JArr(expr: Seq[JSONObject]) extends JSONObject
  case class JNull() extends JSONObject
  case class JBool(boolean: Boolean) extends JSONObject
  case class JFloat(floating: Double) extends JSONObject
  case class JString(string: String) extends JSONObject

  def prettyJason(jason: JSONObject): String = jason match {
    case JNull() => "null"
    case JBool(x) => x.toString
    case JFloat(x) => x.toString
    case JString(s) => s
    case JArr(ss) => "[" ++ ss.map(prettyJason).mkString(", ") ++ "]"
    case JObject(oss) => "{\n" ++ oss.seq.map((x:(JString, JSONObject)) => prettyJason(x._1) ++ ": " ++ prettyJason(x._2)).mkString(",\n") ++ "\n}\n"
  }

  class JSONParser extends JavaTokenParsers {
    def value: Parser[JSONObject] = obj |
      arr |
      stringLiteral ^^ (x => JString(x.toString)) |
      floatingPointNumber ^^ (x => JFloat(x.toDouble)) |
      "true" ^^ (x => JBool(true)) |
      "false" ^^ (x => JBool(false)) |
      "null" ^^ (x => JNull())

    def obj: Parser[JObject] = "{" ~> repsep(member, ",") <~ "}" ^^ {
      case x => JObject(Map() ++ x)
    }

    def arr: Parser[JArr] = "[" ~> repsep(value, ",") <~ "]" ^^ {
      x => JArr(x)
    }

    def member: Parser[(JString, JSONObject)] = stringLiteral ~ ":" ~ value ^^ {
      case stringLiteral ~ ":" ~ value => (JString(stringLiteral), value)
    }
  }

  object ParserObj extends JSONParser {
    val input: String = scala.io.Source.fromFile("test.json").mkString
    println(parseAll(value, input))
    parseAll(value,  input) match {
      case Success(expr, _) => println(expr); println; println("result : " + prettyJason(expr))
      case r => println(r)

    }
  }

  ParserObj
}