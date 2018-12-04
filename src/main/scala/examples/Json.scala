package examples

import recursionless.{Fix, Functor}

sealed trait JsonF[+A]
case object JsNull extends JsonF[Nothing]
final case class JsString(value: String) extends JsonF[Nothing]
final case class JsNumber(value: BigDecimal) extends JsonF[Nothing]
final case class JsBoolean(value: Boolean) extends JsonF[Nothing]
final case class JsArray[A](values: Seq[A]) extends JsonF[A]
final case class JsObject[A](values: Map[String, A]) extends JsonF[A]

object JsonF {
  implicit val jsonFFunctor: Functor[JsonF] = new Functor[JsonF] {
    override def map[A, B](jsonA: JsonF[A])(f: A => B): JsonF[B] =
      jsonA match {
        case JsNull => JsNull
        case JsString(value) => JsString(value)
        case JsNumber(value) => JsNumber(value)
        case JsBoolean(value) => JsBoolean(value)
        case JsArray(values) => JsArray(values.map(f))
        case JsObject(values) => JsObject(values.mapValues(f))
      }
  }
}

object Json {
  import JsonF._
  type Value = Fix[JsonF]

  // Syntactic sugar to hide Fix under the carpet
  val jsNull: Value = Fix[JsonF](JsNull)
  def str(value: String): Value = Fix[JsonF](JsString(value))
  def num(value: BigDecimal): Value = Fix[JsonF](JsNumber(value))
  def num(value: Int): Value = num(BigDecimal(value))
  def num(value: Long): Value = num(BigDecimal(value))
  def bool(value: Boolean): Value = Fix[JsonF](JsBoolean(value))
  def arr(values: Value*): Value = Fix(JsArray(values))
  def obj(values: (String, Value)*): Value = Fix(JsObject(values.toMap))

  // using Recursion schemes to serialise json
  def stringify(json: Fix[JsonF]): String = {
    json.cata[String] {
      case JsNull => "null"
      case JsString(value) => s""""$value""""
      case JsNumber(value) => value.toString
      case JsBoolean(value) => if (value) "true" else "false"
      case JsArray(values) => values.mkString("[", ", ", "]") // look ma, no recursion
      case JsObject(values) => values.map { case (k, v) => s"$k: $v" }.mkString("{", ", ", "}")
    }
  }
}

object Main extends App {
  val json = Json.obj(
    "profile" -> Json.obj(
      "id" -> Json.num(12345),
      "name" -> Json.str("Tamer Abdulradi"),
      "links" -> Json.arr(
        Json.str("http://twitter.com/tabdulradi"),
        Json.str("http://github.com/tabdulradi"),
        Json.str("http://abdulradi.com")
      ),
    ),
    "is_foo" -> Json.bool(true),
    "math_pi" -> Json.num(Math.PI),
    "null" -> Json.jsNull
  )

  println(Json.stringify(json))
}