package oscar.modeling.typetest

sealed trait ValueType
object ValueConversions {
  import scala.language.implicitConversions
  implicit def numberToDouble(n: Number): Double = n.asDouble
  implicit def integerToInt(i: Integer): Int = i.asInt
  implicit def doubleToNumber(d: Double): Number = new Number(d)
  implicit def intToInteger(i: Int): Integer = new Integer(i)
}

class Number(val asDouble: Double) extends ValueType
class Integer(val asInt: Int) extends Number(asInt)