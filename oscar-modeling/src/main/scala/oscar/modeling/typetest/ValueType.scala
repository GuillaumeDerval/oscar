package oscar.modeling.typetest

sealed trait ValueType

class Number(val asDouble: Double) extends ValueType
class Integer(val asInt: Int) extends Number(asInt)