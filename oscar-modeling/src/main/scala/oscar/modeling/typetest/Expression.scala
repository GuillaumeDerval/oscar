package oscar.modeling.typetest

trait Expression[+T <: ValueType] {
  def evaluate(data: Data = MapData.noData): T
  def subExpressions(): Iterable[Expression[_]]
}

// Rough draft of assignment data
trait Data {
  def hasValue[T <: ValueType](v: Var[T]): Boolean
  def getValue[T <: ValueType](v: Var[T]): T
}
class MapData(map: Map[Var[_], ValueType]) extends Data {
  def hasValue[T <: ValueType](v: Var[T]) = map.contains(v) || v.isBound
  def getValue[T <: ValueType](v: Var[T]) = if (v.isBound) v.value else map(v).asInstanceOf[T]
}
object MapData {
  val noData = new MapData(Map())
}

trait Mappable[T <: ValueType, U >: T <: ValueType] {
  def mapSubExpressions[V >: T <: U](func: Expression[T] => Expression[V]): Expression[V]
}