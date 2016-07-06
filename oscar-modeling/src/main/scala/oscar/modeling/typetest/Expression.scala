package oscar.modeling.typetest

trait Expression[+T <: ValueType] {
  def evaluate(): T
  def subExpressions(): Iterable[Expression[T]]
}

trait Mappable[T <: ValueType, U >: T <: ValueType] {
  def mapSubExpressions[V >: T <: U](func: (Expression[T]) => Expression[V]): Expression[V]
}

trait Derivable[T <: Number] {
  def derive(): Expression[T]
}