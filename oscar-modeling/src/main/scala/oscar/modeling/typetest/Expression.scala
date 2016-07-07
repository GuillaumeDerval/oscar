package oscar.modeling.typetest

trait Expression[+T <: ValueType] {
  def evaluate(data: Data = new Data {
    def hasValue[U <: ValueType](v: Var[U]) = v.isBound
    def getValue[U <: ValueType](v: Var[U]) = v.value
  }): T
  def subExpressions(): Iterable[Expression[_]]
}

trait Data {
  def hasValue[T <: ValueType](v: Var[T]): Boolean
  def getValue[T <: ValueType](v: Var[T]): T
}

trait Mappable[T <: ValueType, U >: T <: ValueType] {
  def mapSubExpressions[V >: T <: U](func: Expression[T] => Expression[V]): Expression[V]
}