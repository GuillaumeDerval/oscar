package oscar.modeling.typetest

trait Expression[+T <: ValueType] {
  def evaluate(): T = evaluate(new Data {
    def hasValue[U <: ValueType](v: Var[U]) = v.isBound
    def getValue[U <: ValueType](v: Var[U]) = v.value
  })
  def evaluate(data: Data): T
  def subExpressions(): Iterable[Expression[T]]
}

trait Mappable[T <: ValueType, U >: T <: ValueType] {
  def mapSubExpressions[V >: T <: U](func: (Expression[T]) => Expression[V]): Expression[V]
}

trait Derivable[T <: Number] {
  def derive(): Expression[T]
}