//package oscar.algebra.ampl
//
//import oscar.algebra._
//
//
//case class Var1[T,+V:Numeric](range: Indices[T], name: String, lowerBound: Option[V] = None, upperBound: Option[V] = None)(implicit model: Model[_, _,V]) extends VarDescription[V] with Indexable1[T]{
//
//  val id = model.maxIndex
//  model.maxIndex += range.values.size
//
//  model.variables += this -> id
//
//  //def apply(i: Symbolic) = SymbolicVar(this)(i.name)
//  def get(i: T) = new Var1Ref(this)(i)
//
//  def apply(i: Index[T]): Var[V] = {
//    i match {
//      case SymbolicIndex(name) => SymbolicVar1(this)(name)
//      case ConcreteIndex(index) =>
//        require(range.values.contains(index))
//        // TODO improve this... indexOf is surely not the best...
//        new Var1Ref[T,V](this)(index)
//      //Var(id + range.values.indexOf(index),lowerBound,upperBound)
//    }
//  }
//}
//
//case class Var2[A,B,+V:Numeric](rangeA: Indices[A], rangeB: Indices[B], name: String, lowerBound: Option[V] = None, upperBound: Option[V] = None)(implicit model: Model[_, _,V]) extends VarDescription[V] {
//
//
//  val id = model.maxIndex
//
//  model.variables += this -> id
//  model.maxIndex += rangeA.values.size * rangeB.values.size
//
//  def get(i: A)(j: B) = new Var2Ref(this)(i,j)
//
//  def apply(i: Index[A])(j: Index[B]) = {
//    //    require(rangeA.values.contains(i._1))
//    //    require(rangeB.values.contains(j))
//    (i,j) match {
//      case (ConcreteIndex(indexA),ConcreteIndex(indexB)) => new Var2Ref(this)(indexA,indexB)
//      case _ => SymbolicVar2(this)(i.index, j.index)
//
//    }
//  }
//}
//
////
//
//class Param1Ref[T,+V:Numeric](val param: Param1[T,V])(val index: T) extends Term[Constant,V] {
//  override def toString = name
//  def name = s"${param.name}($index)"
//  def value = None
//  def eval[VP>: V](env: Var[VP] => VP)(implicit numeric: Numeric[VP]) = param.values(index)
//
//  def toExpression[VP>: V](implicit numeric: Numeric[VP]) = new Expression(Stream(Prod(Seq(this))))
//}
//
//
//object SymbolicParam1 {
//  def apply[T,V: Numeric](v: Param1[T,V])(index: String) = new SymbolicParam1[T,V](v)(index)
//}
//
//class SymbolicParam1[T,+V: Numeric] private(v: Param1[T,V])(index: String) extends Term[Constant,V] {
//  def name = s"${v.name}[$index]"
//  override def toString = name
//  def value = None
//  def eval[VP>: V](env: Var[VP] => VP)(implicit numeric: Numeric[VP]) = ???
//
//  def toExpression[VP>: V](implicit numeric: Numeric[VP]) = new Expression(Stream(Prod(Seq(this))))
//}
//
//case class Param1[T,+V: Numeric](val name: String, val range: Indices[T], val values: Map[T,V], default: Option[V] = None)(implicit model: Model[_,_,V]) extends Indexable1[T]{
//
//  model.params += this
//
//  def apply(i: Index[T]) = {
//    i match {
//      case SymbolicIndex(name) => SymbolicParam1(this)(name)
//      case ConcreteIndex(index) =>
//        require(range.values.contains(index))
//        // TODO improve this... indexOf is surely not the best...
//        new Param1Ref[T,V](this)(index)
//      //Var(id + range.values.indexOf(index),lowerBound,upperBound)
//    }
//  }
//
//}
//
//
//trait Indexable1[T]{
//  def range: Indices[T]
//  def name: String
//  override def toString = name
//}
//
//class Var1Ref[T,+V:Numeric](val v: Var1[T,V])(val index: T) extends oscar.algebra.Var[V] with Indexed {
//  def name = s"${v.name}[$index]"
//  def id = v.id + v.range.indexOf(index)
//  def value = None
//}
//
//object SymbolicVar1 {
//  def apply[T,V:Numeric](v: Var1[T,V])(index: String) = new SymbolicVar1[T,V](v)(index)
//}
//
//class SymbolicVar1[T,+V:Numeric] private(v: Var1[T,V])(index: String) extends oscar.algebra.Var[V] {
//  def name = s"${v.name}[$index]"
//
//  def value = None
//}
//
//class Var2Ref[A,B,+V:Numeric](val v: Var2[A,B,V])(val indexA: A, val indexB: B) extends oscar.algebra.Var[V] with Indexed {
//  def name = s"${v.name}[$indexA,$indexB]"
//  def id = v.id + v.rangeA.indexOf(indexA)*v.rangeB.values.size + v.rangeB.indexOf(indexB)
//  def value = None
//}
//
//object SymbolicVar2 {
//  def apply[A,B,V:Numeric](v: Var2[A,B,V])(indexA: String, indexB: String) = new SymbolicVar2[A,B,V](v)(indexA,indexB)
//}
//
//class SymbolicVar2[A,B,+V:Numeric] private(v: Var2[A,B,V])(indexA: String, indexB: String) extends oscar.algebra.Var[V] {
//  def name = s"${v.name}[$indexA,$indexB]"
//
//  def value = None
//}