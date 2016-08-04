import oscar.algebra.{AnyType, Const, Expression, Prod, Term, Var}

//package oscar.algebra.ampl
//
//import oscar.algebra.{AnyType, Equation, Model, System}
//
//class LoopSystem[T <: AnyType,V](val loops: Stream[ALoop[_,Equation[T,V]]], val name: String = "System") extends System[T,V]{
//
//  def equations = loops.flatMap(_.toStream)
//
//  def :||(s: String) = new LoopSystem(loops, s)
//
//  override def toString = {
//    val res = new StringBuffer()
//    res.append(name)
//    res.append("\n--------------------\n")
//    res.append(loops.toList.mkString("  ", "\n  ","\n"))
//    res.append("\n--------------------\n")
//
//    res.toString
//  }
//}
//
//trait Index[+T]{
//  def index: String
//}
//case class SymbolicIndex(name: String) extends Index[Nothing]{
//  def index = name
//}
//case class ConcreteIndex[+T](value: T) extends Index[T]{
//  def index = value.toString
//}
//
//
//trait ALoop[T,+U]{
//
//  def ||: (n: String): ALoop[T,U]
//  def name: String
//
//  def p = List("i","j","k")
//
//  def allIndices: List[Indices[_]]
//
//  def iterator(possibleIndices: List[String]): String = allIndices.zip(possibleIndices).map{case (it,index) => it.iterator(index)}.mkString(", ")
//  def body(possibleIndices: List[String]): U
//
//  def toStream: Stream[U]
//
//  override def toString = s"{${iterator(p)}}:\n\t${body(p)}"
//}
//
//class SLoop[T,TP,+U](val indices: Indices[T], val name: String = "ND")(val f: Index[T] => ALoop[TP,U]) extends ALoop[T,U]{
//
//  def ||: (n: String): ALoop[T,U] = new SLoop(indices,n)(f)
//
//  // TODO not great... the Symbolic("")
//  def allIndices: List[Indices[_]] = indices :: f(SymbolicIndex("")).allIndices
//
//  def body(possibleIndices: List[String]): U = {
//    possibleIndices match {
//      case head :: tail => f(SymbolicIndex(head)).body(tail)
//    }
//  }
//
//  def toStream = indices.values.toStream.flatMap{i => f(ConcreteIndex(i)).toStream}
//
//}
//
//class Loop[T,+U](val indices: Indices[T], val name: String = "ND")(val f: Index[T] => U) extends ALoop[T,U]{
//
//  def ||: (n: String): ALoop[T,U] = new Loop(indices,n)(f)
//
//  override def allIndices: List[Indices[_]] = List(indices)
//
//  def body(possibleIndices: List[String]): U = f(SymbolicIndex(possibleIndices.head))
//
//  def flatMap[TP,UP](fp: Index[T] => Loop[TP,UP]): SLoop[T,TP,UP] = {
//    val fpp = {s:Index[T] => fp(s).f}
//    new SLoop[T,TP,UP](indices)(fp)
//  }
//  //def body(possibleIndices: List[String]): String = f(i).toString
//  override def toString = s"{${iterator(p)}}:\n\t${f(SymbolicIndex(p.head))}"
//
//
//  def toStream = indices.values.toStream.map{i => f(ConcreteIndex(i))}
//}
//
//case class Indices[T](name: String, values: IndexedSeq[T])(implicit model: Model[_,_,_]) /*extends ALoop[T,T]*/ {
//
//  model.indices += this
//
//  override def toString = name
//
//  val indexOf = Map[T,Int]() //values.zipWithIndex.toMap
//  def iterator(index: String): String = s"${index} in $name"
//  def body(possibleIndices: List[String]): String = possibleIndices.head
//  def flatMap[TP,UP](fp: Index[T] => Loop[TP,UP]): SLoop[T,TP,UP] = {
//    val fpp = {s:Index[T] => fp(s).f}
//    new SLoop[T,TP,UP](this)(fp)
//  }
//
//  def map[U](f: Index[T] => U): Loop[T,U] = new Loop(this)(f)
//
//  def toStream = values.toStream
//
//}
//
//
//
////case class MapIndices[K,+T](name: String, values: Map[K,T]) extends Indices[T]{
////  def iterator(index: Index): String = s"$index in $name"
////}

//
///**
//  * Created by smo on 20/07/16.
//  */
//class SumLoop[I,+T <: AnyType,+V](val aLoop: ALoop[I,Expression[T,V]])(implicit numeric: Numeric[V]) extends Term[T,V]{
//  def eval[VP>: V](env: (Var[VP]) => VP)(implicit numeric: Numeric[VP]): VP = ???
//
//  override def value: Option[V] = ???
//
//  def toExpression[VP>: V](implicit numeric: Numeric[VP])  = new Expression[T,VP](Stream(new Prod(Const(numeric.one), Seq(this))))
//}
