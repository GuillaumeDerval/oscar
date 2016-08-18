package oscar.algebra

import scala.collection.mutable

//class Sum[T,U <: AnyType,V](loop: ALoop[T,Expression[U,V]])

class Model[O <: AnyType, C <: AnyType,V]{

  val variables: scala.collection.mutable.Map[Var[V],Int] = new scala.collection.mutable.HashMap[Var[V],Int]()
//  val indices = mutable.HashSet[Indices[_]]()
//  val params = mutable.HashSet[Param1[_,V]]()

  var maxIndex = 0

  val objectives = scala.collection.mutable.ListBuffer[Expression[O,V]]()
  val constraints = scala.collection.mutable.ListBuffer[System[C,V]]()

  def addVariable(v: Var[V]) = {
    val res = maxIndex
    maxIndex += 1
    variables += v -> res
    res
  }

  def subjectTo(eq: Equation[C,V]): Unit ={
    constraints += new StreamSystem[C,V](Iterator(eq))
  }

  def subjectTo(eqs: StreamSystem[C,V]): Unit ={
    constraints += eqs
  }

//  def subjectTo(eqs: ALoop[_,Equation[C,V]]): Unit ={
//    constraints += new LoopSystem(Stream(eqs))
//  }

  def minimize(eq: Expression[O,V]): Unit ={
    objectives += eq
  }

  def withObjective(obj: Expression[O,V]): Unit ={
    objectives += obj
  }

  override def toString = constraints.mkString("\n")
}


class SolutionBuffer[O<: AnyType, C <: AnyType,V](implicit model: Model[O,C,V]) {

  var objectiveValue: Double = 0.0

  val values = new Array[Double](model.maxIndex)

  def apply(v: Var[V]) = values(v.id)

  def update[T](v: Var[V], value: Double): Unit ={
    values(v.id) = value
  }

//  def update[T](v: Var1[_,V], vals: IndexedSeq[Double]): Unit ={
//    var id = v.id
//    for(v <- vals){
//      values(id) = v
//      id += 1
//    }
//  }
//
//  def update[T](v: Var2[_,_,V], vals: IndexedSeq[Double]): Unit ={
//    var id = v.id
//    val iter = vals.iterator
//    for(a <- v.rangeA.values; b <- v.rangeB.values){
//      values(id) = iter.next
//      id += 1
//    }
//  }

}