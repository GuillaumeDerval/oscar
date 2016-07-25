package oscar.algebra

import scala.collection.mutable

class Sum[T,U <: AnyType](loop: ALoop[T,Expression[U]])

class Model[O <: AnyType, C <: AnyType]{

  val variables: scala.collection.mutable.Map[VarDescription,Int] = new scala.collection.mutable.HashMap[VarDescription,Int]()
  val indices = mutable.HashSet[Indices[_]]()
  val params = mutable.HashSet[Param1[_]]()

  var maxIndex = 0

  val objectives = scala.collection.mutable.ListBuffer[Expression[O]]()
  val constraints = scala.collection.mutable.ListBuffer[System[C]]()

  def subjectTo(eq: Equation[C]): Unit ={
    constraints += new StreamSystem[C](Stream(eq))
  }

  def subjectTo(eqs: StreamSystem[C]): Unit ={
    constraints += eqs
  }

  def subjectTo(eqs: ALoop[_,Equation[C]]): Unit ={
    constraints += new LoopSystem(Stream(eqs))
  }

  def minimize(eq: Expression[O]): Unit ={
    objectives += eq
  }

  def withObjective(obj: Expression[O]): Unit ={
    objectives += obj
  }

  override def toString = constraints.mkString("\n")
}


/**
  * Created by smo on 18/07/16.
  */
class SolutionBuffer[O<: AnyType, C <: AnyType](implicit model: Model[O,C]) {

  var objectiveValue: Double = 0.0

  val values = new Array[Double](model.maxIndex)

  def apply(v: Indexed) = values(v.id)

  def update[T](v: Indexed, value: Double): Unit ={
    values(v.id) = value
  }

  def update[T](v: Var1[_], vals: IndexedSeq[Double]): Unit ={
    var id = v.id
    for(v <- vals){
      values(id) = v
      id += 1
    }
  }

  def update[T](v: Var2[_,_], vals: IndexedSeq[Double]): Unit ={
    var id = v.id
    val iter = vals.iterator
    for(a <- v.rangeA.values; b <- v.rangeB.values){
      values(id) = iter.next
      id += 1
    }
  }

}