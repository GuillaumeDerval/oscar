package oscar.modeling.algebra

import oscar.modeling.misc.VariableNotBoundException

import scala.collection.mutable

/**
  * Created by dervalguillaume on 6/06/16.
  */
case class Element2D(array: Array[Array[IntExpression]], idx1: IntExpression, idx2: IntExpression) extends IntExpression {
  /**
    * Evaluate this expression. All variables referenced have to be bound.
    *
    * @throws VariableNotBoundException when a variable is not bound
    * @return the value of this expression
    */
  override def evaluate(): Int = array(idx1.evaluate())(idx2.evaluate()).evaluate()

  override def min: Int = {
    //TODO: we can make it better easily
    values().min
  }
  override def max: Int = {
    //TODO: we can make it better easily
    values().max
  }

  override def values(): Iterable[Int] = {
    val s = new mutable.HashSet[Int]()
    for(i <- idx1.values())
      for(j <- idx2.values())
        s ++= array(i)(j).values
    s
  }

  /**
    * Returns an iterable that contains all sub-expressions of this expression
    */
  override def subexpressions(): Iterable[IntExpression] = array.flatMap(a => a) ++ Array(idx1, idx2)

  /**
    * Apply a function on all sub-expressions of this expression and returns a new expression of the same type.
    * This function should return a value that is of the class as the object that was given to it.
    */
  override def mapSubexpressions(func: (IntExpression) => IntExpression): IntExpression = new Element2D(array.map(a => a.map(func)), func(idx1), func(idx2))
}

case class ElementCst2D(array: Array[Array[Int]], idx1: IntExpression, idx2: IntExpression) extends IntExpression {
  /**
    * Evaluate this expression. All variables referenced have to be bound.
    *
    * @throws VariableNotBoundException when a variable is not bound
    * @return the value of this expression
    */
  override def evaluate(): Int = array(idx1.evaluate())(idx2.evaluate())

  override def min: Int = {
    //TODO: we can make it better easily
    values().min
  }
  override def max: Int = {
    //TODO: we can make it better easily
    values().max
  }

  override def values(): Iterable[Int] = {
    val s = new mutable.HashSet[Int]()
    for(i <- array.indices)
      s ++= array(i)
    s
  }

  /**
    * Returns an iterable that contains all sub-expressions of this expression
    */
  override def subexpressions(): Iterable[IntExpression] = Array(idx1, idx2)

  /**
    * Apply a function on all sub-expressions of this expression and returns a new expression of the same type.
    * This function should return a value that is of the class as the object that was given to it.
    */
  override def mapSubexpressions(func: (IntExpression) => IntExpression): IntExpression = new ElementCst2D(array, func(idx1), func(idx2))
}
