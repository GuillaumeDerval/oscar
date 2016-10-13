package oscar.modeling.algebra

import scala.collection.mutable.HashSet

/**
 * Expression for element constraint
 * @param table The table in which we will retrieve the element ``key``
 * @param key The key to retrieve in ``table``
 */
case class Element(table: Array[IntExpression], key: IntExpression) extends IntExpression {
  override def evaluate(): Int = table(key.evaluate()).evaluate()
  override def min: Int = {
    //TODO: we can make it better easily
    values.min
  }
  override def max: Int = {
    //TODO: we can make it better easily
    values.max
  }
  override def values(): Iterable[Int] = {
    val s = new HashSet[Int]()
    for(i <- key.values)
      s ++= table(i).values
    s
  }

  /**
   * Returns an iterable that contains all sub-expressions of this expression
   */
  override def subexpressions(): Iterable[IntExpression] = table ++ Array(key)

  /**
   * Apply a function on all sub-expressions of this expression and returns a new expression of the same type.
   * This function should return a value that is of the class as the object that was given to it.
   */
  override def mapSubexpressions(func: (IntExpression) => IntExpression): IntExpression = new Element(table.map(func), func(key))
}

case class ElementCst(table: Array[Int], key: IntExpression) extends IntExpression {
  override def evaluate(): Int = table(key.evaluate())
  override def min: Int = {
    //TODO: we can make it better easily
    values.min
  }
  override def max: Int = {
    //TODO: we can make it better easily
    values.max
  }
  override def values(): Iterable[Int] = table

  /**
    * Returns an iterable that contains all sub-expressions of this expression
    */
  override def subexpressions(): Iterable[IntExpression] = Array(key)

  /**
    * Apply a function on all sub-expressions of this expression and returns a new expression of the same type.
    * This function should return a value that is of the class as the object that was given to it.
    */
  override def mapSubexpressions(func: (IntExpression) => IntExpression): IntExpression = new ElementCst(table, func(key))
}
