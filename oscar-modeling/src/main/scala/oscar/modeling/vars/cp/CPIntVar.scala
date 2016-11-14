package oscar.modeling.vars.cp

import oscar.modeling.vars.IntVarImplem

import scala.util.Random

class CPIntVar(val realCPVar: oscar.cp.CPIntVar) extends CPVar with IntVarImplem {
  override def isContinuous: Boolean = realCPVar.isContinuous

  /**
    * @return true if the domain of the variable has exactly one value, false if the domain has more than one value
    */
  def isBound: Boolean = realCPVar.isBound

  /**
    * @param v: value to test
    * @return true if the variable is bound to value v, false if variable is not bound or bound to another value than v
    */
  override def isBoundTo(v: Int): Boolean = realCPVar.isBoundTo(v)

  /**
    * Test if a value is in the domain
    * @param value: value to test
    * @return  true if the domain contains the value val, false otherwise
    */
  def hasValue(value: Int): Boolean = realCPVar.hasValue(value)

  /**
    * @param value
    * @return the smallest value > val in the domain
    */
  def valueAfter(value: Int): Int = realCPVar.valueAfter(value)

  /**
    * @param value
    * @return the largest value < val in the domain
    */
  def valueBefore(value: Int): Int = realCPVar.valueBefore(value)

  /**
    * @return A random value in the domain of the variable (uniform distribution)
    */
  def randomValue(rand: Random): Int = realCPVar.randomValue(rand)

  /**
    * @return the size of the domain
    */
  override def size: Int = realCPVar.size

  /**
    * @return the size of the domain
    */
  def getSize: Int = realCPVar.size

  /**
    * @return true if domain is empty, false else
    */
  override def isEmpty: Boolean = realCPVar.isEmpty

  /**
    * @return  the minimum value in the domain
    */
  def min: Int = realCPVar.min

  /**
    * @return the minimum value in the domain
    */
  def getMin: Int = realCPVar.min

  /**
    * @return  the maximum value in the domain
    */
  def max: Int = realCPVar.max

  /**
    * @return the maximum value in the domain
    */
  def getMax: Int = realCPVar.max

  def iterator: Iterator[Int] = realCPVar.iterator

  override def foreach[@specialized(Int) U](f: Int => U): Unit = realCPVar.foreach(f)

  /**
    * @return an (not sorted) array representation of the domain.
    */
  override def toArray: Array[Int] = realCPVar.toArray

  /**
    * @param array.length >= this.size
    * @return Fills the array with the domain.
    *         returns the number of values (this.size).
    *         The array is not sorted.
    */
  override def fillArray(array: Array[Int]): Int = realCPVar.fillArray(array)

  /**
    * Return a representative name for this var(-like), if one was given
    */
  def name: String = realCPVar.name
}

object CPIntVar {
  def apply(content: Iterable[Int], name: String, store: oscar.cp.CPStore): CPIntVar = {
    new CPIntVar(oscar.cp.CPIntVar(content, name)(store))
  }
}