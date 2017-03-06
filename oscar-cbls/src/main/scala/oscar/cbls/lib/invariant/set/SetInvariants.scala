package oscar.cbls.lib.invariant.set

import oscar.cbls.core.computation.{IntValue, SetValue}

import scala.collection.immutable.SortedSet

/**
 * modeling interface presenting the set invariants
 * @author renaud.delandtsheer@cetic.be
 */
trait SetInvariants{
  /** left UNION right
    * @param left is an intvarset
    * @param right is an intvarset
    * */
  def union(left:SetValue, right:SetValue) = Union(left:SetValue, right:SetValue)

  /** left INTER right
    * @param left is an intvarset
    * @param right is an intvarset
    * */
  def inter(left:SetValue, right:SetValue) = Inter(left:SetValue, right:SetValue)

  /** left MINUS right, the set diff operator
    * @param left is the base set
    * @param right is the set that is removed from left
    * */
  def diff(left:SetValue, right:SetValue) = Diff(left:SetValue, right:SetValue)

  /** #(v) (cardinality)
    * @param v is an IntSetVar, the set of integers to count
    */
  def cardinality(v:SetValue) = Cardinality(v:SetValue)

  /** makes an IntSetVar out of a set of IntVar. If several variables have the same value, the value is present only once in the resulting set
    * @param on is a set of IntVar
    * */
  def makeSet(on:SortedSet[IntValue]) = MakeSet(on:SortedSet[IntValue])

  /** makes a set out of an interval specified by a lower bound and an upper bound. if lb > ub, the set is empty.
    * @param lb is the lower bound of the interval
    * @param ub is the upper bound of the interval
    * */
  def interval(lb:IntValue,ub:IntValue) = Interval(lb:IntValue,ub:IntValue)

  /**maintains the output as any value taken from the intset var parameter.
    * if this set is empty, puts the default value ni output.
    * @param from
    * @param default
    */
  def takeAny(from:SetValue,  default:Int) = TakeAny(from:SetValue,  default:Int)

  /** Sum(i in on)(fun(i))
    * @param on is the set of integers to add
    * @param fun is an optional function Int -> Int to apply before summing elements. It is expected not to rely on any variable of the model.
    * */
  def setSum(on:SetValue, fun:(Int => Int) = ((a:Int) => a)) = SetSum(on, fun)

  /** PRod(i in on)(fun(i))
    * @param on is the set of integers to multiply
    * */
  def setProd(on:SetValue) = SetProd(on)
}


