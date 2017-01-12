package oscar.cbls.lib.invariant.seq

/*******************************************************************************
  * OscaR is free software: you can redistribute it and/or modify
  * it under the terms of the GNU Lesser General Public License as published by
  * the Free Software Foundation, either version 2.1 of the License, or
  * (at your option) any later version.
  *
  * OscaR is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU Lesser General Public License  for more details.
  *
  * You should have received a copy of the GNU Lesser General Public License along with OscaR.
  * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
  ******************************************************************************/


import oscar.cbls.algo.seq.functional.IntSequence
import oscar.cbls.core.computation._


trait SeqInvariants{
  /**
   * builds a changing seq value that is maintained as the concatenation of a and b.
   * if either of them is a constant, the instantiated class is adapted
   *
   * @param a the first sequence
   * @param b the second sequence
   * @param maxPivotPerValuePercent the maxPErcentage for the created variable
   * @param maxHistorySize the max history size for the create variable
   * @return a changing seq value that is maintained as teh concatenation of a and b
   */
  def concatenate(a : SeqValue, b : SeqValue, maxPivotPerValuePercent : Int = 4, maxHistorySize : Int = 20) : SeqValue = {
    (a,b) match {
      case (ac : CBLSSeqConst,bc : CBLSSeqConst) =>
        CBLSSeqConst(IntSequence(ac.value ++ bc.value))
      case (ac : CBLSSeqConst,bv : ChangingSeqValue) =>
        new ConcatenateFirstConstant(ac.value.toList, bv, maxPivotPerValuePercent, maxHistorySize)
      case (av : ChangingSeqValue, bc : CBLSSeqConst) =>
        new ConcatenateSecondConstant(av, bc.value.toList, maxPivotPerValuePercent, maxHistorySize)
      case (av : ChangingSeqValue, bv : ChangingSeqValue) =>
        new Concatenate(av, bv, maxPivotPerValuePercent, maxHistorySize)
    }
  }


  /**
   * content of v
   * @param v is a SeqValue, the values appearing in the sequence
   * @author renaud.delandtsheer@cetic.be
   */
  def content(v:SeqValue) = new Content(v)

  /**
   * maintains this as the flipped value of v
   * @param v
   * @param maxPivotPerValuePercent
   * @param maxHistorySize
   */
  def flip(v: SeqValue, maxPivotPerValuePercent:Int = 10, maxHistorySize:Int = 10) =
    new Flip(v: SeqValue, maxPivotPerValuePercent, maxHistorySize)


  /**
   * @param seq a sequence of integers
   * @param mapArray an array that is taken as a function (it cannot be modified after this call)
   * @return a sequence where the value at any position p is equal to mapArray(seq(p))
   */
  def map(seq:ChangingSeqValue,mapArray:Array[Int]):MapConstantFun = {
    new MapConstantFun(seq,mapArray,InvariantHelper.getMinMaxBoundsInt(mapArray)._2)
  }

  /**
   * @param seq a sequence of integers
   * @param transform a function to apply to each value occuring in the sequence (it cannot be modified after this call)
   * @return a sequence where the value at any position p is equal to transform(seq(p))
   */
  def map(seq:ChangingSeqValue, transform:Int=>Int,maxTransform:Int) =
    new MapConstantFun(seq:ChangingSeqValue, transform:Int=>Int,maxTransform:Int)

  /**
   * @param seq a sequence of integers
   * @param mapArray an array that is taken as a function The value in this array an be variable that change value (althoug hthe content of the array cannot change after this call)
   * @return a sequence where the value at any position p is equal to mapArray(seq(p)).value
   */
  def map(seq:ChangingSeqValue, mapArray:Array[IntValue]) =
    new MapThroughArray(seq:ChangingSeqValue, mapArray)

  /**
   * the position of value a in sequence v; default if not in the sequence
   * @param v is a SeqValue
   * @param a is the value that is to locate in the sequence
   */
  def occurrencesOf(v: SeqValue, a:IntValue) = OccurrencesOf(v: SeqValue, a:IntValue)

  /**
   * Maintains the position of value of variable a in the sequence v.
   * @param v a sequence
   * @param a an intValue, which can be a CBLSIntVar for instance
   * @return a ChangingSetValue that is maintained as the set of position in v where the value is the one of a
   */
  def positionsOf(v: SeqValue, a:IntValue) = new PositionsOf(v, a)

  /**
   * Maintains the position of value of variable a in the sequence v.
   * @param v a sequence
   * @param a an integer
   * @return a ChangingSetValue that is maintained as the set of position in v where the value is a
   */
  def positionsOf(v: SeqValue, a:Int) = new PositionsOfConst(v, a)

  /**
   * precedence assumes that number can occur only once in the sequence
   * so that the constraint is to be enforced from any occurrence to any occurrence,
   * "any" being chosen arbitrarily by tne invariant, and the choice an of course change at any time.
   * also if one of the two value of a precedence is not present in the sequence,
   * it is considered that the precedence is enforced.
   *
   * maintains the number of violated precedences.
   *
   * @param seq
   * @param beforeAfter
   * @author renaud.delandtsheer@cetic.be
   */
  def precedence(seq:ChangingSeqValue, beforeAfter:List[(Int,Int)]) =
    Precedence(seq,beforeAfter)


  /**
   * sum(f(v))
   * the sum of all element in v after passing through f;
   * if a value has multiple occurrences, their f-transformed occurrences are summed
   * @param v is a SeqValue, containing a number of values, to sum
   * @param f is a function that is applied to every value in f prior to the sum
   * @author renaud.delandtsheer@cetic.be
   */
  def seqSum(v: SeqValue, f:(Int => Int) = (a:Int) => a) = SeqSum(v, f)

  /**
   * #(v) (cardinality, or length (since a SeqValue can only contain at most one instance of any int value)
   * @param v is a SeqValue, containing a number of values, to count
   * @author renaud.delandtsheer@cetic.be
   */
  def size(v: SeqValue) = Size(v)

  /**
   * Maintains and array telling, for each value (indice of the array) the set of value that can succeed it in the sequence.
   * There are multiple successors although we only consider the next value
   * because a value can appear several time in the sequence, it can therefore have several successors.
   *
   * @param seq
   * @return the array of SetVar that mention the set of successor for each possible value.
   * @author renaud.delandtsheer@cetic.be
   */
  def successors(seq : ChangingSeqValue) : Array[CBLSSetVar] = Successors(seq : ChangingSeqValue)

}
