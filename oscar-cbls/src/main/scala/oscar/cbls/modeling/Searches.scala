package oscar.cbls.modeling

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

import oscar.cbls.core.computation.CBLSIntVar
import oscar.cbls.lib.search.neighborhoods._

import scala.collection.immutable.SortedSet

/** A trait that interfaces some of the neighborhoods of OScaR.CBLS
  *
  */
trait Search {

  /**
   * will find a variable in the array, and find a value from its range that improves the objective function
   *
   * @param vars an array of [[oscar.cbls.core.computation.CBLSIntVar]] defining the search space
   * @param name the name of the neighborhood
   * @param best true for the best move, false for the first move, default false
   * @param searchZone a subset of the indices of vars to consider.
   *                   If none is provided, all the array will be considered each time
   * @param symmetryClassOfVariables a function that input the ID of a variable and returns a symmetry class;
   *                      ony one of the variable in each class will be considered to make search faster
   *                      Int.MinValue is considered different to itself
   *                      if you set to None this will not be used at all
   *                      variables of the same class with different values will not be considered as symmetrical
   * @param symmetryClassOfValues a function that inputs the ID of a variable and a possible value for this variable,
   *                              and returns a symmetry class for this variable and value
   *                              only values belonging to different symmetry classes will be tested
   *                             Int.MinValue is considered different to itself
   *                             (this is only useful if your model is awfully expensive to evaluate)
   * @param domain a function that receives a variable and its Id in the vars array
   *               and returns the domain that is searched for the variable
   *               by default, the domain of the variable is explored
   * @param hotRestart  if true, the exploration order in case you ar not going for the best is a hotRestart
   *                    even if you specify a searchZone that is: the exploration starts again
   *                    at the position where it stopped, and consider the indices in increasing order
   *                    if false, consider the exploration range in natural order from the first position.
   */
  def assignNeighborhood(vars:Array[CBLSIntVar],
                         name:String = "AssignNeighborhood",
                         best:Boolean = false,
                         searchZone:() => Iterable[Int] = null,
                         symmetryClassOfVariables:Option[Int => Int] = None,
                         symmetryClassOfValues:Option[Int => Int => Int] = None,
                         domain:(CBLSIntVar,Int) => Iterable[Int] = (v,i) => v.domain,
                         hotRestart:Boolean = true)
  = AssignNeighborhood(vars,name,best,searchZone,symmetryClassOfVariables,symmetryClassOfValues,domain,hotRestart)


  /**
   * will randomize the array, typically to get out of a local minimal
   *
   * @param vars an array of [[oscar.cbls.core.computation.CBLSIntVar]] defining the search space
   * @param degree the number of variables to change randomly
   * @param searchZone a subset of the indices of vars to consider.
   *                   If none is provided, all the array will be considered each time
   * @param valuesToConsider: the set of values to consider for the given variable
   * @param name the name of the neighborhood
   */
  def randomizeNeighborhood(vars:Array[CBLSIntVar],
                            degree:Int = 1,
                            name:String = "RandomizeNeighborhood",
                            searchZone:() => SortedSet[Int],
                            valuesToConsider:(CBLSIntVar,Int) => Iterable[Int] = (variable,_) => variable.domain)
  = RandomizeNeighborhood(vars,degree,name,searchZone,valuesToConsider)

  /**
   * will randomize the array, by performing swaps only.
   *
   * @param vars an array of [[oscar.cbls.core.computation.CBLSIntVar]] defining the search space
   * @param degree the number of variables to change randomly
   * @param searchZone a subset of the indices of vars to consider.
   *                   If none is provided, all the array will be considered each time
   * @param name the name of the neighborhood
   */
  def randomSwapNeighborhood(vars:Array[CBLSIntVar],
                             degree:Int = 1,
                             name:String = "RandomSwapNeighborhood",
                             searchZone:() => SortedSet[Int] = null)
  = RandomSwapNeighborhood(vars,degree,name,searchZone)

  /**
   * will iteratively swap the value of two different variables in the array
   *
   * @param vars an array of [[oscar.cbls.core.computation.CBLSIntVar]] defining the search space
   * @param searchZone1 a subset of the indices of vars to consider for the first moved point
   *                   If none is provided, all the array will be considered each time
   * @param searchZone2 a subset of the indices of vars to consider for the second moved point
   *                   If none is provided, all the array will be considered each time
   * @param symmetryCanBeBrokenOnIndices if set to true, the neighborhood will break symmetries on indices of swapped vars
   *                            that is: the first variable will always have an indice strictly smaller than the second swapped variable
   *                            typically, you always want it except if you have specified one or two searchZones, and they are different
   * @param symmetryCanBeBrokenOnValue if set to true, the neighborhood will break symmetries on values of swapped vars
   *                            that is: thee first variable will always have a value strictly smaller than the value of second swapped variable
   *                            you do not want to have both symmetryCanBeBrokenOnIndices and symmetryCanBeBrokenOnValue
   * @param name the name of the neighborhood
   * @param symmetryClassOfVariables1 a function that input the ID of a variable and returns a symmetry class;
   *                      for each role of the move, ony one of the variable in each class will be considered for the vars in searchZone1
   *                      this makes search faster
   *                      Int.MinValue is considered different to itself
   *                      if you set to None this will not be used at all
   * @param symmetryClassOfVariables2 a function that input the ID of a variable and returns a symmetry class;
   *                      for each role of the move, ony one of the variable in each class will be considered for the vars in searchZone2
   *                      this makes search faster
   *                      Int.MinValue is considered different to itself
   *                      if you set to None this will not be used at all
   * @param hotRestart  if true, the exploration order in case you ar not going for the best
   *                    is a hotRestart for the first swapped variable
   *                    even if you specify a searchZone that is: the exploration starts again
   *                    at the position where it stopped, and consider the indices in increasing order
   *                    if false, consider the exploration range in natural order from the first position.
   **/
  def swapsNeighborhood(vars:Array[CBLSIntVar],
                        name:String = "SwapsNeighborhood",
                        searchZone1:()=>Iterable[Int] = null,
                        searchZone2:()=>Iterable[Int] = null,
                        symmetryCanBeBrokenOnIndices:Boolean = true,
                        symmetryCanBeBrokenOnValue:Boolean = false,
                        best:Boolean = false,
                        symmetryClassOfVariables1:Option[Int => Int] = None,
                        symmetryClassOfVariables2:Option[Int => Int] = None,
                        hotRestart:Boolean = true)
  = SwapsNeighborhood(vars,name,searchZone1,searchZone2,
    symmetryCanBeBrokenOnIndices,symmetryCanBeBrokenOnValue,
    best,symmetryClassOfVariables1,symmetryClassOfVariables2,hotRestart)



  /**
   * will randomize the array, by performing shuffle on a subset of the variables
   * This will not consider the objective function, even if it includes some strong constraints
   *
   * @param vars an array of [[oscar.cbls.core.computation.CBLSIntVar]] defining the search space
   * @param indicesToConsider the positions to consider in the shuffle, all positions if not specified
   * @param numberOfShuffledPositions the number of positions to shuffle, taken in indicesToConsider.
   * @param name the name of the neighborhood
   * @param checkNoMoveFound checks that the variables to shuffle have different values, return NoMoveFound if this is not the case
   */
  def shuffleNeighborhood(vars:Array[CBLSIntVar],
                          indicesToConsider:()=>Iterable[Int] = null,
                          numberOfShuffledPositions:() => Int = () => Int.MaxValue,
                          name:String = "ShuffleNeighborhood",
                          checkNoMoveFound:Boolean = true) =
    ShuffleNeighborhood(vars, indicesToConsider, numberOfShuffledPositions, name, checkNoMoveFound)

  /**
    * will shift a block of value to the right(doing it also to the left is redundant)
    *
    * @param vars an array of [[oscar.cbls.core.computation.CBLSIntVar]] defining the search space
    * @param searchZone1 a subset of the indices of vars to consider in order to determine the block's extremities
    *                   if none provided, all the array will be considered each time
    * @param maxShiftSize the max size of the shift, given the first indice considered in the shift
    * @param maxOffsetLength the max size of the offset
    * @param best if true, the neighborhood will try to find the best solution possible
    *             (not very usefull because browsing all the possibilities can be very long)
    * @param name the name of the neighborhood
    * @param hotRestart  if true, the exploration order in case you ar not going for the best
    *                    is a hotRestart for the first swapped variable
    *                    even if you specify a searchZone that is: the exploration starts again
    *                    at the position where it stopped, and consider the indices in increasing order
    *                    if false, consider the exploration range in natural order from the first position.
    */
  def shiftNeighborhood(vars:Array[CBLSIntVar],
                        name:String = "ShiftNeighborhood",
                        searchZone1:()=>Iterable[Int] = null,
                        maxShiftSize:Int = Int.MaxValue,
                        maxOffsetLength:Int = Int.MaxValue,
                        best:Boolean = false,
                        hotRestart:Boolean = true) =
    ShiftNeighborhood(vars, name, searchZone1, maxShiftSize, maxOffsetLength, best, hotRestart)

  def rollNeighborhood(vars:Array[CBLSIntVar],
                       name:String = "RollNeighborhood",
                       searchZone:()=>Set[Int] = null,
                       bridgeOverFrozenVariables:Boolean = false,
                       maxShiftSize:Int=>Int = _ => Int.MaxValue, //the max size of the roll, given the ID of the first variable
                       best:Boolean = false,
                       hotRestart:Boolean = true) =
    RollNeighborhood(vars, name, searchZone, bridgeOverFrozenVariables, maxShiftSize, best, hotRestart)

}

