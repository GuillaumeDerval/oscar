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

package oscar.cbls.modeling

import oscar.cbls.invariants.core.computation._
import oscar.cbls.invariants.lib.logic._
import oscar.cbls.invariants.lib.minmax._
import oscar.cbls.invariants.lib.numeric._
import oscar.cbls.invariants.lib.set._

import scala.collection.immutable.SortedMap

trait Invariants
  extends ClusterInvariants
  with ComplexLogicInvariants
  with ElementInvariants
  with MinMaxInvariants
  with NumericInvariants
  with SetInvariants


/**
 * modeling interface presenting the cluster invariants
 * @author renaud.delandtsheer@cetic.be
 */
trait ClusterInvariants{

  def makeSparseCluster[T<:IntValue](values:Array[T], clusters: Iterable[Int]) = Cluster.MakeSparse(values, clusters)

  def makeDenseCluster[T<:IntValue](values:Array[T]) = Cluster.MakeDense(values)

  def makeDenseClusterAssumingMinMax[T<:IntValue](values:Array[T],themin:Int,themax:Int) = Cluster.MakeDenseAssumingMinMax(values,themin,themax)

  /**maintains a cluster of the indexes of array:  cluster(j) = {i in index of values | values[i] == j}
    * This is considered as a sparse cluster because Cluster is a map and must not cover all possibles values of the values in the array ''values''
    * */
  def sparseCluster[T<:IntValue](values:Array[T], Clusters:SortedMap[Int,CBLSSetVar]) = SparseCluster(values, Clusters)

  /**Maintains a cluster of the indexes of array: cluster(j) = {i in index of values | values[i] == j}
    * This is considered as a dense cluster because Cluster is an array and must cover all the possibles values of the values in the array ''values''
    * */
  def denseCluster[T<:IntValue](values:Array[T], clusters:Array[CBLSSetVar]) = DenseCluster(values, clusters)

  /**
   * Maintains a count of the indexes of array: count(j) = #{i in index of values | values[i] == j}
    * This is considered as a dense count because counts is an array and must cover all the possibles values of the values in the array ''values''
    * */
  def denseCount(values:Array[IntValue], counts:Array[CBLSIntVar]) = DenseCount(values, counts)

  /**
   * Maintains the reverse references. Referencing(i) = {j | Reference(j) includes i}
    * */
  def denseRef(references:Array[SetValue], referencing:Array[CBLSSetVar]) = DenseRef(references, referencing)

  /**
   * Maintains a resource usage profile.
   * @param indices the indices of tasks
   * @param start the start time of tasks
   * @param duration the duration of tasks
   * @param amount the amount that tasks use of this resource
   * @param profile the usage profile of the resource maintained to profile(time) <== sum(task.amount | task.start <= time <= t.start+t.duration)
   * @param active the tasks that are active maintained to active(time) <== (task.indices | task.start <= time <= t.start+t.duration)
   */
  def cumulative(indices:Array[Int], start:Array[IntValue], duration:Array[IntValue], amount:Array[IntValue], profile:Array[CBLSIntVar], active:Array[CBLSSetVar])  =
    Cumulative(indices:Array[Int], start, duration, amount, profile, active)


  /** { i in index(values) | cond(values[i] }
    * @param values is an array of IntVar
    * @param cond is a function that selects values to be includes in the output set.
    * This ''cond'' function cannot depend on any IntVar, as updates to these IntVars will not trigger propagation of this invariant.
    */
  def filter(values:Array[IntValue], cond:(Int=>Boolean) = _ != 0) = Filter(values:Array[IntValue], cond:(Int=>Boolean))

  /** {i in index of values | values[i] <= boundary}
    * It is based on two heap data structure, hence updates are log(n) and all updates are allowed
    * @param values an array of intvar
    * @param boundary the boundary for comparison
    */
  def selectLEHeapHeap(values:Array[IntValue], boundary: IntValue) = SelectLEHeapHeap(values:Array[IntValue], boundary: IntValue)


  /**{i \in index of values | values[i] <= boundary}
    * It is based on a queue for the values above the boundary, hence all updates must be accepted by this scheme:
 - SelectLESetQueue does not allow boundary to decrease
 - SelectLESetQueue does not allow elements above boundary to change
 - SelectLESetQueue requires latest variables passing above boundary to be the biggest one
    * @param values: an array of intvar
    * @param boundary: the boundary for comparison
    */
  def selectLESetQueue(values:Array[IntValue], boundary: IntValue) = SelectLESetQueue(values, boundary)

}

/**
 * modeling interface presenting the complex logic invariants
 * @author renaud.delandtsheer@cetic.be
 */
trait ComplexLogicInvariants{

  /**this invariants maintains data structures representing vrp of vehicles.
    * for use in TSP, VRP, etc.
    * arrays start at one until N
    * position 0 is to denote an unrouted node.
    * The nodes from 1 to V are the starting points of vehicles.
    *
    * @param V the number of vrp to consider V>=1 and V<=N
    */
  def routes(V: Int, Next:Array[IntValue]) = Routes.buildRoutes(Next, V)

  /**maintains a sorting of the ''values'' array:
    * @param ReversePerm   i < j => values(ReversePerm(i)) < values(ReversePerm(j))
    * see method GetForwardPerm() for the forward permutation: ReversePerm(ForwardPerm(i)) == i
    * */
  def sort(values:Array[IntValue], ReversePerm:Array[CBLSIntVar]) = new Sort(values, ReversePerm)

  /**returns the ForwardPerm for a given array
    * It instantiates an array of the appropriate size and populates it with IntVar.
    */
  def makeSort(values:Array[IntValue]) = Sort.MakeSort(values)
}


/**
 * modeling interface presenting the element invariants
 * @author renaud.delandtsheer@cetic.be
 */
trait ElementInvariants{
  /** if (ifVar >0) then thenVar else elveVar
    * @param ifVar the condition (IntVar)
    * @param thenVar the returned value if ifVar > 0
    * @param elseVar the returned value if ifVar <= 0
    * */
  def intITE(ifVar:IntValue, thenVar:IntValue, elseVar:IntValue, pivot: Int = 0) = IntITE(ifVar, thenVar, elseVar, pivot)

  /** inputarray[index]
    * @param inputarray is an array of IntVar
    * @param index is the index accessing the array*/
  def intElement(index:IntValue, inputarray:Array[IntValue]) = IntElement(index:IntValue, inputarray:Array[IntValue])

  /**Union(i in index) (array[i])
    * @param index is an IntSetVar denoting the set of positions in the array to consider
    * @param inputarray is the array of intvar that can be selected by the index
    */
  def intElements(index:SetValue, inputarray:Array[IntValue]) = Elements(index, inputarray)

  /** inputarray[index] on an array of IntSetVar
    * @param inputarray is the array of intsetvar
    * @param index is the index of the array access
    **/
  def intSetElement(index:IntValue, inputarray:Array[SetValue]) = SetElement(index, inputarray)
}

