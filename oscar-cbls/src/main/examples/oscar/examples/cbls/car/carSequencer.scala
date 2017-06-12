package oscar.examples.cbls.car

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
import oscar.cbls.core.objective.Objective
import oscar.cbls.core.search.SwapMove
import oscar.cbls.lib.search.combinators.{BestSlopeFirst, Profile}
import oscar.cbls.lib.search.neighborhoods.{RollNeighborhood, WideningFlipNeighborhood}
import oscar.cbls.modeling.CBLSModel

import scala.collection.immutable.SortedMap
import scala.language.postfixOps
import scala.util.Random
/**
 * Created by rdl on 29-01-16.
 */
object carSequencer  extends CBLSModel with App {

  val orderedCarsByType:SortedMap[Int,Int] = SortedMap(0 -> 90, 1 -> 60, 2 -> 110 , 3 -> 120, 4 -> 40, 5 -> 30)
  val carTypes = 0 to 5

  println("carSequencing")
  println("orderedCarTypes:" + orderedCarsByType)

  //option types
  //   A   G   D   E
  //0  T   T   T   F
  //1  F   T   T   F
  //2  T   F   T   F
  //3  F   F   F   T
  //4  T   T   F   T
  //5  F   T   F   T

  def makeBoolArray(values:Int*):Array[Boolean] = {
    val toReturn = Array.fill(carTypes.end +1)(false)
    values.foreach(toReturn(_) = true)
    toReturn
  }
  val airCoCarTypes = makeBoolArray(0,2,4)
  val automaticGearBoxCarTypes = makeBoolArray(0,1,4,5)
  val dieselCarTypes = makeBoolArray(0,1,2)
  val espCarTypes = makeBoolArray(3,4,5)

  val maxType = orderedCarsByType.keys.max
  val minType = orderedCarsByType.keys.min
  val typeRange = minType to maxType
  def prependItems(acc:List[Int],n:Int,item:Int):List[Int] = (if(n == 0) acc else prependItems(item :: acc,n-1,item))
  val orderedCarTypes:List[Int] = orderedCarsByType.foldLeft(List.empty[Int])({case (accList,(carType,nbItems)) => prependItems(accList,nbItems,carType)})
  val nbCars = orderedCarTypes.size

  println("totalNumberOfCars:" + nbCars)

  //initializes the car sequence in a random way
  val orderedCarTypesIterator = Random.shuffle(orderedCarTypes).toIterator
  val carSequence:Array[CBLSIntVar] = Array.tabulate(nbCars)(p => CBLSIntVar(orderedCarTypesIterator.next(),typeRange,"carClassAtPosition" + p)).toArray

  //airConditionner: max 2 out of 3
  c.post(sequence(carSequence,3,2,airCoCarTypes))

  //automaticGearBox: max 3 out of 5
  c.post(sequence(carSequence,5,3,automaticGearBoxCarTypes))

  //diesel: max 3 out of 5
  c.post(sequence(carSequence,5,3,dieselCarTypes))

  //esp: max 2 ouf of 3
  c.post(sequence(carSequence,3,2,espCarTypes))

  val impactZone = 5

  val varViolation = c.violations(carSequence)
  val violatedCars = filter(varViolation)
  val mostViolatedCars = argMax(varViolation)

  println("closing model")

  c.close
  val obj:Objective = c.violation

  s.close()

  val swap = swapsNeighborhood(carSequence,"swapCars")
  val rollViolated = RollNeighborhood(carSequence, name = "RollViolatedCars", maxShiftSize = _ => 20, searchZone = violatedCars)
  val roll = RollNeighborhood(carSequence, name = "RollAllCars", maxShiftSize = _ => 10)
  val mostViolatedSwap = swapsNeighborhood(carSequence,"mostViolatedSwap", searchZone2 = (_,_) => mostViolatedCars.value, symmetryCanBeBrokenOnIndices = false)
  val shiftNeighbor = shiftNeighborhood(carSequence, searchZone1 =() => violatedCars.value.toList, maxShiftSize = carSequence.length/2/*, maxOffsetSize = carSequence.length/2*/, hotRestart = true)
  val rollNeighbor = rollNeighborhood(carSequence)

  val linkedDoubleSwaps =
    swapsNeighborhood(carSequence,"swapCars1") dynAndThen(
      ((swapMove:SwapMove) => {
        val indices = List(swapMove.idI, swapMove.idJ)
        swapsNeighborhood(carSequence, "swapCars2", searchZone1 = () => indices, symmetryCanBeBrokenOnIndices = false, symmetryCanBeBrokenOnValue = true)
      })) name "linkedDoubleSwaps"

  val doubleSwaps = (swapsNeighborhood(carSequence,"swapCars1") andThen swapsNeighborhood(carSequence,"swapCars2")) name "doubleSwaps"

  val looselyLinkedDoubleSwaps =
    swapsNeighborhood(carSequence,"swapCars1", symmetryCanBeBrokenOnIndices = false) dynAndThen(
      ((swapMove:SwapMove) => {
        val firstSwappedCar = 0.max(swapMove.idI - impactZone) until (nbCars).min(swapMove.idI + impactZone)
        val otherSwappedCar = (nbCars-1).min(swapMove.idJ+1) until nbCars
        swapsNeighborhood(carSequence, "swapCars2", searchZone1 = () => firstSwappedCar, searchZone2 = (_,_) => otherSwappedCar, symmetryCanBeBrokenOnIndices = false)
      })) name "looselyLinkedDoubleSwaps"

  val search2 = (
    BestSlopeFirst(List(Profile(mostViolatedSwap), Profile(roll), Profile(WideningFlipNeighborhood(carSequence, maxFlipSize = impactZone *2, name = "FlipMax10Cars", minFlipSize = 4))))
      onExhaustRestartAfter(Profile(shuffleNeighborhood(carSequence, mostViolatedCars, name = "shuffleMostViolatedCars")) guard(() => mostViolatedCars.value.size > 2), 5, obj)
      orElse Profile(shiftNeighbor)
      //      exhaust
      onExhaustRestartAfter(Profile(shuffleNeighborhood(carSequence, violatedCars, name = "shuffleSomeViolatedCars", numberOfShuffledPositions = () => violatedCars.value.size/2)), 2, obj)
      orElse (Profile(shuffleNeighborhood(carSequence, name = "shuffleAllCars")) maxMoves 4)
      saveBestAndRestoreOnExhaust obj)


  val search3 =
    (Profile(mostViolatedSwap) exhaust Profile(WideningFlipNeighborhood(carSequence))
  onExhaustRestartAfter(Profile(shuffleNeighborhood(carSequence, violatedCars, name = "shuffleSomeViolatedCars", numberOfShuffledPositions = () => violatedCars.value.size/2)), 2, obj)
  orElse (Profile(shuffleNeighborhood(carSequence, name = "shuffleAllCars")) maxMoves 4)
  saveBestAndRestoreOnExhaust obj)

  val search4 = Profile(WideningFlipNeighborhood(carSequence))

  val search = search3

  search.verbose = 1
  search.doAllMoves(_ => c.isTrue,obj)

  println(search.profilingStatistics)

  println(c.violation)
  println("car sequence:" + carSequence.map(_.value).mkString(","))

  println("grouped:" + carSequence.map(_.value).toList.groupBy[Int]((c:Int) => c).mapValues((l:List[Int]) => l.size))

  println(if(c.violation.value == 0) "problem solved" else "PROBLEM COULD NOT BE SOLVED")

  def checkGrouped(): Unit ={
    val groupedValuesInSlution = carSequence.map(_.value).toList.groupBy[Int]((c:Int) => c).mapValues((l:List[Int]) => l.size)
    for((carType,count) <- groupedValuesInSlution){
      require(orderedCarsByType(carType) == count, "car count changed on car type " + carType)
    }
  }
}
