package oscar.examples.cbls.car

import oscar.cbls.invariants.core.computation.CBLSIntVar
import oscar.cbls.modeling.CBLSModel
import oscar.cbls.objective.Objective
import oscar.cbls.search.WideningFlipNeighborhood
import oscar.cbls.search.combinators.Profile

import scala.collection.immutable.SortedMap
import scala.language.postfixOps
import scala.util.Random

/**
 * Created by rdl on 29-01-16.
 */
object carSequencerSwapFlip  extends CBLSModel with App {

  val orderedCarsByType:SortedMap[Int,Int] = SortedMap(0 -> 110, 1 -> 60, 2 -> 110 , 3 -> 120, 4 -> 40, 5 -> 30)
  val carTypes = 0 to 5

  println("carSequencing")
  println("orderedCarTypes:" + orderedCarsByType)

  //option types
  //   A   G   D   E  (airConditionner, automaticGearBox, diesel, esp)
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

  def prependItems(acc:List[Int],n:Int,item:Int):List[Int] = (if(n == 0) acc else prependItems(item :: acc,n-1,item))
  val orderedCarTypes:List[Int] = orderedCarsByType.foldLeft(List.empty[Int])({case (accList,(carType,nbItems)) => prependItems(accList,nbItems,carType)})
  val nbCars = orderedCarTypes.size

  println("totalNumberOfCars:" + nbCars)

  //initializes the car sequence in a random way
  val orderedCarTypesIterator = Random.shuffle(orderedCarTypes).toIterator
  val carSequence:Array[CBLSIntVar] = Array.tabulate(nbCars)(p => CBLSIntVar(orderedCarTypesIterator.next(),carTypes,"carClassAtPosition" + p))

  //airConditionner: max 2 out of 3
  c.post(sequence(carSequence,3,2,airCoCarTypes))

  //automaticGearBox: max 3 out of 5
  c.post(sequence(carSequence,5,3,automaticGearBoxCarTypes))

  //diesel: max 3 out of 5
  c.post(sequence(carSequence,5,3,dieselCarTypes))

  //esp: max 2 ouf of 3
  c.post(sequence(carSequence,3,2,espCarTypes))

  val varViolation = c.violations(carSequence)
  val violatedCars = filter(varViolation)
  val mostViolatedCars = argMax(varViolation)

  println("closing model")

  c.close

  val obj:Objective = c.violation

  s.close()

  val search =
    (Profile(swapsNeighborhood(carSequence,"mostViolatedSwap", searchZone2 = mostViolatedCars, symmetryCanBeBrokenOnIndices = false))
      exhaust Profile(WideningFlipNeighborhood(carSequence)) //it seems useless to try swaps once flip is exhausted, so simple exhaust is used here
      onExhaustRestartAfter(Profile(shuffleNeighborhood(carSequence, mostViolatedCars, name = "shuffleMostViolatedCars")) guard(() => mostViolatedCars.value.size > 2), 2, obj)
      onExhaustRestartAfter(Profile(shuffleNeighborhood(carSequence, violatedCars, name = "shuffleSomeViolatedCars", numberOfShuffledPositions = () => 5 max (violatedCars.value.size/2))), 2, obj)
      onExhaustRestartAfter(Profile(shuffleNeighborhood(carSequence, name = "shuffleMostCars", numberOfShuffledPositions = () => nbCars/2)), 2, obj)
      orElse (Profile(shuffleNeighborhood(carSequence, name = "shuffleAllCars")) maxMoves 4)
      saveBestAndRestoreOnExhaust obj) //in case we do not solve it, we want to restore the best solution anyway

  search.verbose = 1
  search.paddingLength = 150
  search.doAllMoves(_ => c.isTrue,obj)

  println(search.profilingStatistics)

  println("car sequence:" + carSequence.map(_.value).mkString(","))

  println(if(c.violation.value == 0) "problem solved" else ("PROBLEM COULD NOT BE SOLVED: " + c.violation))
}