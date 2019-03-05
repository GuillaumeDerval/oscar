package oscar.cbls.test.unit

import org.scalatest.{FunSuite, Matchers}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import oscar.cbls.algo.seq.{ConcreteIntSequence, IntSequence}

import scala.util.Random


// Test suite to handle cases that were not covered in InvariantTests
class IntSequenceTestSuite extends FunSuite with GeneratorDrivenPropertyChecks with Matchers {

  test("largestValue returns expected value"){
    forAll((referenceArray: Array[Long]) => {
      whenever(referenceArray.nonEmpty){
        val seq = IntSequence(referenceArray)
        seq.asInstanceOf[ConcreteIntSequence].largestValue.get should be(referenceArray.max)
      }
    })
  }

  test("smallestValue returns expected value"){
    forAll((referenceArray: Array[Long]) => {
      whenever(referenceArray.nonEmpty){
        val seq = IntSequence(referenceArray)
        seq.asInstanceOf[ConcreteIntSequence].smallestValue.get should be(referenceArray.min)
      }
    })
  }

  test("insertAtPosition with fast=false inserts expected elements"){
    forAll((referenceArray: Array[Long]) => {
      whenever(referenceArray.nonEmpty){

        var seq = IntSequence(referenceArray)
        val randomIndex = Random.nextInt(referenceArray.size)
        val randomValue = Random.nextInt(1000)

        seq = seq.insertAtPosition(randomValue,randomIndex,false)

        seq.toList(randomIndex) should be (randomValue)
      }
    })
  }

  test("deleteAtPosition with fast=false deletes expected element"){
    forAll((referenceList: List[Long]) => {
      whenever(referenceList.nonEmpty){

        var seq = IntSequence(referenceList)
        val randomIndex = Random.nextInt(referenceList.size)
        seq = seq.delete(randomIndex,false)

        seq.iterator.size should be (referenceList.size - 1)
        // The new list is the referenceList without the item at randomIndex
        seq.toList should be (referenceList.zipWithIndex.filter{case (e,i) => i != randomIndex}
          .map(tuple => tuple._1))
      }
    })
  }

  test("moveAfter applies expected transformation"){
    forAll (minSuccessful(500)) { referenceList :List[Long] => {
      whenever(referenceList.size > 5){

        var seq = IntSequence(referenceList)
        val (indexFrom,indexTo,destination) = getRandomParameters(referenceList)
        seq = seq.moveAfter(indexFrom, indexTo, destination, true)
        val flippedList = flipListManually(referenceList,indexFrom,indexTo,destination)

        seq.toList should be (flippedList)
      }
    }}
  }

  test("regularization on sequence keeps expected values"){
    var seq = IntSequence(0L to 100)
    var referenceList = (0L to 100).toList
    for(i <- 0 to 100){
      val (indexFrom,indexTo,destination) = getRandomParameters(referenceList)
      seq = seq.moveAfter(indexFrom, indexTo, destination, true)
      referenceList = flipListManually(referenceList,indexFrom,indexTo,destination)
    }

    seq = seq.regularize()
    seq.toList should be (referenceList)
  }

  /**
    * Implements manually the moveAfter transformation (to compare with IntSequence)
    * @param list
    * @param indexFrom
    * @param indexTo
    * @param destination
    * @return
    */
  def flipListManually(list :List[Long], indexFrom :Int, indexTo :Int, destination :Int) : List[Long] = {

    var flippedList = List[Long]()
    val flip = list.slice(indexFrom,indexTo+1).reverse
    val start =  list.take(indexFrom)
    val end = if(indexTo < list.size - 1) list.takeRight(list.size - indexTo - 1) else List()

    if(destination == -1){
      flippedList = flip ::: start ::: end
    }
    else {
      if(destination < indexFrom){
        val part1 = start.take(destination + 1)
        val part2 = start.takeRight(indexFrom - destination - 1)
        flippedList = part1 ::: flip ::: part2 ::: end
      }
      else{
        val part1 = end.take(destination - indexTo)
        val part2 = end.takeRight(list.size - destination - 1)
        flippedList = start ::: part1 ::: flip ::: part2
      }
    }

    flippedList
  }

  def getRandomParameters(list :List[Long]): (Int, Int, Int) = {
    val indexFrom = Random.nextInt(list.size - 1)
    val indexTo = indexFrom + Random.nextInt(list.size - indexFrom)
    var destination = -1
    if(Random.nextBoolean()){
      // Insert before indexFrom
      destination = if(indexFrom == 0) -1 else Random.nextInt(indexFrom)
    }
    else {
      // Insert after indexTo (if indexTo is not the very last index)
      destination =
        if(indexTo < list.size - 1)
          indexTo + Random.nextInt(list.size - indexTo - 1) + 1
        else
          -1
    }

    (indexFrom,indexTo,destination)
  }
}
