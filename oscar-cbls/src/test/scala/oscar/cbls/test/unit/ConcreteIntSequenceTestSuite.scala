package oscar.cbls.test.unit

import org.scalacheck.Gen
import org.scalatest.{FunSuite, Matchers}
import TestUtils._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import oscar.cbls.algo.seq.{ConcreteIntSequence, IntSequence}

import scala.util.Random


/**
  * Test suite to handle cases that were not covered in InvariantTests
  * Some of these tests start with whenever(list.size > n). This prevents
  * IllegalArgumentExceptions when dealing with random indices using the size
  * of the list as an upper bound.
  *
  * Most of these tests use a referenceList that undergoes the same modifications as the IntSequence, then
  * the tests end with a comparison between values of the referenceList and the IntSequence
  *
  * Crucial tests may use forAll(minSuccessful(m)) with a large m value to force a large exploration of possibilities
  */
class ConcreteIntSequenceTestSuite extends FunSuite with GeneratorDrivenPropertyChecks with Matchers {

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

  test("smallestValue returns expected value"){
    forAll((referenceArray: Array[Long]) => {
      whenever(referenceArray.nonEmpty){
        val seq = IntSequence(referenceArray)
        seq.toIterable
      }
    })
  }



  test("isEmpty returns expected value on empty list"){
    var seq = IntSequence(List())
    seq.isEmpty should be (true)
  }

  test("isEmpty returns expected value after removing all elements"){
    var seq = IntSequence(0L until 100)

    for(i <- 0 until 100){
      seq = seq.delete(0)
    }

    seq.isEmpty should be (true)
  }

  test("nbOccurrences returns expected value"){
    forAll(elemListGen){list => {

      var referenceList = list
      var seq = IntSequence(list)

      for(i <- list.min until list.max){
        seq.nbOccurrence(i) should be (referenceList.count(_ == i))
      }
    }}
  }

  test("contains returns expected value"){
    forAll(elemListGen){list => {

      var referenceList = list
      var seq = IntSequence(list)

      for(i <- 0 until 20){
        val (indexFrom,indexTo,destination) = getRandomParametersForMoveAfter(referenceList)
        seq = seq.moveAfter(indexFrom, indexTo, destination, true)
        referenceList = flipListManually(referenceList,indexFrom,indexTo,destination)
      }

      for(i <- list.min until list.max){
        seq.contains(i) should be (referenceList.contains(i))
      }
    }}
  }

  test("valueAtPosition returns expected value"){
    forAll(elemListGen){list => {
      whenever(list.size > 3){
        var referenceList = list
        var seq = IntSequence(list)

        for(i <- 0 until 20){
          val (indexFrom,indexTo,destination) = getRandomParametersForMoveAfter(referenceList)
          seq = seq.moveAfter(indexFrom, indexTo, destination, true)
          referenceList = flipListManually(referenceList,indexFrom,indexTo,destination)
        }

        for(i <- referenceList.indices){
          seq.valueAtPosition(i).get should be (referenceList(i))
        }
      }
    }}
  }

  test("nbOccurrences after random pivots returns expected value"){
    forAll(elemListGen){list => {
      whenever(list.size > 3){
        var referenceList = list
        var seq = IntSequence(list)

        for(i <- 0 until 20){
          val (indexFrom,indexTo,destination) = getRandomParametersForMoveAfter(referenceList)
          seq = seq.moveAfter(indexFrom, indexTo, destination, true)
          referenceList = flipListManually(referenceList,indexFrom,indexTo,destination)
        }

        for(i <- list.min until list.max){
          seq.nbOccurrence(i) should be (referenceList.count(_ == i))
        }
      }
    }}
  }

  test("insertAtPosition inserts expected elements"){
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

  test("moveAfter once applies expected transformation"){
    forAll (minSuccessful(500)) { referenceList :List[Long] => {
      whenever(referenceList.size > 5){

        var seq = IntSequence(referenceList)
        val (indexFrom,indexTo,destination) = getRandomParametersForMoveAfter(referenceList)
        seq = seq.moveAfter(indexFrom, indexTo, destination, true)
        val flippedList = flipListManually(referenceList,indexFrom,indexTo,destination)

        seq.toList should be (flippedList)
      }
    }}
  }

  test("moveAfter batch applies expected transformation"){
    forAll (minSuccessful(1500)) { referenceList :List[Long] => {
      whenever(referenceList.size > 5){

        var seq = IntSequence(referenceList)
        var flippedList = referenceList

        for(i <- 0 until 20){
          val (indexFrom,indexTo,destination) = getRandomParametersForMoveAfter(flippedList)
          seq = seq.moveAfter(indexFrom, indexTo, destination, true)
          flippedList = flipListManually(flippedList,indexFrom,indexTo,destination)
        }

        seq.toList should be (flippedList)
      }
    }}
  }

  test("undorderedContentNoDuplicates yields expected list"){
    forAll{ referenceList :List[Long] => {
      whenever(referenceList.size > 5){

        var seq = IntSequence(referenceList)
        var flippedList = referenceList

        for(i <- 0 until 20){
          val (indexFrom,indexTo,destination) = getRandomParametersForMoveAfter(flippedList)
          seq = seq.moveAfter(indexFrom, indexTo, destination, true)
          flippedList = flipListManually(flippedList,indexFrom,indexTo,destination)
        }

        seq.unorderedContentNoDuplicate.sorted should be (flippedList.sorted.distinct)
      }
    }}
  }

  test("undorderedContentNoDuplicatesWithNBOccurences yields expected list"){
    forAll{ referenceList :List[Long] => {
      whenever(referenceList.size > 5){

        var seq = IntSequence(referenceList)
        var flippedList = referenceList

        for(i <- 0 until 20){
          val (indexFrom,indexTo,destination) = getRandomParametersForMoveAfter(flippedList)
          seq = seq.moveAfter(indexFrom, indexTo, destination, true)
          flippedList = flipListManually(flippedList,indexFrom,indexTo,destination)
        }

        seq.unorderedContentNoDuplicateWithNBOccurences.sorted
          .should(be
            (flippedList.sorted.distinct.map(e => (e,flippedList.count(_ == e))))
          )
      }
    }}
  }

  test("regularization on sequence keeps expected values"){
    var seq = IntSequence(0L to 100)
    var referenceList = (0L to 100).toList

    // Swap both lists randomly
    for(i <- 0 to 100){
      val (indexFrom,indexTo,destination) = getRandomParametersForMoveAfter(referenceList)
      seq = seq.moveAfter(indexFrom, indexTo, destination, true)
      referenceList = flipListManually(referenceList,indexFrom,indexTo,destination)
    }

    seq = seq.regularize()
    seq.toList should be (referenceList)
  }

  val elem = for (n <- Gen.choose(0, 100)) yield n * 4L // Sparse elements
  val elemListGen =  for {
    numElems <- Gen.choose(20, 1000)
    elems <- Gen.listOfN(numElems, elem)
  } yield elems
}

object TestUtils{

  /**
    * Implements manually the moveAfter transformation (to compare with IntSequence)
    * @param list The original list to swap
    * @param indexFrom The starting index of the subsequence to flip (inclusive)
    * @param indexTo The ending index of the subsequence to flip (inclusive)
    * @param destination The index where the subsequence must be re-inserted (equivalent to moveAfter parameter)
    * @return A new sequence with the proper transformation
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
        // Insert the flip at the left
        val part1 = start.take(destination + 1)
        val part2 = start.takeRight(indexFrom - destination - 1)
        flippedList = part1 ::: flip ::: part2 ::: end
      }
      else{
        // Insert the flip at the right
        val part1 = end.take(destination - indexTo)
        val part2 = end.takeRight(list.size - destination - 1)
        flippedList = start ::: part1 ::: flip ::: part2
      }
    }

    flippedList
  }

  def getRandomParametersForMoveAfter(list :List[Long]): (Int, Int, Int) = {
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
