package oscar.cbls.test.unit

import org.scalatest.{FunSuite, Matchers}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import oscar.cbls.algo.seq.IntSequence
import TestUtils._

class ConcreteIntSequenceExplorerTestSuite extends FunSuite with  GeneratorDrivenPropertyChecks with Matchers{

  test("explorer.prev when explorer is on first element returns None"){
    var seq = IntSequence(0L until 100)
    var explorer = seq.explorerAtAnyOccurrence(0).get

    explorer.prev should be (None)
  }

  test("explorer.next when explorer is on last element returns None"){
    var seq = IntSequence(0L to 100)
    var explorer = seq.explorerAtAnyOccurrence(100).get

    explorer.next should be (None)
  }

  test("Iterating on all explorers with .next yields expected elements"){
    forAll{referenceList :List[Long] =>
      whenever(referenceList.nonEmpty){
        var seq = IntSequence(referenceList)
        var explorer = seq.explorerAtFirstOccurrence(referenceList.head)

        referenceList.foreach(e => {
          e should be (explorer.get.value)
          explorer = explorer.get.next
        })
      }
    }
  }

  test("Iterating on all explorers with .prev yields expected elements"){
    forAll{referenceList :List[Long] =>
      whenever(referenceList.nonEmpty){
        var seq = IntSequence(referenceList)
        var explorer = seq.explorerAtLastOccurrence(referenceList.last)

        referenceList.reverse.foreach(e => {
          e should be (explorer.get.value)
          explorer = explorer.get.prev
        })
      }
    }
  }

  test("Iterating on all explorers with .prev on sequence with pivots yields expected elements"){
    forAll{list :List[Long] =>
      whenever(list.size > 3){
        var referenceList = list
        var seq = IntSequence(referenceList)

        // Swap both lists randomly
        for(i <- 0 to 10){
          val (indexFrom,indexTo,destination) = getRandomParametersForMoveAfter(referenceList)
          seq = seq.moveAfter(indexFrom, indexTo, destination, true)
          referenceList = flipListManually(referenceList,indexFrom,indexTo,destination)
        }

        var explorer = seq.explorerAtLastOccurrence(referenceList.last)
        referenceList.reverse.foreach(e => {
          e should be (explorer.get.value)
          explorer = explorer.get.prev
        })
      }
    }
  }

  test("exhaustive iteration on explorer yields expected elements"){
    forAll{list :List[Long] =>
      whenever(list.size > 3){

        var referenceList = list
        var seq = IntSequence(list)

        // Swap both lists randomly
        for(i <- 0 to 10){
          val (indexFrom,indexTo,destination) = getRandomParametersForMoveAfter(referenceList)
          seq = seq.moveAfter(indexFrom, indexTo, destination, true)
          referenceList = flipListManually(referenceList,indexFrom,indexTo,destination)
        }

        val explorerList = seq.zipWithIndex.map(e => (e._2, seq.explorerAtPosition(e._2)))

        // For all (index,explorer)
        for(explorerTuple <- explorerList){

          val index = explorerTuple._1
          var explorerTmp = explorerTuple._2

          // Iterate on all prev
          referenceList.take(index + 1).reverse.foreach(e => {
            e should be (explorerTmp.get.value)
            explorerTmp = explorerTmp.get.prev
          })

          explorerTmp = explorerTuple._2
          // Iterate on all next
          referenceList.takeRight(referenceList.size - index).foreach(e => {
            e should be (explorerTmp.get.value)
            explorerTmp = explorerTmp.get.next
          })
        }
      }
    }
  }
}
