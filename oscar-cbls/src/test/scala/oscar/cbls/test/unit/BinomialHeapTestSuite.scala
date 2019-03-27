package oscar.cbls.test.unit

import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FunSuite, Matchers}
import oscar.cbls.algo.heap.{BinomialHeap, BinomialHeapWithMoveLong}

import scala.util.Random

class BinomialHeapTestSuite extends FunSuite with GeneratorDrivenPropertyChecks with Matchers {

  val gen: Gen[Operation] = Gen.frequency(
    (10, Insert()),
    (6, RemoveFirst()),
    (4, Delete())
  )
  val genOperations1: Gen[List[Operation]] = for {
    size <- Gen.chooseNum(30, 100)
    list <- Gen.listOfN(size, gen)
  } yield list

  test("BinomialHeapWithMoveLong : Batch operations keep expected heap") {
    forAll(genOperations1,minSuccessful(100)) { operations: List[Operation] =>
      whenever(operations.size > 5) {
        val keys = Random.shuffle(operations.indices.toList ::: operations.indices.toList) // Just to have duplicated keys
        var values = List[Int]()
        val maxcount = operations.count(_ == Insert()) + 1
        val heap = new BinomialHeapWithMoveLong(i => keys(i), maxcount, keys.max)

        for (operation <- operations) {
          operation match {
            case Insert() =>
              val value = Random.nextInt(keys.max)
              if (!values.contains(value)){
                heap.insert(value)
                values = value :: values
              }

            case Delete() =>
              if (!heap.isEmpty) {
                val value = Gen.oneOf(values).sample.get
                heap.delete(value)
                values = removeElem(values, value)
              }

            case RemoveFirst() =>
              if (!heap.isEmpty) {
                val value = heap.removeFirst()
                values = removeElem(values, value)
              }
          }
        }

        var list: List[Int] = List()

        while (!heap.isEmpty) {
          list = heap.removeFirst() :: list
        }

        list.sorted should be(values.sorted)
      }
    }
  }

  val gen2: Gen[Operation] = Gen.frequency(
    (10, Insert()),
    (6, Clear()),
    (4, RemoveFirst()),
    (4, RemoveFirsts())
  )
  val genOperations2: Gen[List[Operation]] = for {
    size <- Gen.chooseNum(30, 100)
    list <- Gen.listOfN(size, gen2)
  } yield list

  test("BinomialHeap : Batch operations keep expected heap") {
    forAll(genOperations1,minSuccessful(100)) { operations: List[Operation] =>
      whenever(operations.size > 5) {
        val keys = Random.shuffle(operations.indices.toList ::: operations.indices.toList)
        var values = List[Int]()
        val maxcount = operations.count(_ == Insert()) + 1
        val heap = new BinomialHeap[Int](i => keys(i), maxcount)

        for (operation <- operations) {
          operation match {
            case Insert() =>
              val value = Random.nextInt(keys.max)
              if (!values.contains(value)){
                heap.insert(value)
                values = value :: values
              }

            case RemoveFirst() =>
              if (!heap.isEmpty) {
                val value = heap.popFirst()
                values = removeElem(values, value)
              }

            case RemoveFirsts() =>
              if(!heap.isEmpty){
                val removed = heap.popFirsts
                values = removeElems(values, removed)
              }


            case Clear() =>
              heap.dropAll()
              values = List[Int]()
          }
        }

        var list: List[Int] = List()

        while (!heap.isEmpty) {
          list = heap.popFirst() :: list
        }

        list.sorted should be(values.sorted)
      }
    }
  }


  def removeElem(list: List[Int], elem: Int): List[Int] = {
    val indexMin = list.indexOf(elem)
    list.take(indexMin) ::: list.takeRight(list.size - indexMin - 1)
  }

  def removeElems(list: List[Int], elems: List[Int]): List[Int] = {
    var tempList = list
    for(i <- elems){
      tempList = removeElem(tempList,i)
    }
    tempList
  }

  abstract sealed class Operation()
  case class Insert() extends Operation
  case class RemoveFirst() extends Operation
  case class RemoveFirsts() extends Operation
  case class Delete() extends Operation
  case class Clear() extends Operation
}