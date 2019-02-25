package oscar.cbls.test.unit

import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FunSuite, Matchers}
import oscar.cbls.algo.dll.DoublyLinkedList

import scala.util.Random

class DLLTestSuite extends FunSuite with GeneratorDrivenPropertyChecks with Matchers {

  test("Dequeuing doesn't set the size negative"){

    val dll = new DoublyLinkedList[Int]()

    dll.enqueue(1)
    dll.dequeue()
    dll.dequeue()
    dll.dequeue()

    dll.size should be >= 0
  }

  test("Iterator gives the same list after enqueuing"){

    forAll((intList: List[Int]) => {

      val dll = new DoublyLinkedList[Int]()

      for(i <- intList){
        dll.enqueue(i)
      }

      dll.iterator.zip(intList.iterator).forall(x => x._1 == x._2) &&
        (dll.iterator.length == intList.iterator.length)
    })
  }

  test("Iterator gives the same list reversed after addElem"){

    forAll((intList: List[Int]) => {

      val dll = new DoublyLinkedList[Int]()

      for(i <- intList){
        dll.addElem(i)
      }

      val revList = intList.reverse

      dll.iterator.zip(revList.iterator).forall(x => x._1 == x._2) &&
        (dll.iterator.length == revList.iterator.length)
    })
  }

  test("DropAll sets the size to 0 and sets isEmpty"){

    forAll((intList: List[Int]) => {

      val dll = new DoublyLinkedList[Int]()

      for(i <- intList){
        dll.addElem(i)
      }

      dll.dropAll()
      dll.size == 0 && dll.isEmpty
    })
  }

  test("addElem, dequeue and enqueue keep the correct size (batch)"){

    var helper = new CRUDHelpers[Int]()
    val dll = new DoublyLinkedList[Int]()
    var size = 0

    for(i <- 1 to 100){
      val a = helper.operationList(Random.nextInt(helper.operationList.size))
      size = a(dll,i,size)
    }

    size should be (dll.size)
  }

  class CRUDHelpers[A]{
    val add: (DoublyLinkedList[A], A, Int) => Int = (dll: DoublyLinkedList[A], i: A, size: Int) => {
      dll.addElem(i)
      size+1
    }
    val enqueue: (DoublyLinkedList[A], A, Int) => Int = (dll: DoublyLinkedList[A], i: A, size:Int) => {
      dll.enqueue(i)
      size+1
    }
    val dequeue: (DoublyLinkedList[A], A, Int) => Int = (dll: DoublyLinkedList[A], i: A, size:Int) => {
      if(size == 0)
        size
      else
      {
        dll.dequeue()
        size-1
      }
    }
    val operationList = List(add,enqueue,dequeue)
  }
}
