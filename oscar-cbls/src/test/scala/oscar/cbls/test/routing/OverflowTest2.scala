package oscar.cbls.test.routing
/*
import oscar.cbls._
import oscar.cbls.algo.seq.IntSequence

object OverflowTest2 extends App{
  val m = new Store()

  val n = 10
  val v = 1

  val route = new CBLSSeqVar(m,
    initialValue = IntSequence(0 until n),  //0 1 2 3 4 5 6 7 8 9
    maxVal = n-1,
    n = "route")

  val routeContent = content(route).setName("routeContent")
  m.registerForPartialPropagation(routeContent)

  val routeContent2 = content(route.createClone(1)).setName("routeContent2")

  m.close()

  def doIt() {
    //val checkpoint00 = route.defineCurrentValueAsCheckpoint(true)

    route.remove(6)

    //val checkpoint0 = route.defineCurrentValueAsCheckpoint(true)

    route.remove(7)

    println(routeContent)
    println(routeContent2)

    val checkpoint01 = route.defineCurrentValueAsCheckpoint(true)

    route.insertAtPosition(5,7)
    route.remove(6)

    println(routeContent)

    route.rollbackToTopCheckpoint(checkpoint01)
    route.releaseTopCheckpoint()

    route.remove(6)
    val checkpoint02 = route.defineCurrentValueAsCheckpoint(true)

    //route.rollbackToTopCheckpoint(checkpoint0)
    route.insertAtPosition(5,5)

    println(routeContent)

    route.rollbackToTopCheckpoint(checkpoint02)
    route.releaseTopCheckpoint()

    route.insertAtPosition(7,5)
    println(routeContent)

    val checkpoint03 = route.defineCurrentValueAsCheckpoint(true)

    route.remove(5)
    println(routeContent2)

    //route.releaseTopCheckpoint()
    route.releaseTopCheckpoint()
  }

  doIt()
  route.insertAtPosition(9,6)
  route.insertAtPosition(9,6)
  doIt()
  println(routeContent)

}

*/