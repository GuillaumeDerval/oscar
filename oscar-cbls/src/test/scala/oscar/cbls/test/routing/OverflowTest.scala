package oscar.cbls.test.routing
/*
import oscar.cbls._
import oscar.cbls.algo.seq.IntSequence

object OverflowTest extends App{
  val m = new Store()

  val n = 10
  val v = 1

  val route = new CBLSSeqVar(m,
    initialValue = IntSequence(0 until n),  //0 1 2 3 4 5 6 7 8 9
    maxVal = n-1,
    n = "route")

  val routeContent = content(route).setName("routeContent")
  m.registerForPartialPropagation(routeContent)

  val routeContent2 = content(route.createClone(1).createClone(1)).setName("routeContent2")

  m.close()

  def explore1(moved:Int,inBetweens:List[()=>Unit]):Unit = {
    val checkpoint = route.defineCurrentValueAsCheckpoint(true)
    var currentMoved = moved
    for(inBetween <- inBetweens){
      route.move(currentMoved,currentMoved,9,false)
      inBetween()
      route.rollbackToTopCheckpoint(checkpoint)
      currentMoved += 1
    }
    route.releaseTopCheckpoint()
  }

  def checkPartial(){
    println("check partial")
    println(route)
    println(routeContent)
  }
  def checkTotal(){
    println("check total")
    checkPartial()
    println(route)
    println(routeContent2)
  }

  explore1(1,List(() => {
    explore1(4,List(() =>
      explore1(3,List(checkPartial))))
    explore1(2,List(() =>
      explore1(3,List(checkTotal))))
  }))

  route.move(1,1,9,false)
  route.move(2,2,9,false)
  route.move(3,3,9,false)


  explore1(3,List(
    () => explore1(2,List(checkPartial)),
    () => explore1(3,List(checkTotal)))
  )
  route.move(1,1,9,false)

}
*/