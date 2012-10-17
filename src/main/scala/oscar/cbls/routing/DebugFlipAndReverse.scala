import oscar.cbls.invariants.core.computation.{IntVar, Model}
import oscar.cbls.routing.VRP
/**
 * Created with IntelliJ IDEA.
 * User: Florent
 * Date: 17/10/12
 * Time: 14:12
 * To change this template use File | Settings | File Templates.
 */

object DebugFlipAndReverse extends App{
  val m: Model = new Model(false,true,false,false)
  val vrp =  new VRP (7,1,m)

  // make a cycle 0-1-2-3-4-5-6-0 (circle)
  vrp.Next(0):=1
  vrp.Next(1):=2
  vrp.Next(2):=3
  vrp.Next(3):=4
  vrp.Next(4):=5
  vrp.Next(5):=6
  vrp.Next(6):=0
  println("VRP:\n"+vrp)

  //easy flip the circle
  /*
  vrp.reverseSegmentListToUpdate(0,6).foreach(t => t._1 := t._2)
  vrp.Next(0):=6
  */

  vrp.flipVariablesToUpdate(3,2,5,6).foreach(t => t._1 := t._2)
  println("VRP after flip:\n"+vrp)










}