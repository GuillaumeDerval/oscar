package oscar.cbls.test.routing.testcumul

import oscar.cbls.algo.seq.IntSequence
import oscar.cbls.business.routing.invariants.cumul.nonStackable.VehicleCapacity
import oscar.cbls.core.computation.{CBLSIntVar, CBLSSeqVar, Store}

import scala.util.Random

/**
  * Created by Quentin Meurisse on 8/09/17.
  */


object IntGenerator{
  /**
    *
    * @param minValueIncluded
    * @param maxValueIncluded
    * @return a random Int in [minValueIncluded, maxValueIncluded]
    */
  def apply(minValueIncluded: Int, maxValueIncluded: Int): Int = {
    val intervalSize = maxValueIncluded - minValueIncluded
    val rand = new Random()
    rand.nextInt(intervalSize+1) + minValueIncluded
  }
}

object VehicleCapacityBench{
  val m = Store()

  val v = 2
  val n = 2200
  var delta = Array[Int]()
  for(i <- 0 until n){
    val x = if (i < v) IntGenerator(0, 5)
    else IntGenerator(-5, 5)
    delta = delta :+ x
  }
  val violation = Array.tabulate(v)(vehicle => CBLSIntVar(m, name = "violation of vehicle" + vehicle))
  val contentAtEndOfVehicleRoute = Array.tabulate(v)(vehicle => CBLSIntVar(m, value = delta(vehicle), name = "content at end of vehicle" + vehicle))


  val maxCapacity = 15
  val routes = new CBLSSeqVar(m, initialValue = IntSequence(0 until v), maxVal = n - 1, n = "Routes")
  val inv = new VehicleCapacity(routes, v, delta, maxCapacity, violation, contentAtEndOfVehicleRoute)
  m.close()

  for (i <- 2000 to 1001 by -1) {
    routes.insertAtPosition(i, 2)
    if(i % 100 == 0) m.propagate()
  }
  for(i <- 1000 to 2 by -1){
    routes.insertAtPosition(i, 1)
    if (i % 100 == 0) m.propagate()
  }
  m.propagate()
  routes.defineCurrentValueAsCheckpoint(true)

  val formatter = java.text.NumberFormat.getInstance

  def run()={

    print("\tinsert point with pre-compute: ")
    var startTime = System.nanoTime()
    routes.insertAtPosition(2111, 5)
    m.propagate()
    println(formatter.format(System.nanoTime() - startTime) + " ns")

    print("\tinsert point from scratch: ")
    startTime = System.nanoTime()
    routes.insertAtPosition(2017, 5)
    m.propagate()
    println(formatter.format(System.nanoTime() - startTime) + " ns")

    routes.rollbackToTopCheckpoint(inv.checkpoint)

    print("\tremove point with pre-compute: ")
    startTime = System.nanoTime()
    routes.remove(5)
    m.propagate()
    println(formatter.format(System.nanoTime() - startTime) + " ns")

    print("\tremove point from scratch: ")
    startTime = System.nanoTime()
    routes.remove(5)
    m.propagate()
    println(formatter.format(System.nanoTime() - startTime) + " ns")

    routes.rollbackToTopCheckpoint(inv.checkpoint)

    print("\tflip with pre-compute: ")
    startTime = System.nanoTime()
    routes.flip(1, 999)
    m.propagate()
    println(formatter.format(System.nanoTime() - startTime) + " ns")

    print("\tflip from scratch: ")
    startTime = System.nanoTime()
    routes.flip(1, 999)
    m.propagate()
    println(formatter.format(System.nanoTime() - startTime) + " ns")

    routes.rollbackToTopCheckpoint(inv.checkpoint)

    print("\tmove segment with pre-compute: ")
    startTime = System.nanoTime()
    routes.move(1, 500, 900, flip = true)
    m.propagate()
    println(formatter.format(System.nanoTime() - startTime) + " ns")

    print("\tmove segment from scratch: ")
    startTime = System.nanoTime()
    routes.move(1, 500, 900, flip = true)
    m.propagate()
    println(formatter.format(System.nanoTime() - startTime) + " ns")

    routes.rollbackToTopCheckpoint(inv.checkpoint)

    print("\tmove segment to another vehicle with pre-compute: ")
    startTime = System.nanoTime()
    routes.move(1, 900, 1900, flip = true)
    m.propagate()
    println(formatter.format(System.nanoTime() - startTime) + " ns")

    print("\tmove segment to another vehicle from scratch: ")
    startTime = System.nanoTime()
    routes.move(1001, 1900, 0, flip = true)
    m.propagate()
    println(formatter.format(System.nanoTime() - startTime) + " ns")

    routes.rollbackToTopCheckpoint(inv.checkpoint)
    println("")
  }

  def main(args: Array[String]): Unit = {
    val nbRun = if(args.length == 0 || args(0).toInt <= 1) 5
    else args(0).toInt
    for(i <- 0 to nbRun) {
      println("run " + i)
      run()
    }
  }

}
