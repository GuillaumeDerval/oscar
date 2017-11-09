package oscar.cbls.business.routing.invariants.cumul.test

import oscar.cbls.algo.seq.IntSequence
import oscar.cbls.business.routing.invariants.capa.ForwardCumulativeConstraintOnVehicle
import oscar.cbls.business.routing.invariants.cumul.nonStackable.VehicleCapacity
import oscar.cbls.core.computation.{CBLSIntVar, CBLSSeqVar, Store}

/**
  * Created by Quentin Meurisse on 11/09/17.
  */
object VehicleCapacityVsCumulative {

  val m = Store()
  val v = 2
  val n = 2200
  var delta = Array[Int]()
  for(i <- 0 until n){
    val x = if (i < v) IntGenerator(0, 5)
    else IntGenerator(-5, 5)
    delta = delta :+ x
  }
  val violationCapacity = Array.tabulate(v)(vehicle => CBLSIntVar(m, name = "violation of vehicle" + vehicle))
  val contentAtEndOfVehicleRoute = Array.tabulate(v)(vehicle => CBLSIntVar(m, value = delta(vehicle), name = "content at end of vehicle" + vehicle))

  val maxCapacity = 15
  val routes1 = new CBLSSeqVar(m, initialValue = IntSequence(0 until v), maxVal = n - 1, n = "Routes with vehicleCapacity")
  val vehicleCapacity = new VehicleCapacity(routes1, v, delta, maxCapacity, violationCapacity, contentAtEndOfVehicleRoute)

  val m2 = Store()
  val violationCumulative = new CBLSIntVar(m2, 0, 0 to Int.MaxValue, "violation of capacity test")


  val routes2 = new CBLSSeqVar(m2, initialValue = IntSequence(0 until v), maxVal = n - 1, n = "Routes with forward cumulative constraint")

  val cumulative = new ForwardCumulativeConstraintOnVehicle(
    routes2,
    n,
    v,
    {case (fromNode, toNode, content) => content + delta(fromNode)},
    maxCapacity,
    Array.tabulate(v)(delta),
    violationCumulative,
    1)

  m.close()
  m2.close()

  for (i <- 2000 to 1001 by -1) {
    routes1.insertAtPosition(i, 2)
    routes2.insertAtPosition(i, 2)
    if(i % 100 == 0){
      m.propagate()
      m2.propagate()
    }
  }
  for(i <- 1000 to 2 by -1){
    routes1.insertAtPosition(i, 1)
    routes2.insertAtPosition(i, 1)
    if (i % 100 == 0){
      m.propagate()
      m2.propagate()
    }
  }
  m.propagate()
  m2.propagate()
  routes1.defineCurrentValueAsCheckpoint(true)
  routes2.defineCurrentValueAsCheckpoint(true)

  val formatter = java.text.NumberFormat.getInstance

  def run()={
    print("\tinsert point with vehicleCapacity: ")
    var startTime = System.nanoTime()
    routes1.insertAtPosition(2111, 5)
    m.propagate()
    println(formatter.format(System.nanoTime() - startTime) + " ns")

    print("\tinsert point with cumulative: ")
    startTime = System.nanoTime()
    routes2.insertAtPosition(2017, 5)
    m2.propagate()
    println(formatter.format(System.nanoTime() - startTime) + " ns")

    routes1.rollbackToTopCheckpoint(vehicleCapacity.checkpoint)
    routes2.rollbackToTopCheckpoint(routes2.getTopCheckpoint)

    print("\tremove point with vehicleCapacity: ")
    startTime = System.nanoTime()
    routes1.remove(5)
    m.propagate()
    println(formatter.format(System.nanoTime() - startTime) + " ns")

    print("\tremove point with cumulative ")
    startTime = System.nanoTime()
    routes2.remove(5)
    m2.propagate()
    println(formatter.format(System.nanoTime() - startTime) + " ns")

    routes1.rollbackToTopCheckpoint(vehicleCapacity.checkpoint)
    routes2.rollbackToTopCheckpoint(routes2.getTopCheckpoint)

    print("\tflip with vehicleCapacity: ")
    startTime = System.nanoTime()
    routes1.flip(1, 999)
    m.propagate()
    println(formatter.format(System.nanoTime() - startTime) + " ns")

    print("\tflip with cumulative: ")
    startTime = System.nanoTime()
    routes2.flip(1, 999)
    m2.propagate()
    println(formatter.format(System.nanoTime() - startTime) + " ns")

    routes1.rollbackToTopCheckpoint(vehicleCapacity.checkpoint)
    routes2.rollbackToTopCheckpoint(routes2.getTopCheckpoint)

    print("\tmove segment with vehicleCapacity: ")
    startTime = System.nanoTime()
    routes1.move(1, 500, 900, flip = false)
    m.propagate()
    println(formatter.format(System.nanoTime() - startTime) + " ns")

    print("\tmove segment with cumulative: ")
    startTime = System.nanoTime()
    routes2.move(1, 500, 900, flip = false)
    m2.propagate()
    println(formatter.format(System.nanoTime() - startTime) + " ns")

    routes1.rollbackToTopCheckpoint(vehicleCapacity.checkpoint)
    routes2.rollbackToTopCheckpoint(routes2.getTopCheckpoint)

    print("\tmove segment to another vehicle with vehicleCapacity: ")
    startTime = System.nanoTime()
    routes1.move(1, 900, 1900, flip = false)
    m.propagate()
    println(formatter.format(System.nanoTime() - startTime) + " ns")

    print("\tmove segment to another vehicle with cumulative: ")
    startTime = System.nanoTime()
    routes2.move(1, 900, 1900, flip = false)
    m2.propagate()
    println(formatter.format(System.nanoTime() - startTime) + " ns")

    routes1.rollbackToTopCheckpoint(vehicleCapacity.checkpoint)
    routes2.rollbackToTopCheckpoint(routes2.getTopCheckpoint)
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
