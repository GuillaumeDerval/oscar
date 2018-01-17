package oscar.cbls.test.routing.testcumul

import oscar.cbls.business.routing.invariants.cumul.nonStackable.VehicleCapacity
import oscar.cbls.core.computation.{CBLSIntVar, CBLSSeqVar, Store}
import oscar.cbls.core.propagation.ErrorChecker


/**
  * @author Quentin Meurisse
  */
object TestVehicleCapacity extends App {
  val m = Store(checker = Some(ErrorChecker()))

  val v = 2
  val n = 18
  val deltaAtNode = Array(3, 0, 1, 2, -4, -3, 6, 1, -1, 2, -2, 1, 3, 2, -1, 2, -3, 0)
  val violation = Array(CBLSIntVar(m), CBLSIntVar(m))
  val contentAtEndOfVehicleRoute = Array(CBLSIntVar(m, value = 3), CBLSIntVar(m))
  val maxCapacity = 4
  val routes = new CBLSSeqVar(m, initialValue = IntSequence(0 until v), maxVal = n - 1, n = "Routes")

  val inv = VehicleCapacity(routes, v, deltaAtNode, maxCapacity, violation, contentAtEndOfVehicleRoute)

  m.close()

  print("Insert from scratch")
  for (i <- 8 to 2 by -1) {
    routes.insertAtPosition(i, 1)
  }
  for (i <- 14 to 9 by -1) {
    routes.insertAtPosition(i, 9)
  }
  routes.defineCurrentValueAsCheckpoint(true)
  m.propagate()
  println("\tok\n")

  print("insert node with delta = 0 with pre-compute")
  routes.insertAtPosition(17, 6)
  m.propagate()
  println("\tok\n")

  routes.rollbackToTopCheckpoint(inv.checkpoint)

  print("Insert node with delta > 0 with pre-compute")
  routes.insertAtPosition(15, 4)
  m.propagate()
  println("\tok\n")

  routes.rollbackToTopCheckpoint(inv.checkpoint)

  print("Insert node with delta < 0 with pre-compute")
  routes.insertAtPosition(16, 2)
  m.propagate()
  println("\tok\n")

  routes.rollbackToTopCheckpoint(inv.checkpoint)

  print("Remove node with delta > 0 with pre-compute")
  routes.remove(9)
  m.propagate()
  println("\tok\n")

  print("Remove node with delta < 0 with pre-compute")
  routes.remove(4)
  m.propagate()
  println("\tok\n")

  print("Remove node from scratch")
  routes.remove(9)
  m.propagate()
  println("\tok\n")

  print("Remove last node from scratch")
  routes.remove(6)
  m.propagate()
  println("\tok\n")

  routes.rollbackToTopCheckpoint(inv.checkpoint)

  print("Flip a segment with pre-compute" )
  routes.flip(2, 5)
  m.propagate()
  println("\tok\n")

  print("Flip a segment with afterValue < v with pre-compute")
  routes.flip(9, 11)
  m.propagate()
  println("\tok\n")

  print("Flip segment from scratch")
  routes.flip(1, 3)
  routes.flip(5, 7)
  m.propagate()
  println("\tok\n")

  routes.rollbackToTopCheckpoint(inv.checkpoint)

  print("Move a segment with sumDelta > 0 upwards without flip with pre-compute")
  routes.move(1, 2, 5, flip = false)
  m.propagate()
  println("\tok\n")

  print("Move a segment with sumDelta < 0 downwards without flip with pre-compute")
  routes.move(10, 11, 8, flip = false)
  m.propagate()
  println("\tok\n")

  routes.rollbackToTopCheckpoint(inv.checkpoint)

  print("Move a segment with sumDelta < 0 upwards without flip with pre-compute")
  routes.move(2, 4, 6, flip = false)
  m.propagate()
  println("\tok\n")

  print("Move a segment with sumDelta > 0 downwards without flip with pre-compute")
  routes.move(12, 14, 8, flip = false)
  m.propagate()
  println("\tok\n")

  print("Move a segment upwards without flip from scratch")
  routes.move(9, 11, 14, flip = false)
  m.propagate()
  println("\tok\n")

  print("Move a segment downwards without flip from scratch")
  routes.move(4, 6, 1, flip = false)
  m.propagate()
  println("\tok\n")

  routes.rollbackToTopCheckpoint(inv.checkpoint)

  print("Move a segment upwards with flip with pre-compute")
  routes.move(2, 4, 6, flip = true)
  m.propagate()
  println("\tok\n")

  print("Move a segment downwards with flip with pre-compute")
  routes.move(12, 14, 8, flip = true)
  m.propagate()
  println("\tok\n")

  print("Move a segment upwards with flip from scratch")
  routes.move(9, 11, 14, flip = true)
  m.propagate()
  println("\tok\n")

  print("Move a segment downwards with flip from scratch")
  routes.move(4, 6, 1, flip = true)
  m.propagate()
  println("\tok\n")

  routes.rollbackToTopCheckpoint(inv.checkpoint)

  print("Move a segment with sumDelta < 0 from vehicle 1 to vehicle 0 without flip with pre-compute")
  routes.move(10, 11, 4, flip = false)
  m.propagate()
  println("\tok\n")

  routes.rollbackToTopCheckpoint(inv.checkpoint)

  print("Move a segment with sumDelta > 0 from vehicle 1 to vehicle 0 without flip with pre-compute")
  routes.move(10, 12, 4, flip = false)
  m.propagate()
  println("\tok\n")

  print("Move a segment from vehicle 0 to vehicle 1 without flip from scratch")
  routes.move(5, 7, 12, flip = false)
  m.propagate()
  println("\tok\n")

  routes.rollbackToTopCheckpoint(inv.checkpoint)

  print("Move a segment from vehicle 0 to vehicle 1 without flip with pre-compute for remove the segment form vehicle 0 and fom scratch form insert")
  routes.insertAtPosition(16, 15)
  m.propagate()
  routes.move(5, 7, 10, flip = false)
  m.propagate()
  println("\tok\n")

  routes.rollbackToTopCheckpoint(inv.checkpoint)

  print("Move a segment from vehicle 1 to vehicle 0 with flip with pre-compute")
  routes.move(10, 12, 4, flip = true)
  m.propagate()
  println("\tok\n")

  print("Move a segment from vehicle 0 to vehicle 1 with flip from scratch")
  routes.move(5, 7, 12, flip = true)
  m.propagate()
  println("\tok\n")

}
