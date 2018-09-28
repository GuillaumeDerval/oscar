package oscar.cbls.lib.invariant.graph

abstract sealed class IncrementalDistanceResult(from:Node)

case class Distance(from:Node,
                    to:Node,
                    distance:Int,
                    requiredConditions:Set[Int],
                    unlockingConditions:Set[Int]) extends IncrementalDistanceResult(from)

case class NeverConnected(from:Node) extends IncrementalDistanceResult(from)

case class NotConnected(from:Node,
                        to:Node,
                        unlockingConditions:Set[Int]) extends IncrementalDistanceResult(from)
