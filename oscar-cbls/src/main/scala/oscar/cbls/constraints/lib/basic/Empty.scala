package oscar.cbls.constraints.lib.basic

import oscar.cbls.constraints.core.Constraint
import oscar.cbls.invariants.core.computation._
import oscar.cbls.invariants.lib.set.Cardinality

/**
 * implements \exists i, i \in set
 * @author gael.thouvenin@student.umons.ac.be
 */
case class Empty(set: SetValue) extends Constraint {
  private val content = EQ(Cardinality(set), 0)

  override def violation(v: Value): IntValue = content.violation(v)

  override def violation: IntValue = content.violation
}
