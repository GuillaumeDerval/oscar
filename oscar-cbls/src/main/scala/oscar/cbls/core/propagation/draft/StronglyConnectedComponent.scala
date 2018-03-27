package oscar.cbls.core.propagation.draft

import oscar.cbls.algo.quick.QList

class StronglyConnectedComponent(val propagationElements:QList[PropagationElement])
  extends PropagationElement(PropagationImpactCharacteristics.SCCNotificationBehavior) {

  override var layer: Int = _
  override var threadID: Int = _
  override var schedulingHandler: SchedulingHandler = _

}


