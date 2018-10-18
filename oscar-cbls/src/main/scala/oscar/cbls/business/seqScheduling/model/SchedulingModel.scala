package oscar.cbls.business.seqScheduling.model

import oscar.cbls.core.computation.CBLSIntVar

class SchedulingModel(val maxActivities: Int,
                      val maxResources: Int,
                      val maxModes: Int) {
  // Activities
  var nbActivities: Int = 0
  val activities: Array[Activity] = new Array[Activity](maxActivities)
  // Resources
  var nbResources: Int = 0
  val resources: Array[Resource] = new Array[Resource](maxResources)
  // Running Modes
  var nbModes: Int = 0
  val runningModes: Array[RunningMode] = new Array[RunningMode](maxModes)
  // Precedences
  val precedences: Precedences = new Precedences(maxActivities)

  def addActivity(act: Activity): Unit = {
    require(nbActivities < maxActivities)
    activities(nbActivities) = act
    act.index = nbActivities
    nbActivities += 1
  }

  def addResource(res: Resource): Unit = {
    require(nbResources < maxResources)
    resources(nbResources) = res
    res.index = nbResources
    nbResources += 1
  }

  def addRunningMode(rm: RunningMode): Unit = {
    require(nbModes < maxModes)
    runningModes(nbModes) = rm
    rm.index = nbModes
    nbModes += 1
  }

  def addPrecedence(act1: Activity, act2: Activity): Unit = {
    precedences.addPrecedence(act1.index, act2.index)
  }

  def getPriorityList: List[Int] = precedences.getPriorityList

}
