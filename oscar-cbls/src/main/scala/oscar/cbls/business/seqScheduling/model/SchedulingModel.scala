package oscar.cbls.business.seqScheduling.model

class SchedulingModel__A(val maxActivities: Int,
                         val maxResources: Int,
                         val maxModes: Int) {
  // Activities
  var nbActivities: Int = 0
  val activities: Array[Activity__A] = new Array[Activity__A](maxActivities)
  // Resources
  var nbResources: Int = 0
  val resources: Array[Resource__A] = new Array[Resource__A](maxResources)
  // Running Modes
  var nbModes: Int = 0
  val runningModes: Array[RunningMode__A] = new Array[RunningMode__A](maxModes)
  // Precedences
  val precedences: Precedences = new Precedences(maxActivities)

  def addActivity(act: Activity__A): Unit = {
    require(nbActivities < maxActivities)
    activities(nbActivities) = act
    act.index = nbActivities
    nbActivities += 1
  }

  def addResource(res: Resource__A): Unit = {
    require(nbResources < maxResources)
    resources(nbResources) = res
    res.index = nbResources
    nbResources += 1
  }

  def addRunningMode(rm: RunningMode__A): Unit = {
    require(nbModes < maxModes)
    runningModes(nbModes) = rm
    rm.index = nbModes
    nbModes += 1
  }

  def addPrecedence(act1: Activity__A, act2: Activity__A): Unit = {
    precedences.addPrecedence(act1.index, act2.index)
  }

  def getPriorityList: List[Int] = precedences.getPriorityList
}



//////////////////////////////////////
/////////////////////////// DEPRECATED
//////////////////////////////////////


/**
  * This class is a model of a scheduler based on priority lists
  *
  * @param numActivities the number of activities to schedule
  */
class SchedulingModel(val numActivities: Int) {
  // Activities
  var activities: Array[Activity] = new Array(numActivities)
  // Resources
  var resources: List[Resource] = List()
  // Running modes
  var runningModes: List[RunningMode] = List()
  // Precedences
  val precedences: Precedences = new Precedences(numActivities)
  // Number of activities actually encoded
  var nbActivities = 0

  /**
    * Adds a running mode to the model
    *
    * @param nameRM the name of the running mode
    * @param defaultSetupTime the default setup time for the running mode
    * @return the added running mode
    */
  def addRunningMode(nameRM: String, defaultSetupTime: Int): RunningMode = {
    val newRM = ResourceMode(nameRM, defaultSetupTime)
    if (!runningModes.contains(newRM)) { runningModes = newRM::runningModes }
    newRM
  }

  /**
    * Adds a resource to the model
    *
    * @param res the resource to be added
    */
  def addResource(res: Resource): Unit = {
    if (!resources.contains(res)) { resources = res::resources }
  }

  /**
    * Adds an activity to the model
    *
    * @param act the activity to be added
    * @return the index of the added activity. If the activity was already in
    *         the model, it returns the position in the activities vector
    */
  def addActivity(act: Activity): Int = {
    if (nbActivities < numActivities) {
      if (activities.contains(act)) {
        activities.indexOf(act)
      } else {
        val indAct = nbActivities
        act.index = indAct
        activities(indAct) = act
        nbActivities += 1
        indAct
      }
    } else {
      throw new IllegalArgumentException("Attempt to add an Activity in a full model")
    }
  }

  /**
    * Adds the usage of a resource by an activity
    *
    * @param act the activity
    * @param res the resource
    * @param capacity the used capacity of the resource by activity
    * @param runningMode the running mode of the resource in the activity
    */
  def addUsageResourceActivity(act: Activity,
                               res: Resource,
                               capacity: Int,
                               runningMode: RunningMode): Unit = {
    require(activities.contains(act))
    require(resources.contains(res))
    require(runningModes.contains(runningMode))
    require(act.index != Activity.NO_INDEX)
    require(capacity >= 0)
    /////
    // add resource to activity
    act.resources :+= res
    // Add resource usage into the resource
    res.usageActivities(act.index) = ActivityResource(capacity, runningMode)
  }

  /**
    * Adds a precedence between two activities act1, act2
    *
    * @param act1 the activity preceding act2
    * @param act2 the activity preceded by act1
    */
  def addPrecedenceActivities(act1: Activity, act2: Activity): Unit = {
    require(activities.contains(act1))
    require(activities.contains(act2))
    /////
    val ind1 = activities.indexOf(act1)
    val ind2 = activities.indexOf(act2)
    precedences.addPrecedence(ind1, ind2)
  }

  /**
    * Gets a priority list of activity indices according to precedences in
    * the model
    *
    * @return a list of activity indices respecting the precedence relation
    */
  def getPriorityList: List[Int] = precedences.getPriorityList

  /**
    * Computes the schedule (relative start times for each activity) according
    * to the model and a priority list. Algorithm based on C. Pralet's paper
    *
    * @param priorityList a list of activity indices respecting the precedence
    *                     relation
    * @return an array representing the relative start dates for each activity
    *         a list of setup times for running modes, which indicates when a
    *         setup time had to be done
    */
  def getSchedule(priorityList: List[Int]): (Array[Int], List[SetupTimeInfo]) = {

    /////
    // Initialization
    val startTimes: Array[Int] = Array.tabulate(numActivities)(_ => 0)
    var setupTimes: List[SetupTimeInfo] = Nil

    println(s"Initial Start Times: [${startTimes.foldLeft("")((s, t) => s"$s $t ")}]")
    println(s"Activities durations: [${activities.foldLeft("")((s,a) => s"$s ${a.duration} ")}]")

    // main loop
    for {i <- priorityList.indices} {
      val indexActivity = priorityList(i)
      val resourcesActI = activities(indexActivity).resources
      val precedencesActI = precedences.precArray(indexActivity)

      println(s"******* Analyzing Activity $indexActivity ********")
      println(s"Resources for activity: {${resourcesActI.foldLeft("")((s,r) => s"$s ${r.name} ")}}")
      println(s"Precedences for activity: {${precedencesActI.foldLeft("")((s,p) => s"$s $p ")}}")

      // compute maximum of start+duration for preceding activities
      val maxEndTimePrecsActI = if (precedencesActI.isEmpty) 0
      else {
        precedencesActI.map(precInd => startTimes(precInd)+activities(precInd).duration).max
      }

      println(s"Maximum duration of preceding activities = $maxEndTimePrecsActI")

      // compute maximum of starting times for availability of resources
      var maxStartTimeAvailableResActI = 0
      for {res <- resourcesActI} {
        // update last activity index
        res.lastActivityIndex = indexActivity
        val lastModeRes = res.lastRunningMode
        val newModeRes = res.usageActivities(indexActivity).runningMode
        val lastResFlows = res.forwardFlows
        val resourceQty = res.usageActivities(indexActivity).capacity
        if (lastModeRes == newModeRes) {
          // running mode for resource in activity has not changed
          maxStartTimeAvailableResActI = math.max(maxStartTimeAvailableResActI,
                                                  ResourceFlow.getEndTimeResourceFlows(resourceQty, lastResFlows))
        } else {
          // running mode for resource in activity changed
          res.changedRunningMode = Some(res.lastRunningMode)
          res.lastRunningMode = newModeRes
          val lastEndTime = ResourceFlow.getLastEndTimeResourceFlow(lastResFlows)
          val setupTimeMode = res.setupTimeChangeMode(lastModeRes, newModeRes)
          maxStartTimeAvailableResActI = math.max(maxStartTimeAvailableResActI,
                                                  lastEndTime+setupTimeMode)
          // a new setup activity must be added
          setupTimes ::= SetupTimeInfo(res.name, lastModeRes, newModeRes, lastEndTime, setupTimeMode)
        }
      }
      // now we can update the start time for the activity
      startTimes(indexActivity) = math.max(maxEndTimePrecsActI, maxStartTimeAvailableResActI)
      // update map of resource flows for all resources. It requires another
      // loop on the resources
      for {res <- resourcesActI} {
        val lastResFlows = res.forwardFlows
        val resourceQty = res.usageActivities(indexActivity).capacity
        res.forwardFlows = ResourceFlow.addResourceFlowToList(
          activities(indexActivity),
          indexActivity,
          resourceQty,
          startTimes,
          ResourceFlow.flowQuantityResource(
            resourceQty,
            if (res.changedRunningMode.isDefined) {
              // running mode has changed
              val lastEndTime = ResourceFlow.getLastEndTimeResourceFlow(lastResFlows)
              val setupTimeMode = res.setupTimeChangeMode(res.changedRunningMode.get, res.lastRunningMode)
              List(SetupFlow(lastEndTime, setupTimeMode, res.maxCapacity))
            } else {
              lastResFlows
            }
          )
        )
      }

      println(s"Start Times after activity $indexActivity : [${startTimes.foldLeft("")((s, t) => s"$s $t ")}]")
    }
    (startTimes, setupTimes)
  }

  override def toString: String = {
    val strActivities = activities.foldLeft("Activities: ")((str, act) => s"$str\n$act")
    val strResources = resources.foldLeft("Resources: ")((str, res) => s"$str\n Resource:\n$res")
    s"SchedulingModel:\n$strActivities\n$strResources\n$runningModes\n$precedences\n"
  }
}
