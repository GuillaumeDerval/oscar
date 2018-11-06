package oscar.cbls.business.seqScheduling.model

/**
  * This class contains all the elements of a Scheduling model
  * (Activities, Resources, Running Modes and Precedences)
  * with their associated CBLS variables
  *
  * @param maxActivities maximum number of activities in the model
  * @param maxResources maximum number of resources in the model
  * @param maxModes maximum number of running modes in the model
  */
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

  //TODO: en général, je considèreque ceci est du sur-emballage. Tu peux mettre ces tableaux en paramètre de cette classe et te passer des "add"

  /**
    * Adds an activity to the model
    * @param act the activity to be added
    */
  def addActivity(act: Activity): Unit = {
    require(nbActivities < maxActivities)
    activities(nbActivities) = act
    act.index = nbActivities
    nbActivities += 1
  }

  /**
    * Adds a resource to the model
    * @param res the resource to be added
    */
  def addResource(res: Resource): Unit = {
    require(nbResources < maxResources)
    resources(nbResources) = res
    res.index = nbResources
    nbResources += 1
  }

  /**
    * Adds a running mode to the model
    * @param rm the running mode to be added
    */
  def addRunningMode(rm: RunningMode): Unit = {
    require(nbModes < maxModes)
    runningModes(nbModes) = rm
    rm.index = nbModes
    nbModes += 1
  }

  /**
    * Adds the precedence between two activities act1, act2 to the model: act1->act2
    * Both activities must be already added to the model
    * @param act1 the preceding activity
    * @param act2 the successive activity
    */
  def addPrecedence(act1: Activity, act2: Activity): Unit = {
    require(act1.index != Constants.NO_INDEX && act2.index != Constants.NO_INDEX)
    precedences.addPrecedence(act1.index, act2.index)
  }

  /**
    * Gets a valid priority list from the precedences relation
    *
    * @return a list representing a permutation of [0..nbActivities) where
    *         each element (actI) is an activity index and all indices of successive
    *         activities of the element are found after actI
    */
  def getPriorityList: List[Int] = precedences.getPriorityList

}
