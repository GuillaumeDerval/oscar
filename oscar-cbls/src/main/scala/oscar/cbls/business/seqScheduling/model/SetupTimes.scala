package oscar.cbls.business.seqScheduling.model

/**
  * This case class represents a setup time for changing a running mode in a resource
  */
case class SetupTimeData(resourceIndex: Int,
                         modeFromInd: Int,
                         modeToInd: Int,
                         startTime: Int,
                         duration: Int)

case class SetupTimeData_B(resourceIndex: Int,
                           modeFromInd: Option[RunningMode_B],
                           modeToInd: Option[RunningMode_B],
                           startTime: Int,
                           duration: Int)

/**
  * This is a container class for setup times
  */
class SetupTimes {
  var setupTimesList: List[SetupTimeData] = List()

  def reset(): Unit = {
    setupTimesList = List()
  }

  def addSetupTime(st: SetupTimeData): Unit = {
    setupTimesList = st::setupTimesList
  }

  override def toString: String = {
    val stringBuilder = new StringBuilder
    stringBuilder.append(s"SetupTimes (Total: ${setupTimesList.length}): \n")
    for {st <- setupTimesList} {
      stringBuilder.append(s"* $st \n")
    }
    stringBuilder.toString
  }
}


/**
  * This is a container class for setup times
  */
class SetupTimes_B {
  var setupTimesList: List[SetupTimeData_B] = List()

  def reset(): Unit = {
    setupTimesList = List()
  }

  def addSetupTime(st: SetupTimeData_B): Unit = {
    setupTimesList = st::setupTimesList
  }

  override def toString: String = {
    val stringBuilder = new StringBuilder
    stringBuilder.append(s"SetupTimes (Total: ${setupTimesList.length}): \n")
    for {st <- setupTimesList} {
      stringBuilder.append(s"* $st \n")
    }
    stringBuilder.toString
  }
}