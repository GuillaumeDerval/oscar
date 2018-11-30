package oscar.cbls.business.seqScheduling.model

case class RunningMode(name: String, defaultSetupTime: Int = 0) {
  // index in the running mode resource array
  var index: Int = Constants.NO_INDEX
}

object RunningMode {
  def setIndex(rm: RunningMode, index: Int): Unit = {
    rm.index = index
  }
}