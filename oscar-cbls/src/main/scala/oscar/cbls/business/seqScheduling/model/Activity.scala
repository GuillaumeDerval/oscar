package oscar.cbls.business.seqScheduling.model

import oscar.cbls._

/**
  * This class represents an Activity (Task, Step, ...) in the scheduling
  * model
  *
  * @param name a readable name for the activity
  * @param valDuration the initial duration of the activity
  */
class Activity(m: Store,
               val name: String,
               val valDuration: Int) {
  // index in activities array in the schM model
  var index: Int = Constants.NO_INDEX
  // CBLS variable for activity duration
  val durationCBLS = new CBLSIntVar(m, valDuration, 0 to Int.MaxValue, s"${name}__duration")

  /**
    * Activity duration
    * @return the current duration of the activity
    */
  def duration: Int = durationCBLS.value
}

object Activity {
  def setIndex(act: Activity, index: Int): Unit = {
    act.index = index
  }
}