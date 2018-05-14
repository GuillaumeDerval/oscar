package oscar.cbls.business.seqScheduling.model

/**
  * This class represents an generic activity
  *
  * @param name the name of the activity
  * @param duration the number of time units needed to perform the activity
  */
case class Activity(name: String, duration: Int) {
  var index: Int = Activity.NO_INDEX
  var resources: Vector[Resource] = Vector()
}

/**
  * This object contains basic constants for activities
  */
object Activity {
  final val NO_INDEX = -1
  final val SOURCE = -1
}