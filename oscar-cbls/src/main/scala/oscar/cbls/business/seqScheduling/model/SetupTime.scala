package oscar.cbls.business.seqScheduling.model

/**
  * This class represents a specification of a setup time (time for changing
  * running modes) between two running modes
  *
  * @param m0 the initial running mode
  * @param m1 the final running mode
  * @param time the time for changing between m0 and m1
  */
case class SetupTime(m0: RunningMode, m1: RunningMode, time: Int)

/**
  * This class represents an instance of setup time between two running modes
  * for a resource
  *
  * @param resourceName the name of the involved resource
  * @param m0 the initial running mode
  * @param m1 the final running mode
  * @param startTime the starting time of the setup mode
  * @param duration the duration of the setup mode
  */
case class SetupTimeInfo(resourceName: String,
                         m0: RunningMode,
                         m1: RunningMode,
                         startTime: Int,
                         duration: Int)
