package oscar.anytime.lns.benchmarks.rcpsp

import oscar.anytime.lns.models._

/**
  * Created by pschaus on 8/05/17.
  */
object RCPSP_J120_7_10 extends App {

  // http://people.eng.unimelb.edu.au/pstuckey/rcpsp/
  new RCPSP("data/rcpsp/j120/j120_7_10.rcp",111).main(args)

}
