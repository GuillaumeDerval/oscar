package oscar.anytime.lns.benchmarks.xcsp.rcpsp

import oscar.anytime.lns.models.XCSP

object XCSP_RCPSP_J120_8_5 extends App{
  new XCSP("data/xcsp3/rcpsp/Rcpsp-j120-08-05.xml", 99).main(args)
}
