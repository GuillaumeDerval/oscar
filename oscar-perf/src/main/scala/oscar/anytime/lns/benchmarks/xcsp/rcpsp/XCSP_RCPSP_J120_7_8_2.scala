package oscar.anytime.lns.benchmarks.xcsp.rcpsp

import oscar.anytime.lns.models.XCSP

object XCSP_RCPSP_J120_7_8_2 extends App{
  new XCSP("data/xcsp3/rcpsp/Rcpsp-j120-08-02.xml", 101).main(args)
}
