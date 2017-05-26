package oscar.anytime.lns.benchmarks.xcsp.rcpsp

import oscar.anytime.lns.models.XCSP

object XCSP_RCPSP_J120_11_3 extends App{
  new XCSP("data/xcsp3/rcpsp/Rcpsp-j120-11-03.xml", 188).main(args)
}
