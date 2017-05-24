package oscar.anytime.lns.benchmarks.xcsp.rcpsp

import oscar.anytime.lns.models.XCSP

object XCSP_RCPSP_J120_7_10 extends App{
  new XCSP("data/xcsp3/rcpsp/Rcpsp-j120-07-10.xml", 111).main(args)
}
