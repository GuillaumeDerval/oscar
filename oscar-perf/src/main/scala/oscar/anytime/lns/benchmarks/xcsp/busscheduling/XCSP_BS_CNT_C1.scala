package oscar.anytime.lns.benchmarks.xcsp.busscheduling

import oscar.anytime.lns.models.XCSP

object XCSP_BS_CNT_C1 extends App{
  new XCSP("data/xcsp3/busScheduling/BusScheduling-cnt-c1.xml", 32).main(args)
}
