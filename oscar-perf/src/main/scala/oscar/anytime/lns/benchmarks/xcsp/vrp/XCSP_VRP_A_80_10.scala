package oscar.anytime.lns.benchmarks.xcsp.vrp

import oscar.anytime.lns.models.XCSP

object XCSP_VRP_A_80_10 extends App{
  new XCSP("data/xcsp3/vrp/Vrp-A-n80-k10.xml", 1763).main(args)

}
