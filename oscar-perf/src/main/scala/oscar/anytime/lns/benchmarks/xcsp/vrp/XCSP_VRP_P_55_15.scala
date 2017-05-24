package oscar.anytime.lns.benchmarks.xcsp.vrp

import oscar.anytime.lns.models.XCSP

object XCSP_VRP_P_55_15 extends App{
  new XCSP("data/xcsp3/vrp/Vrp-P-n55-k15.xml", 945).main(args)

}
