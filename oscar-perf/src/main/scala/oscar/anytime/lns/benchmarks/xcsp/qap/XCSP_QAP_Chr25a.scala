package oscar.anytime.lns.benchmarks.xcsp.qap

import oscar.anytime.lns.models.XCSP

object XCSP_QAP_Chr25a extends App{
  new XCSP("data/xcsp3/qap/QuadraticAssignment-chr25a.xml",3796).main(args)
}
