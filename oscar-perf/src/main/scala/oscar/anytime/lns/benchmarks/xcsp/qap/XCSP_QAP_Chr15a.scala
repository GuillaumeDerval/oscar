package oscar.anytime.lns.benchmarks.xcsp.qap

import oscar.anytime.lns.models.XCSP

object XCSP_QAP_Chr15a extends App{
  new XCSP("data/xcsp3/qap/QuadraticAssignment-chr15a.xml",9896).main(args)
}
