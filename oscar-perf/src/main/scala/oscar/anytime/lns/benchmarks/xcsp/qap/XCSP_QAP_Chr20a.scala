package oscar.anytime.lns.benchmarks.xcsp.qap

import oscar.anytime.lns.models.XCSP

object XCSP_QAP_Chr20a extends App{
  new XCSP("data/xcsp3/qap/QuadraticAssignment-chr20a.xml",2192).main(args)
}
