package oscar.anytime.lns.benchmarks.xcsp.qap

import oscar.anytime.lns.models.XCSP

object XCSP_QAP_Chr22a extends App{
  new XCSP("data/xcsp3/qap/QuadraticAssignment-chr22a.xml",6156).main(args)
}
