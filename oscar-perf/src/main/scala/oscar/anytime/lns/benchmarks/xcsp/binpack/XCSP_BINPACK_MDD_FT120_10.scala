package oscar.anytime.lns.benchmarks.xcsp.binpack

import oscar.anytime.lns.models.XCSP

object XCSP_BINPACK_MDD_FT120_10 extends App{
  new XCSP("data/xcsp3/binpack/BinPacking-mdd-ft120-10.xml", 5).main(args)
}
