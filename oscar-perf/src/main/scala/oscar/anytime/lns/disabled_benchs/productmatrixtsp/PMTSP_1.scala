package oscar.anytime.lns.disabled_benchs.productmatrixtsp

import oscar.anytime.lns.models.ProductMatrixTSP

/**
  * Created by pschaus on 8/05/17.
  */
object PMTSP_1 extends App {

  new ProductMatrixTSP("data/pmtsp/pmtsp-1.txt", 5048).main(args)

}
