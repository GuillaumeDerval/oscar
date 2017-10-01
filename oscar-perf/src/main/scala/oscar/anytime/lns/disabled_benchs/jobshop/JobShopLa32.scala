package oscar.anytime.lns.disabled_benchs.jobshop

import oscar.anytime.lns.models.JobShop

object JobShopLa32 extends App {

  new JobShop("data/jobshop/Lawrence/la32.txt", 1850).main(args)

}
