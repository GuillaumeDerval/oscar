package oscar.anytime.lns.disabled_benchs.jobshop

import oscar.anytime.lns.models.JobShop

object JobShopLa17 extends App {

  new JobShop("data/jobshop/Lawrence/la17.txt", 784).main(args)

}
