package oscar.anytime.lns.benchmarks.jobshop

import oscar.anytime.lns.models.JobShop

object JobShopLa28 extends App {

  new JobShop("data/jobshop/Lawrence/la28.txt", 1216).main(args)

}
