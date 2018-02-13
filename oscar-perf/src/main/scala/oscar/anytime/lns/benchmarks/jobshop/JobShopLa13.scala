package oscar.anytime.lns.benchmarks.jobshop

import oscar.anytime.lns.models.JobShop

object JobShopLa13 extends App {

  new JobShop("data/jobshop/Lawrence/la13.txt", 1150).main(args)

}
