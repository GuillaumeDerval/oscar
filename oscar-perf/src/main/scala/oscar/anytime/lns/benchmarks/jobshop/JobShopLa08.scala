package oscar.anytime.lns.benchmarks.jobshop

import oscar.anytime.lns.models.JobShop

object JobShopLa08 extends App {

  new JobShop("data/jobshop/Lawrence/la08.txt", 863).main(args)

}
