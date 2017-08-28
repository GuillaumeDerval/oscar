package oscar.anytime.lns.benchmarks.atsp

import oscar.anytime.lns.models.{ATSP, TSP}

object FT53 extends App {
  new ATSP("data/ATSP/ft53.atsp", 6905).main(args)
}




