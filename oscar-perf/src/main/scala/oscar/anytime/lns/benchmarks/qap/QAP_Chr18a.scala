package oscar.anytime.lns.benchmarks.qap

import oscar.anytime.lns.models.QuadraticAssignment

object QAP_Chr18a extends App {

  //http://anjos.mgi.polymtl.ca/qaplib/inst.html
  new QuadraticAssignment("data/qap/chr18a.dat.txt",11098).main(args)

}
