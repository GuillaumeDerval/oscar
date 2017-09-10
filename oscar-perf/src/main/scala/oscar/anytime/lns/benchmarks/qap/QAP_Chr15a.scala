package oscar.anytime.lns.benchmarks.qap

import oscar.anytime.lns.models.QuadraticAssignment

object QAP_Chr15a extends App {

  //http://anjos.mgi.polymtl.ca/qaplib/inst.html
  new QuadraticAssignment("data/qap/chr15a.dat.txt",9896).main(args)

}
