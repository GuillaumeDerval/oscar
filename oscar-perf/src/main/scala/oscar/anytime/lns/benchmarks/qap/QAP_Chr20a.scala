package oscar.anytime.lns.benchmarks.qap

import oscar.anytime.lns.models.QuadraticAssignment

object QAP_Chr20a extends App {

  //http://anjos.mgi.polymtl.ca/qaplib/inst.html
  new QuadraticAssignment("data/qap/chr20a.dat.txt",2192).main(args)

}
