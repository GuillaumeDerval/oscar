package oscar.anytime.lns.disabled_benchs.qap

import oscar.anytime.lns.models.QuadraticAssignment

object QAP_Chr22a extends App {

  //http://anjos.mgi.polymtl.ca/qaplib/inst.html
  new QuadraticAssignment("data/qap/chr22a.dat.txt", 6156).main(args)

}
