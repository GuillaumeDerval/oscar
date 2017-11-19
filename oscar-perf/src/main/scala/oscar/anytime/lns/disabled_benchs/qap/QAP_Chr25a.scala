package oscar.anytime.lns.disabled_benchs.qap

import oscar.anytime.lns.models.QuadraticAssignment

object QAP_Chr25a extends App {

  //http://anjos.mgi.polymtl.ca/qaplib/inst.html
  new QuadraticAssignment("data/qap/chr25a.dat.txt", 3796).main(args)

}
