package oscar.cp.heuristics

class Hybridization(importance:Double, varH1: Int => Double, varH2:Int => Double) {


  def hybridize(i:Int):Double = {
    importance*varH1(i) + (1-importance)*varH2(i)
  }

}
