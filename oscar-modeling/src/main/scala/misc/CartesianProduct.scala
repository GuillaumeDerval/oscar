package misc

import vars.IntVar

object CartesianProduct {
  def compute(vars: Iterable[IntVar]): BigInt = {
    var v = BigInt(1)
    for(i <- vars)
      v *= i.size
    v
  }

  def computeLog(vars: Iterable[IntVar]): Double = {
    var v = 1.0d
    for(i <- vars)
      v += Math.log(i.size.toDouble)
    v
  }
}
