package oscar.modeling.misc

/**
  * Compute a simple moving average
  */
class ExponentialMovingAverage(alpha: Double) {
  var current = 0.0
  var first = true

  def update(newVal: Double): Double = {
    if(first)
      current = newVal
    else
      current = alpha*newVal+(1.0-alpha)*current
    first = false
    current
  }

  def get: Double = current
}
