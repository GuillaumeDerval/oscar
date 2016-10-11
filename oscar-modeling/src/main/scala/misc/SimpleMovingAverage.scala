package misc

/**
  * Compute a simple moving average
  */
class SimpleMovingAverage(size: Int) {
  val values = Array.tabulate(size)(i => 0.0)
  var position = 0
  var full = false
  var current = 0.0

  def update(newVal: Double): Double = {
    if (full) {
      current += newVal - values(position)
      values(position) = newVal
      position = (position + 1) % size
      current / size
    } else {
      current += newVal
      values(position) = newVal
      position = (position + 1) % size
      if (position == 0) {
        full = true
        current / size
      } else {
        current / position
      }
    }
  }

  def get: Double = {
    if (full)
      current / size
    else
      current / position
  }
}
