/*******************************************************************************
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 ******************************************************************************/

package oscar.modeling.misc

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
