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

package oscar.visualfx.shape

/**
  * @author Rémi Barralis remi.barralis@yahoo.fr
  */

import scalafx.scene.layout.Region

/**
  * An abstract class that allows to create customs shape
  *
  * @param position the position of the shape
  * @param id the id of shape
  * @param width the width of the region containing the shape
  * @param height the height of the region containing the shape
  * @tparam T the data structure representing the position of the shape
  */
abstract class Shape[T](position: T, id: Int, width: Double, height: Double) extends Region {
  val _id: Int = id
  val _width: Double = width
  val _height: Double = height
  var _pos: T = position

  def pos: T = _pos
  def pos_=(newPosition: T): Unit

  /**
    * Set the css stylesheet that will customs the shape
    *
    * @param path the path of the css file
    */
  def setStyleSheet(path: String): Unit = {
    this.getStylesheets.add(getClass.getResource(path).toExternalForm)
  }
}
