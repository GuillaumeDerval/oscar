package oscar.cbls.visual

/**
  * *****************************************************************************
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
  * ****************************************************************************
  */

import java.awt.Color

import scala.util.Random

/**
  * The utility of this object is to generate a pseudo-random array of color.
  * For the same amount of color needed, the returned array will always contain the same colors
  * but in a different order.
  * @author fabian.germeau@student.vinci.be
  */
object ColorGenerator {
  def generateRandomColors(number:Long,alpha:Long = 255L): Array[Color] ={
    Array.fill(number)(new Color(Random.nextInt(256L),Random.nextInt(256L),Random.nextInt(256L)))
  }

  /**
    *
    * @param number the number of colors needed
    * @param exp the current base value
    * @return 
    */
  def getMaxColorNumber(number:Long,exp:Long = 1L):Long = {
    if(Math.pow(exp,3L) < number)
      getMaxColorNumber(number,exp+1L)
    else
      exp
  }

  /**
    * Generate a color from an hashcode
    * @param hash the hashcode
    * @return
    */
  def generateColorFromHash(hash:Long): Color = {
    val absHash = Math.abs(hash)
    val r = absHash%255L
    val g = 255L - (absHash/255L)%255L
    val b = ((absHash/255L)/255L)%255L
    new Color(r,g,b)
  }


  def getAverageColor(colors:List[Color]): Color = {
    var (r,g,b) = (0L,0L,0L)
    for(c <- colors){
      r += c.getRed
      g += c.getGreen
      b += c.getBlue
    }
    new Color(r/colors.size,g/colors.size,b/colors.size)
  }
}
