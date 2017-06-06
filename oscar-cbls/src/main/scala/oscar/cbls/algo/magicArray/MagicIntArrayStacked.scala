package oscar.cbls.algo.magicArray

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


class MagicIntArrayStacked(maxLevel:Int, initVal:(Int => Int), size:Int) extends Iterable[Int]{

  private[this] val levelToArray:Array[Array[Int]] = Array.tabulate(maxLevel+1)(level => if(level == 0) Array.tabulate(size)(initVal) else Array.fill(size)(0))
  private[this] val levelToIsValueChangedAtNextLevel:Array[IterableMagicBoolArray] = Array.tabulate(maxLevel)(level => new IterableMagicBoolArray(size,false))
  private[this] var currentLevel:Int = 0

  def level = currentLevel - 1

  def update(indice:Int,value:Int){
    levelToArray(currentLevel)(indice) = value
    if(currentLevel!=0)levelToIsValueChangedAtNextLevel(currentLevel-1)(indice) = true
  }

  def apply(indice:Int):Int = {
    var attemptLevel = currentLevel
    while(attemptLevel>0){
      val levelBelow = attemptLevel - 1
      if(levelToIsValueChangedAtNextLevel(levelBelow)(indice)){
        return levelToArray(attemptLevel)(indice)
      }else{
        attemptLevel = levelBelow
      }
    }
    levelToArray(0)(indice)
  }

  def pushLevel(){
    require(currentLevel < maxLevel,"MagicIntArrayStacked was declaring with max " + maxLevel + " levels; trying to push more")
    levelToIsValueChangedAtNextLevel(currentLevel).all = false
    currentLevel += 1
  }

  def popLevel(dropChanges:Boolean){
    require(currentLevel > 0,"trying to pop level zero")
    if(dropChanges){
      currentLevel -= 1
    }else{
      //save changes to lower level!
      val newLevel = currentLevel - 1
      for(changedID <- levelToIsValueChangedAtNextLevel(currentLevel-1).indicesAtTrue){
        levelToArray(newLevel)(changedID) = levelToArray(currentLevel)(changedID)
        levelToIsValueChangedAtNextLevel(newLevel)(changedID) = false
      }
      currentLevel = newLevel
    }
  }

  def cloneTopArray:Array[Int] = {
    Array.tabulate(size)(this(_))
  }

  override def iterator : Iterator[Int] = {
    cloneTopArray.iterator
  }

  override def toString : String = "MagicIntArrayStacked(size:" + size + " level:" + currentLevel + " [" + cloneTopArray.mkString(",") + "])"
}
