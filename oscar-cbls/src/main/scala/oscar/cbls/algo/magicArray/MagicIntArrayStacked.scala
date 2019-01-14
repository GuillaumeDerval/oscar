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


class MagicIntArrayStacked(maxLevel:Long, initVal:(Long => Long), size:Long) extends Iterable[Long]{

  private[this] val levelToArray:Array[Array[Long]] = Array.tabulate(maxLevel+1L)(level => if(level == 0L) Array.tabulate(size)(initVal) else Array.fill(size)(0L))
  private[this] val levelToIsValueChangedAtNextLevel:Array[IterableMagicBoolArray] = Array.tabulate(maxLevel)(level => new IterableMagicBoolArray(size,false))
  private[this] var currentLevel:Long = 0L

  def level:Long = currentLevel - 1L

  def update(indice:Long,value:Long){
    levelToArray(currentLevel)(indice) = value
    if(currentLevel!=0L)levelToIsValueChangedAtNextLevel(currentLevel-1L)(indice) = true
  }

  def apply(indice:Long):Long = {
    var attemptLevel = currentLevel
    while(attemptLevel>0L){
      val levelBelow = attemptLevel - 1L
      if(levelToIsValueChangedAtNextLevel(levelBelow)(indice)){
        return levelToArray(attemptLevel)(indice)
      }else{
        attemptLevel = levelBelow
      }
    }
    levelToArray(0L)(indice)
  }

  def pushLevel(){
    require(currentLevel < maxLevel,"MagicIntArrayStacked was declaring with max " + maxLevel + " levels; trying to push more")
    levelToIsValueChangedAtNextLevel(currentLevel).all = false
    currentLevel += 1L
  }

  def popLevel(dropChanges:Boolean){
    require(currentLevel > 0L,"trying to pop level zero")
    if(dropChanges){
      currentLevel -= 1L
    }else{
      //save changes to lower level!
      val newLevel = currentLevel - 1L
      for(changedID <- levelToIsValueChangedAtNextLevel(currentLevel-1L).indicesAtTrue){
        levelToArray(newLevel)(changedID) = levelToArray(currentLevel)(changedID)
        levelToIsValueChangedAtNextLevel(newLevel)(changedID) = false
      }
      currentLevel = newLevel
    }
  }

  def cloneTopArray:Array[Long] = {
    Array.tabulate(size)(this(_))
  }

  override def iterator : Iterator[Long] = {
    cloneTopArray.iterator
  }

  override def toString : String = "MagicIntArrayStacked(size:" + size + " level:" + currentLevel + " [" + cloneTopArray.mkString(",") + "])"
}
