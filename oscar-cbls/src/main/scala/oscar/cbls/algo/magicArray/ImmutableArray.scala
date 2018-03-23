package oscar.cbls.algo.magicArray

import oscar.cbls.algo.rb.RedBlackTreeMap

object ImmutableArray{
  def createFromBaseArrayNeverModified[T](baseValueNeverModified:Array[T]):ImmutableArray[T] = {
    new ImmutableArray[T](baseValueNeverModified,
      baseValueNeverModified.length,
    RedBlackTreeMap.empty[T])
  }
  def createAndImportBaseValues[T](baseValues:Iterable[T]):ImmutableArray[T] = {
    val size = baseValues.size
    val it = baseValues.iterator
    this(Array.tabulate[T](size)(id => it.next()))
  }
}

class ImmutableArray[T](baseValueNeverModified:Array[T],
                        val size:Int,
                        updates:RedBlackTreeMap[T]){
  def apply(id: Int): T =
    if(id >= size) throw new ArrayIndexOutOfBoundsException
    else updates.getOrElse(id,baseValueNeverModified(id))

  def update(id: Int, value: T, fast: Boolean): ImmutableArray[T] = {
    if(id == size) new ImmutableArray[T](baseValueNeverModified,size+1,updates.insert(id,value))
    else if (id < size) new ImmutableArray[T](baseValueNeverModified,size,updates.insert(id,value))
    else throw new ArrayIndexOutOfBoundsException
  }

  def flatten():ImmutableArray[T] = new ImmutableArray(Array.tabulate[T](size)(id => this.apply(id)), size, RedBlackTreeMap.empty[T])
}

