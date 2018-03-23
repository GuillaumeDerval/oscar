package oscar.cbls.algo.magicArray
import oscar.cbls.algo.rb.RedBlackTreeMap

import scala.reflect.ClassTag
import scala.util.Random

object ImmutableArray{
  def createFromBaseArrayNeverModified[T:ClassTag](baseValueNeverModified:Array[T]):ImmutableArray[T] = {
    new ImmutableArray[T](baseValueNeverModified,
      baseValueNeverModified.length,
      RedBlackTreeMap.empty[T])
  }
  def createAndImportBaseValues[T:ClassTag](baseValues:Iterable[T]):ImmutableArray[T] = {
    val size = baseValues.size
    val it = baseValues.iterator
    createFromBaseArrayNeverModified(Array.tabulate[T](size)(id => it.next()))
  }
}

class ImmutableArray[T:ClassTag](baseValueNeverModified:Array[T],
                        override val size:Int,
                        updates:RedBlackTreeMap[T]) extends Iterable[T]{
  def apply(id: Int): T =
    if(id >= size) throw new ArrayIndexOutOfBoundsException
    else updates.getOrElse(id,baseValueNeverModified(id))

  def update(id: Int, value: T, fast: Boolean): ImmutableArray[T] = {
    val tmp = if(id == size) new ImmutableArray[T](baseValueNeverModified,size+1,updates.insert(id,value))
    else if (id < size) new ImmutableArray[T](baseValueNeverModified,size,updates.insert(id,value))
    else throw new ArrayIndexOutOfBoundsException
    if(fast) tmp else tmp.flatten()
  }

  def flatten():ImmutableArray[T] = new ImmutableArray(Array.tabulate[T](size)(id => this.apply(id)), size, RedBlackTreeMap.empty[T])

  override def iterator: Iterator[T] = new ImmutableArrayIterator[T](this)
}

class ImmutableArrayIterator[T](on:ImmutableArray[T])extends Iterator[T]{
  var nextPos = 0

  override def hasNext: Boolean = nextPos < on.size

  override def next(): T = {
    val toReturn = on(nextPos)
    nextPos+=1
    toReturn
  }
}

object TestImmutableArray extends App{

  val n = 100
  val referenceArray = Array.tabulate(n)(id => Random.nextInt(id+1))
  var immutableArray = ImmutableArray.createAndImportBaseValues(referenceArray)

  for(i <- 1 to 1000){
    val modifiedId = Random.nextInt(n)
    val newValue = Random.nextInt(n * (modifiedId+1))

    referenceArray(modifiedId) = newValue
    immutableArray = immutableArray.update(modifiedId,newValue,Random.nextBoolean())

    for(id <- 0 until n){
      require(referenceArray(id) == immutableArray(id))
    }
  }

}