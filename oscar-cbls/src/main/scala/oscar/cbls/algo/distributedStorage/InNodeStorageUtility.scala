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
package oscar.cbls.algo.distributedStorage

import scala.collection.immutable.SortedMap

/**
 * integrate this trait to store something in your class using the standard storing mechanism
 * @author renaud.delandtsheer@cetic.be
 */
trait DistributedStorageUtility {

  var storage: SortedMap[Long, Any] = null

  /**returns null if nothing was stored*/
  final def getStorageAt[T](key: Long, default: T = null.asInstanceOf[T]) =
    if(storage == null) default
    else storage.getOrElse(key, default).asInstanceOf[T]

  final def storeAt(key: Long, value: Any) {
    if(storage == null) storage = SortedMap.empty
    storage = storage + ((key, value))
  }

  final def freeStorageAt(key: Long): Unit ={
    storage = storage - key
    if(storage.isEmpty) storage = null
  }

  final def getAndFreeStorageAt[T](key: Long, default: T = null.asInstanceOf[T]) =
    if(storage == null) default
    else{
      val toReturn = storage.getOrElse(key, default).asInstanceOf[T]
      storage = storage - key
      if(storage.isEmpty) storage = null
      toReturn
    }
}

/**
 * integrate this trait somewhere as he dictionary defining unique keys for the [[oscar.cbls.algo.distributedStorage.DistributedStorageUtility]]
 * @author renaud.delandtsheer@cetic.be
 */
trait StorageUtilityManager {
  var nextStoragePlace: Long = 0L

  /** creates a unique storage key that can be used
    * to store anything in all DistributedStorageUtility
    * bound to this StorageUtilityManager
    * @return
    */
  def newStorageKey(): Long = {
    val toreturn = nextStoragePlace
    nextStoragePlace += 1L
    toreturn
  }

  /**
   * This method stores for each item of the array, its index in the array.
   * We suppose that an item only appears once in the array
   * @param storagePlaces the array
   * @param storageKey the storageKey where the index will be saved
   * @param offsetIndex if not zero, the value stored is actually index+offsetIndex. DEfault value is zero
   * @tparam T the actual type of the storage places
   */
  def storeIndexesAt[T <: DistributedStorageUtility] (storagePlaces:Array[T], storageKey:Long, offsetIndex:Long = 0L){
    for(i <- storagePlaces.indices) {
      storagePlaces(i).storeAt(storageKey, i + offsetIndex)
    }
  }
}
