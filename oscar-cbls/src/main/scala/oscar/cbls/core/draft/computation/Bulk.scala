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
/*******************************************************************************
  * Contributors:
  *     This code has been initially developed by CETIC www.cetic.be
  *         by Renaud De Landtsheer
  *            Yoann Guyot
  ******************************************************************************/

package oscar.cbls.core.draft.computation

import scala.collection.immutable.SortedMap

/**Invariants over arrays can implement this trait to make it possible to bulk load their dependencies
  *
  * @author renaud.delandtsheer@cetic.be
  * */
trait Bulked[VarType <: ChangingValue, BulkedComputationResult] extends Invariant {

  /**
    * registers a static dependency to all variables mentioned in the bulkedVars.
    * @param bulkedVars: an iterable of variables to bulk together
    * @param id a reference name to identify to which bulk in the invariant this belongs to. Several bulks can be done with a single invariants
    * @return the result of performBulkComputation(bulkedVars),  possibly computed by co-bulking invariants
    */
  final def bulkRegister(bulkedVars: Array[VarType], id: Int = 0, store:Store): BulkedComputationResult = {

    //check for existing bulk
    val identifyingString = this.getClass.getName + "/" + id

    store.getBulk(identifyingString, bulkedVars.asInstanceOf[Array[ChangingValue]]) match{
      case Some(incredibleBulk) =>
        incredibleBulk.registerStaticallyListeningElement(this)
        incredibleBulk.bulkedComputationResult.asInstanceOf[BulkedComputationResult]

      case None =>
        //create a new bulk
        val bcr = performBulkComputation(bulkedVars,id)
        val newBulk = new Bulk(store, bulkedVars.asInstanceOf[Array[ChangingValue]], bcr)
        newBulk.registerStaticallyListeningElement(this)
        store.registerBulk(identifyingString, newBulk)
        bcr
    }
  }

  def performBulkComputation(vars: Array[VarType], id: Int): BulkedComputationResult = null.asInstanceOf[BulkedComputationResult]
}

/**
  * This is the node that is put in the propagation graph
  * used by BulkLoad only
  * @author renaud.delandtsheer@cetic.be
  */
class Bulk(store: Store, val bulkedVars: Array[ChangingValue], val bulkedComputationResult: Any)
  extends Invariant(store:Store){

  for (v <- bulkedVars) v.registerStaticallyListeningElement(this)

  override def checkInternals(): Unit = require(true)
}

/**This is the dictionaries where bulks are stored, and can be searched for
  * @author renaud.delandtsheer@cetic.be
  */
trait Bulker {

  private var createdBulks: SortedMap[String, List[Bulk]] = SortedMap.empty

  def getBulk(identifyingName: String, bulkedVars: Array[ChangingValue]): Option[Bulk] = {
    createdBulks.get(identifyingName) match {
      case None => None
      case Some(bulks) =>
        for (b <- bulks) {
          if (bulkedVars == b.bulkedVars) {  //we really want to compare references
            return Some(b)
          }
        }
        None
    }
  }

  def registerBulk(identifyingName: String, bulk: Bulk) {
    val knownbulk = createdBulks.getOrElse(identifyingName, List.empty)
    createdBulks += ((identifyingName, bulk :: knownbulk))
  }

  protected def killBulker(): Unit ={
    createdBulks = null
  }
}
