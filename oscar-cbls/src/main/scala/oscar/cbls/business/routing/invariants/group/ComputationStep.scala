package oscar.cbls.business.routing.invariants.group

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

/**
  * For the segment of the function, those class say if we can use pre-computes on this segment,
  * if the segment is a flipped segment or if we need from scratch method
  * @author Quentin Meurisse
  */
abstract sealed class ComputationStep(){
  def reverse() : ComputationStep
}

/**
  *
  * @param fromPosAtCheckpointZero
  * @param toPosAtCheckpointZero
  * @param flipPrecomputation
  * @author Quentin Meurisse
  * */
case class FetchFromPreCompute(fromPosAtCheckpointZero:Int,
                               toPosAtCheckpointZero: Int,
                               flipPrecomputation:Boolean)
  extends ComputationStep(){

  override def reverse() = FetchFromPreCompute(toPosAtCheckpointZero, fromPosAtCheckpointZero, !flipPrecomputation)
}

/**
  *
  * @param fromPos if topOfStack: the position in the new bijection
  *                else: the position in the concrete
  * @param toPos if topOfStack: the position in the new bijection
  *                else: the position in the concrete
  * @param topOfStack if the node is the inserted node for a InsertStackFunction
  * @author Quentin Meurisse
  * */
case class FromScratch(fromPos: Int,
                       toPos: Int,
                       topOfStack:Boolean = false)
  extends ComputationStep(){

  override def reverse(): ComputationStep = FromScratch(toPos, fromPos, topOfStack)
}