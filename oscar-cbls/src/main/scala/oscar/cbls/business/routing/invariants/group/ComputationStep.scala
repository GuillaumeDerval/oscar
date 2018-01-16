package oscar.cbls.business.routing.invariants.group

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