package oscar.cbls.business.routing.invariants.group

/**
  * For the segment of the function, those class say if we can use pre-computes on this segment,
  * if the segment is a flipped segment or if we need from scratch method
  * @param fromPosAtCheckpointZero
  * @param toPosAtCheckpointZero
  * @author Quentin Meurisse
  */
abstract sealed class ComputationStep(val fromPosAtCheckpointZero: Int,
                                      val toPosAtCheckpointZero: Int){
  def reverse() : ComputationStep
}

case class FetchFromPreCompute(override val fromPosAtCheckpointZero:Int,
                               override val toPosAtCheckpointZero: Int,
                               flipPrecomputation:Boolean)
  extends ComputationStep(fromPosAtCheckpointZero, toPosAtCheckpointZero){

  override def reverse() = FetchFromPreCompute(toPosAtCheckpointZero, fromPosAtCheckpointZero, !flipPrecomputation)
}

/**
  *
  * @param fromPosAtCheckpointZero
  * @param toPosAtCheckpointZero
  * @param topOfStack if the node is the inserted node for a InsertStackFunction
  * @author Quentin Meurisse
  * */
case class FromScratch(override val fromPosAtCheckpointZero: Int,
                       override val toPosAtCheckpointZero: Int,
                       topOfStack:Boolean = false)
  extends ComputationStep(fromPosAtCheckpointZero, toPosAtCheckpointZero){

  override def reverse(): ComputationStep = FromScratch(toPosAtCheckpointZero, fromPosAtCheckpointZero, topOfStack)
}