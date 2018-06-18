package oscar.cbls.business.routing.invariants.group

/**
  * For the segment of the function, those class say if we can use pre-computes on this segment,
  * if the segment is a flipped segment or if we need from scratch method
  * @param fromPosAtCheckpoint0
  * @param toPosAtCheckpoint0
  * @author Quentin Meurisse
  */
abstract sealed class ComputationStep(val fromPosAtCheckpoint0: Int,
                                      val toPosAtCheckpoint0: Int){
  def reverse() : ComputationStep
}

case class FetchFromPreCompute(override val fromPosAtCheckpoint0:Int,
                               override val toPosAtCheckpoint0: Int,
                               flipPrecomputation:Boolean)
  extends ComputationStep(fromPosAtCheckpoint0, toPosAtCheckpoint0){

  override def reverse() = FetchFromPreCompute(toPosAtCheckpoint0, fromPosAtCheckpoint0, !flipPrecomputation)
}

/**
  *
  * @param fromPosAtCheckpoint0
  * @param toPosAtCheckpoint0
  * @param topOfStack if the node is the inserted node for a InsertStackFunction
  * @author Quentin Meurisse
  * */
case class FromScratch(override val fromPosAtCheckpoint0: Int,
                       override val toPosAtCheckpoint0: Int,
                       topOfStack:Boolean = false)
  extends ComputationStep(fromPosAtCheckpoint0, toPosAtCheckpoint0){

  override def reverse(): ComputationStep = FromScratch(toPosAtCheckpoint0, fromPosAtCheckpoint0, topOfStack)
}