package oscar.cbls.business.routing.invariants.timeWindow

/**
  * This abstract class defines a TransferFunction.
  * The TransferFunction's main purpose is to compute
  * the leaving time at a node or segment's end given
  * the arrival time at the node or the segment's start.
  * It uses three values
  * @param ea the earliest arrival time at the node or segment's start
  * @param la the latest arrival time at the node or segment's start
  * @param el the earliest leaving time from node or segment's end
  */
abstract class TransferFunction(val ea: Int, val la: Int, val el: Int, val from: Int, val to: Int){

  // This method is used to compute the leaving time
  def apply(t: Int): Int

  // If true it means that the TransferFunction isn't defined
  // and that apply() return always None
  def isEmpty: Boolean

  override def toString: String = {
    "earliest arrival time : " + ea + "\n latest arrival time : " + la + "\n earliest leaving time : " + el
  }
}

case class DefinedTransferFunction(override val ea: Int, override val la: Int, override val el: Int,
                                   override val from: Int, override val to: Int) extends TransferFunction(ea,la,el,from,to){
  require(la >= ea && el >= ea, "earliest arrival time : " + ea + ", latest arrival time : " + la + ", earliest leaving time : " + el)
  override def apply(t: Int): Int = {
    if(t <= ea)
      el
    else if(t <= la)
      t + el - ea
    else
      -1
  }

  override def isEmpty: Boolean = false

  override def toString: String = {
    "Defined transfert function : \nFrom " + from + "   To " + to + "\n" + super.toString
  }
}

case object EmptyTransferFunction extends TransferFunction(1,-1,-1,-1,-1){
  override def apply(t: Int): Int = -1

  override def isEmpty: Boolean = true

  override def toString: String = "Empty transfert function"
}