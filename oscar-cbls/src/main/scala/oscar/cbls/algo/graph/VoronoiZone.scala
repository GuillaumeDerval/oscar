package oscar.cbls.algo.graph


abstract sealed class ClosestCentroidLabeling{
  def <(that:ClosestCentroidLabeling):Boolean
  def equals(that:ClosestCentroidLabeling):Boolean
  def min(that:ClosestCentroidLabeling):ClosestCentroidLabeling = {
    if(this < that) this else that
  }
}

case class VoronoiZone(centroid:Node,distance:Int) extends ClosestCentroidLabeling{
  override def <(that: ClosestCentroidLabeling): Boolean = that match{
    case Unreachable => true
    case that:VoronoiZone =>
      this.distance < that.distance || (that.distance == this.distance && this.centroid.nodeId < that.centroid.nodeId)
  }

  override def equals(that: ClosestCentroidLabeling): Boolean = that match{
    case Unreachable => false
    case that:VoronoiZone => that.distance == this.distance && this.centroid == that.centroid
  }

  def + (length:Int):VoronoiZone = VoronoiZone(centroid,distance+length)

  override def toString: String = "VoronoiZone(centroid:" + centroid.nodeId + " distance:" + distance + ")"
}

case object Unreachable extends ClosestCentroidLabeling{
  override def <(that: ClosestCentroidLabeling): Boolean = false

  override def equals(that: ClosestCentroidLabeling): Boolean = that match{
    case Unreachable => true
    case that:VoronoiZone => false
  }
}