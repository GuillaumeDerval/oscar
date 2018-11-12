package oscar.cbls.business.geometric

object OverlapDetection {

  def isOverlapNaive(shapes:List[Shape]): Boolean = {

    val shapesA = shapes.toArray
    for(i <- shapes.indices){
      for(j <- 0 until i){
        if (Shape.isOverlap(shapesA(i), shapesA(j))) {
          println(new OverlapError(shapesA(i), shapesA(j)))
          return true
        }
      }
    }
    false
  }

  def isOverlapByLineSweep(shapes:List[Shape]): Boolean = {

    var shapesSortedByMinX = shapes.sortBy(_.minX)
    var openShapesSortedByClosingX: List[Shape] = List.empty

    while (shapesSortedByMinX.nonEmpty) {
      val nextOpeningShape = shapesSortedByMinX.head
      val nextClosingShapeOpt: Option[Shape] = openShapesSortedByClosingX.headOption

      if (nextClosingShapeOpt.isEmpty || nextOpeningShape.minX <= nextClosingShapeOpt.get.maxX) {
        //open a new shape, and insert at the right lace in the list (insertion sort)
        try {
          openShapesSortedByClosingX = checkOverlapAndInsertNewShapeIntoList(openShapesSortedByClosingX,nextOpeningShape)
        } catch {
          case o: OverlapError =>
            println(o)
            return true
        }
        shapesSortedByMinX = shapesSortedByMinX.tail
      } else {
        //close shape
        openShapesSortedByClosingX = openShapesSortedByClosingX.tail
      }
    }
    false // no overlap detected
  }

  class OverlapError(shape1:Shape,shape2:Shape) extends Exception{
    override def toString: String = "OverlapError(" + shape1 + "," + shape2 + ")"
  }

  def checkOverlapAndInsertNewShapeIntoList(openShapes: List[Shape],nextOpeningShape:Shape): List[Shape] = {
    openShapes match {
      case openShape :: tail =>
        if (Shape.isOverlap(openShape, nextOpeningShape))
          throw new OverlapError(openShape,nextOpeningShape)

        checkOverlapAndInsertNewShapeIntoList(tail,nextOpeningShape) match {
          case h :: t =>
            if (h.maxX < openShape.maxX) h :: openShape :: t
            else openShape :: h :: t
          case Nil => List(openShape)
        }
      case Nil => List(nextOpeningShape)
    }
  }
}
