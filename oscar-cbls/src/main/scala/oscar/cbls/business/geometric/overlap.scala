package oscar.cbls.business.geometric

object LineSweepOverlapDetection {

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
          case _: OverlapError =>
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

  class OverlapError extends Exception

  def checkOverlapAndInsertNewShapeIntoList(openShapes: List[Shape],nextOpeningShape:Shape): List[Shape] = {
    openShapes match {
      case openShape :: tail =>
        if (Shape.isOverlap(openShape, nextOpeningShape)) throw new OverlapError

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
