package oscar.cbls.algo.accList


package oscar.cbls.algo.accList

object AccList{
  def base[T](l:T*):AccList[T] = new LeafAccList[T](l.toList)
  def acc[T](l:AccList[T]*):AccList[T] = new AccNode[T](l.toList)
  def empty[T]():AccList[T] = base[T]()
  def fromList[T](l:List[T]) = new LeafAccList[T](l.toList)
}

abstract class AccList[T]{
  def toList:List[T] = toListAcc(List.empty)
  def toListAcc(tail:List[T]):List[T]
}

class LeafAccList[T](content:List[T]) extends AccList[T]{
  override def toListAcc(tail: List[T]): List[T] = {
    content ::: tail
  }
}

class AccNode[T](content:List[AccList[T]]) extends AccList[T]{
  override def toListAcc(tail: List[T]): List[T] = {
    def myToListAcc(myList:List[AccList[T]]):List[T] = {
      myList match{
        case Nil => tail
        case myHead::myTail => myHead.toListAcc(myToListAcc(myTail))
      }
    }
    myToListAcc(content)
  }
}
