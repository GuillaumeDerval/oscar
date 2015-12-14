package oscar.algo.array

/**
 *  An array-based double ended queue for objects.
 *  This means that primitive types are boxed.
 *
 *  @author Renaud Hartert ren.hartert@gmail.com
 */
final class ArrayQueueInt(initialSize: Int = 8) {

  // The length of this array must be a power of 2
  private[this] var queue = new Array[Int](computeSize(initialSize))

  // Used for fast position testing
  private[this] var bitMask: Int = queue.length - 1

  private[this] var head: Int = 0
  private[this] var tail: Int = 0

  /**
   *  Return the size of the queue
   *
   *  @return The size of the queue
   */
  @inline final def size: Int = (tail - head) & bitMask

  /**
   *  Test if the queue is empty or not
   *
   *  @return `true` if the queue is empty, `false` otherwise
   */
  @inline final def isEmpty = head == tail

  /**
   *  Return the first element of the queue without removing it
   *
   *  Throws an exception if the queue is empty
   *
   *  @return The last element of the queue
   */
  @inline final def first: Int = {
    val elem = queue(head)
    if (head == tail) sys.error("Queue empty")
    else elem
  }

  /**
   *  Return the last element of the queue without removing it
   *
   *  Throws an exception if the queue is empty
   *
   *  @return The last element of the queue
   */
  @inline final def last: Int = {
    val elem = queue(tail)
    if (head == tail) sys.error("Queue empty")
    else elem
  }

  @inline final def clear(): Unit = {
    head = 0
    tail = 0
  }

  @inline final def addFirst(elem: Int): Unit = {
    head = (head - 1) & bitMask
    queue(head) = elem
    if (head == tail) growQueue() // Increase the size of the queue
  }

  @inline final def addLast(elem: Int): Unit = {
    queue(tail) = elem
    tail = (tail + 1) & bitMask
    if (head == tail) growQueue() // Increase the size of the queue
  }

  @inline final def removeFirst(): Int = {
    if (head == tail) sys.error("Queue empty")
    else {
      val elem = queue(head)
      head = (head + 1) & bitMask
      elem
    }
  }

  @inline final def removeLast(): Int = {
    if (head == tail) sys.error("Queue empty")
    else {
      tail = (tail - 1) & bitMask
      queue(tail)
    }
  }

  final def foreach[U](f: Int => U): Unit = {
    var i = head
    while (i != tail) {
      f(queue(i))
      i = (i + 1) & bitMask
    }
  }

  final def mkString(start: String, sep: String, end: String): String = {
    val builder = new StringBuilder()
    var first = true
    var i = head
    builder.append(start)
    while (i != tail) {
      if (first) {
        builder.append(queue(i))
        first = false
      } else {
        builder.append(sep)
        builder.append(queue(i))
      }
      i = (i + 1) & (queue.length - 1)
    }
    builder.append(end)
    builder.toString
  }

  final def mkString(sep: String): String = mkString("", sep, "")

  // Double the size of the queue
  @inline private def growQueue(): Unit = {
    // This function does not work if this condition does not hold
    assert(head == tail, "should not resize if head != tail")
    val size = queue.length
    val rest = size - head
    val newSize = size << 1
    if (newSize < 0) sys.error("too many elements")
    else {
      val newQueue = new Array[Int](newSize)
      System.arraycopy(queue, head, newQueue, 0, rest)
      System.arraycopy(queue, 0, newQueue, rest, head)
      queue = newQueue
      bitMask = newQueue.length - 1
      head = 0
      tail = size
    }
  }

  // Returns the lowest power of 2 that is superior to the initial size
  @inline private def computeSize(oldSize: Int): Int = {
    if (oldSize <= 8) 8
    else {
      var size = oldSize
      size |= (size >>> 1)
      size |= (size >>> 2)
      size |= (size >>> 4)
      size |= (size >>> 8)
      size |= (size >>> 16)
      size += 1
      if (size < 0) size >>> 1 // 2^30 elements
      else size
    }
  }
}
