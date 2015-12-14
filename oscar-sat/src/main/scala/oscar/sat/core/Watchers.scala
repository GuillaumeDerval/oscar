package oscar.sat.core

import oscar.sat.constraints.Constraint

final class Watchers(initialSize: Int = 16) {

  // The length of this array must be a power of 2
  private[this] var queue: Array[Constraint] = new Array(computeSize(initialSize))

  // Used for fast modulo
  private[this] var bitMask: Int = queue.length - 1

  private[this] var head: Int = 0
  private[this] var tail: Int = 0

  @inline final def length: Int = (tail - head) & bitMask

  @inline final def isEmpty = head == tail

  final def first: Constraint = {
    if (head == tail) sys.error("Queue empty")
    else queue(head)
  }

  final def last: Constraint = {
    if (head == tail) sys.error("Queue empty")
    else queue(tail)
  }

  @inline final def clear(): Unit = {
    head = 0
    tail = 0
  }
  
  @inline final def clearDeleted(): Unit = {
    var i = head
    var j = head
    while (i != tail) {
      val constraint = queue(i)
      if (!constraint.isDeleted) {
        queue(j) = constraint
        j = (j + 1) & bitMask
      }
      i = (i + 1) & bitMask
    }
    tail = j
  }

  @inline final def enqueue(constraint: Constraint): Unit = {
    queue(tail) = constraint
    tail = (tail + 1) & bitMask
    if (head == tail) growQueue() // Increase the size of the queue
  }

  @inline final def dequeue(): Constraint = {
    if (head == tail) sys.error("Queue empty")
    else {
      val elem = queue(head)
      head = (head + 1) & bitMask
      elem
    }
  }

  final def foreach[U](f: Constraint => U): Unit = {
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
      val newQueue = new Array[Constraint](newSize)
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
    if (oldSize <= 16) 16
    else {
      var size = initialSize
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