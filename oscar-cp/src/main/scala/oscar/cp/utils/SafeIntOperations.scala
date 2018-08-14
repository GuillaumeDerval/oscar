package oscar.cp.utils

/**
  * Safe mathematical operations for Ints using upcasting to detect overflows.
  *
  * @author Charles Thomas cftmthomas@gmail.com
  */
object SafeIntOperations {
  def intRangeCheck(value: Long): Long = {
    if(value > Int.MaxValue || value < Int.MinValue) throw new ArithmeticException("Integer Overflow")
    value
  }

  def safeSum(left: Int, right: Int): Int = intRangeCheck(left.toLong + right.toLong).toInt

  def safeSum(values: Iterable[Int]): Int = values.foldLeft(0L)((acc, value) => intRangeCheck(acc + value.toLong)).toInt

  //TODO: other operations
}
