package oscar.modeling.misc

import scala.util.DynamicVariable

/**
 * A version of DynamicVariable for when we do not need to be dynamic
 * @param init: the initial value to be set
 * @tparam T: the type of the values to store
 */
class NotReallyDynamicVariable[T](init: T = null) extends DynamicVariable(init) {
  var content = init

  override def withValue[S](newval: T)(thunk: => S): S = {
    val oldval = value
    content = newval

    try thunk
    finally content = oldval
  }

  override def value: T = content

  override def value_=(newval: T) = {
    content = newval
  }
}