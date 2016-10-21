package oscar.modeling.vars.cp
import oscar.modeling.vars.BoolVarImplem

class CPBoolVar(realCPVar: oscar.cp.CPBoolVar) extends CPIntVar(realCPVar) with BoolVarImplem {
  /** @return true if the variable is bound and bound to value 1 */
  override def isTrue: Boolean = realCPVar.isTrue

  /** @return true if the variable is bound and bound to value 0 */
  override def isFalse: Boolean = realCPVar.isFalse

  /** Returns `true` if the domain contains 1. */
  override def containsTrue: Boolean = realCPVar.containsTrue

  /** Returns `true` if the domain contains 0. */
  override def containsFalse: Boolean = realCPVar.containsFalse
}

object CPBoolVar {
  def apply(content: Iterable[Int], name: String, store: oscar.cp.CPStore): CPBoolVar = {
    val cpvar = if(content.size == 1) oscar.cp.CPBoolVar(content.head != 0, name)(store)
    else oscar.cp.CPBoolVar(name)(store)
    new CPBoolVar(cpvar)
  }
}