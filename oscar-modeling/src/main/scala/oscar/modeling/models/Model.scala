package oscar.modeling.models

import oscar.modeling.misc.ModelVarStorage
import oscar.modeling.vars._

/**
 * Basic trait for all models
 */
trait Model extends Serializable {
  type IntVarImplementation <: IntVarImplem

  val declaration: ModelDeclaration
  val intRepresentatives: ModelVarStorage[IntVar, IntVarImplementation]
  val optimisationMethod: OptimisationMethod

  /**
   * Get implementation of a Var
   * @param v variable to find
   * @return On an instantiated model, the model itself; on an uninstantiated one, a copy of it
   */
  def getRepresentative(v: IntVar): IntVarImplementation = intRepresentatives.get(v)

  /**
   * Apply a function on this model
   * @param func
   */
  def apply[R](func: => R): R = declaration.apply(this)(func)
}
