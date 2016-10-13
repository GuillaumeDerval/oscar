package oscar.modeling.models.operators

import oscar.modeling.models.{Model, UninstantiatedModel}

/**
 * Trait common to all ModelOperators
 */
trait ModelOperator[OutputType <: Model] {
  def apply(model: UninstantiatedModel): OutputType
}
