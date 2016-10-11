package models.operators

import models.{Model, UninstantiatedModel}

/**
 * Trait common to all ModelOperators
 */
trait ModelOperator[OutputType <: Model] {
  def apply(model: UninstantiatedModel): OutputType
}
