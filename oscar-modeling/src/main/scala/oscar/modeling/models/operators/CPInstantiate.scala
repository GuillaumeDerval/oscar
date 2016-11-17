package oscar.modeling.models.operators

import oscar.modeling.models.UninstantiatedModel
import oscar.modeling.models.cp.CPModel

/**
 * Instantiate a model for a solve using CP
 */
object CPInstantiate extends ModelOperator[CPModel] {
  def apply(model: UninstantiatedModel): CPModel = {
    new CPModel(model)
  }
}