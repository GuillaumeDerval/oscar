package oscar.modeling.models

import oscar.modeling.misc.ModelVarStorage
import oscar.modeling.vars.{FloatVar, IntVar}
import oscar.modeling.vars.domainstorage.{FloatDomainStorage, IntDomainStorage}

/**
  * Constructor for the base model of each model declarator
  */
object BaseModel {
  def apply(declaration: ModelDeclaration) = {
    UninstantiatedModel(declaration, Nil, ModelVarStorage[IntVar, IntDomainStorage](), ModelVarStorage[FloatVar, FloatDomainStorage](), NoOptimisation())
  }
}
