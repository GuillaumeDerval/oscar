package models

import misc.ModelVarStorage
import vars.IntVar
import vars.domainstorage.int.IntDomainStorage

/**
  * Constructor for the base model of each model declarator
  */
object BaseModel {
  def apply(declaration: ModelDeclaration) = UninstantiatedModel(declaration, Nil, ModelVarStorage[IntVar, IntDomainStorage](), NoOptimisation())
}
