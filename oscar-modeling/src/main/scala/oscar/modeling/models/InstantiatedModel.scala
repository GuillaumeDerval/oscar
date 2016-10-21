package oscar.modeling.models

import oscar.modeling.constraints.Constraint
import oscar.modeling.misc.ModelVarStorage
import oscar.modeling.vars.IntVar
import oscar.modeling.vars.domainstorage.IntDomainStorage

trait LeafModel extends Model {
  /**
    * Post a new constraint
    * @param constraint constraint to add
    */
  def post(constraint: Constraint): Unit

  /**
    * Post a new constraint
    * @param constraint constraint to add
    */
  def add(constraint: Constraint): Unit = post(constraint)

  /**
    * Post a new constraint
    * @param constraint constraint to add
    */
  def += (constraint: Constraint): Unit = post(constraint)
}

/**
  * Abstract class for all Instantiated Models
  * @param p: the model from which to inherit
  */
abstract class InstantiatedModel(p: UninstantiatedModel) extends LeafModel {
  override val declaration: ModelDeclaration = p.declaration
  override val intRepresentatives: ModelVarStorage[IntVar, IntVarImplementation] = ModelVarStorage[IntVar, IntVarImplementation, IntDomainStorage](p.intRepresentatives, instantiateIntDomainStorage)
  override val optimisationMethod: OptimisationMethod = p.optimisationMethod

  // Post the constraints
  p.constraints.foreach(post)

  private def instantiateIntDomainStorage(v: IntDomainStorage): IntVarImplementation = instantiateIntVar(v.content, v.name)

  protected def instantiateIntVar(content: Iterable[Int], name: String): IntVarImplementation
}