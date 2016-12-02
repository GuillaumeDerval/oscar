package oscar.modeling.models

import oscar.modeling.constraints.Constraint
import oscar.modeling.misc.ModelVarStorage
import oscar.modeling.vars.domainstorage.{FloatDomainStorage, IntDomainStorage}
import oscar.modeling.vars.{FloatVar, IntVar}

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
  * @param parent: the model from which to inherit
  */
abstract class InstantiatedModel(val parent: UninstantiatedModel) extends LeafModel {
  override val declaration: ModelDeclaration = parent.declaration
  override val intRepresentatives: ModelVarStorage[IntVar, IntVarImplementation] = ModelVarStorage[IntVar, IntVarImplementation, IntDomainStorage](parent.intRepresentatives, instantiateIntDomainStorage)
  override val floatRepresentatives: ModelVarStorage[FloatVar, FloatVarImplementation] = ModelVarStorage[FloatVar, FloatVarImplementation, FloatDomainStorage](parent.floatRepresentatives, instantiateFloatDomainStorage)
  override val optimisationMethod: OptimisationMethod = parent.optimisationMethod

  // Post the constraints
  parent.constraints.foreach(add)

  // Post the max/minisation
  postObjective(parent.optimisationMethod)

  private def instantiateIntDomainStorage(v: IntDomainStorage): IntVarImplementation = instantiateIntVar(v.content, v.name)
  private def instantiateFloatDomainStorage(v: FloatDomainStorage): FloatVarImplementation = instantiateFloatVar(v.min, v.max, v.name)

  protected def instantiateIntVar(content: Iterable[Int], name: String): IntVarImplementation
  protected def instantiateFloatVar(min: Double, max: Double, name: String): FloatVarImplementation

  protected def postObjective(optimisationMethod: OptimisationMethod): Unit
}