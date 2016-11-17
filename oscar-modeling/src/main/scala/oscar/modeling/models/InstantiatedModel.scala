package oscar.modeling.models

import oscar.algo.search.Outcome
import oscar.modeling.constraints.Constraint
import oscar.modeling.misc.ModelVarStorage
import oscar.modeling.vars.{FloatVar, IntVar}
import oscar.modeling.vars.domainstorage.{FloatDomainStorage, IntDomainStorage}

trait LeafModel extends Model {
  /**
    * Post a new constraint
    * @param constraint constraint to add
    */
  def post(constraint: Constraint): Outcome

  /**
    * Post a new constraint
    * @param constraint constraint to add
    */
  def add(constraint: Constraint): Unit = {
    if(post(constraint) == Outcome.Failure)
      throw new NoSolException
  }

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
  override val floatRepresentatives: ModelVarStorage[FloatVar, FloatVarImplementation] = ModelVarStorage[FloatVar, FloatVarImplementation, FloatDomainStorage](p.floatRepresentatives, instantiateFloatDomainStorage)
  override val optimisationMethod: OptimisationMethod = p.optimisationMethod

  // Post the constraints
  p.constraints.foreach(add)

  // Post the max/minisation
  postObjective(p.optimisationMethod)

  private def instantiateIntDomainStorage(v: IntDomainStorage): IntVarImplementation = instantiateIntVar(v.content, v.name)
  private def instantiateFloatDomainStorage(v: FloatDomainStorage): FloatVarImplementation = instantiateFloatVar(v.min, v.max, v.name)

  protected def instantiateIntVar(content: Iterable[Int], name: String): IntVarImplementation
  protected def instantiateFloatVar(min: Double, max: Double, name: String): FloatVarImplementation

  protected def postObjective(optimisationMethod: OptimisationMethod): Unit
}