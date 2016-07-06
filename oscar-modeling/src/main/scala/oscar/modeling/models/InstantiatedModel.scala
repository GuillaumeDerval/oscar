package oscar.modeling.models

import oscar.modeling.constraints.Constraint
import oscar.modeling.misc.ModelVarStorage
import oscar.modeling.vars.IntVar
import oscar.modeling.vars.domainstorage.DomainStorage
import oscar.modeling.vars.domainstorage.int._

trait LeafModel extends Model {
  /**
    * Post a new constraint
 *
    * @param constraint constraint to add
    */
  def post(constraint: Constraint): Boolean

  /**
    * Post a new constraint
 *
    * @param constraint constraint to add
    */
  def add(constraint: Constraint): Boolean = post(constraint)

  /**
    * Post a new constraint
 *
    * @param constraint constraint to add
    */
  def += (constraint: Constraint): Boolean = post(constraint)
}

/**
  * Abstract class for all Instantiated Models
 *
  * @param p: the model from which to inherit
  */
abstract class InstantiatedModel(p: UninstantiatedModel) extends LeafModel {
  override val declaration: ModelDeclaration = p.declaration
  override val intRepresentatives: ModelVarStorage[IntVar, IntVarImplementation] = ModelVarStorage[IntVar, IntVarImplementation, IntDomainStorage](p.intRepresentatives, instantiateDomainStorage)
  override val optimisationMethod: OptimisationMethod = p.optimisationMethod

  for(c <- p.constraints)
    post(c)

  protected def instantiateDomainStorage(v: DomainStorage): IntVarImplementation = {
    v match {
      case adaptable: AdaptableIntDomainStorage => instantiateAdaptableIntDomainStorage(adaptable)
      case interval: IntervalDomainStorage => instantiateIntervalDomainStorage(interval)
      case set: SetDomainStorage => instantiateSetDomainStorage(set)
      case singleton: SingletonDomainStorage => instantiateSingletonDomainStorage(singleton)
      case _ => sys.error("Unknown DomainStorage type in InstantiatedModel.instantiateDomainStorage")
    }
  }

  protected def instantiateAdaptableIntDomainStorage(adaptable: AdaptableIntDomainStorage): IntVarImplementation

  protected def instantiateIntervalDomainStorage(interval: IntervalDomainStorage): IntVarImplementation

  protected def instantiateSetDomainStorage(set: SetDomainStorage): IntVarImplementation

  protected def instantiateSingletonDomainStorage(singleton: SingletonDomainStorage): IntVarImplementation
}