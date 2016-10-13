package oscar.modeling.models

import oscar.modeling.constraints.Constraint
import oscar.modeling.misc.ModelVarStorage
import oscar.modeling.vars.IntVar
import oscar.modeling.vars.domainstorage.int.IntDomainStorage


/**
  * Uninstanciated model
  * @param declaration Model declarator
  * @param constraints The constraints applied to this model
  * @param intRepresentatives the variables id and the domains associated
  * @param optimisationMethod the eventual optimization method
  */
case class UninstantiatedModel(declaration: ModelDeclaration,
                               constraints: List[Constraint],
                               intRepresentatives: ModelVarStorage[IntVar, IntDomainStorage],
                               optimisationMethod: OptimisationMethod) extends Model {
  override type IntVarImplementation = IntDomainStorage

  /**
    * Add a new variable with a new domain
    *
    * @param domain: an IntVarImplementation
    * @return  the id to the newly created variable and a new Model containing the variable
    */
  def withNewVariable(domain: IntDomainStorage): (Int, UninstantiatedModel) = {
    val r = intRepresentatives.add(domain)
    (r._1, copy(intRepresentatives = r._2))
  }

  /**
    * Post a new constraint
    *
    * @param constraint constraint to add
    * @return new model with the new constraint
    */
  def post(constraint: Constraint): UninstantiatedModel = copy(constraints = constraint :: constraints)

  /**
    * Post a new constraint
    *
    * @param constraint constraint to add
    * @return new model with the new constraint
    */
  def add(constraint: Constraint): UninstantiatedModel = post(constraint)

  /**
    * Post a new constraint
    *
    * @param constraint constraint to add
    * @return new model with the new constraint
    */
  def +(constraint: Constraint): UninstantiatedModel = post(constraint)

  /**
    * Minimize v
    *
    * @param v variable to minimize
    * @return copy of this model, but with minimisation of v
    */
  def minimize(v: IntVar): UninstantiatedModel = copy(optimisationMethod = new Minimisation(v))

  /**
    * Maximize v
    *
    * @param v variable to maximize
    * @return copy of this model, but with maximisation of v
    */
  def maximize(v: IntVar): UninstantiatedModel = copy(optimisationMethod = new Maximisation(v))

  /**
    * Maximize v
    *
    * @return copy of this model, but with optimisation disabled
    */
  def removeOptimisation(): UninstantiatedModel = copy(optimisationMethod = new NoOptimisation)
}