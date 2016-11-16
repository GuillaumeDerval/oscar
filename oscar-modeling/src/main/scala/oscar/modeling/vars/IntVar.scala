package oscar.modeling.vars

import oscar.algo.vars.IntVarLike
import oscar.modeling.algebra.IntExpression
import oscar.modeling.misc.VariableNotBoundException
import oscar.modeling.models.ModelDeclaration
import oscar.modeling.vars.domainstorage.IntDomainStorage

import scala.collection.immutable.SortedSet
import scala.util.Random

/**
 * Represents a variable with Integer domain
 * @param model_decl: the ModelDeclaration associated with this Var
 */
class IntVar(model_decl: ModelDeclaration, id: Int, name: String) extends Var(model_decl, id, name) with IntVarLike with IntExpression {
  def getRepresentative: IntVarImplem = model_decl.getCurrentModel.getRepresentative(this).asInstanceOf[IntVarImplem]
  override def context = getRepresentative.context
  override def isBound: Boolean = getRepresentative.isBound
  override def randomValue(rand: Random): Int = getRepresentative.randomValue(rand)
  override def max: Int = getRepresentative.max
  override def valueBefore(value: Int): Int = getRepresentative.valueBefore(value)
  override def min: Int = getRepresentative.min
  override def valueAfter(value: Int): Int = getRepresentative.valueAfter(value)
  override def iterator: Iterator[Int] = getRepresentative.iterator
  override def hasValue(value: Int): Boolean = getRepresentative.hasValue(value)

  override def reify()(implicit modelDeclaration: ModelDeclaration): IntVar = this
  override def evaluate(): Int = if(isBound) max else throw new VariableNotBoundException()
  override def values(): Iterable[Int] = this

  override def subexpressions(): Iterable[IntExpression] = Array[IntExpression]()
  override def mapSubexpressions(func: (IntExpression) => IntExpression): IntExpression = this

  override def toString(): String = if(name.isEmpty) "IntVar" else name
}

object IntVar {
  def apply(minValue: Int, maxValue: Int)(implicit model_decl: ModelDeclaration) = {
    new IntVar(model_decl, model_decl.addNewRepresentative(IntDomainStorage(minValue, maxValue)), "")
  }

  def apply(content: Set[Int])(implicit model_decl: ModelDeclaration) = {
    new IntVar(model_decl, model_decl.addNewRepresentative(IntDomainStorage(content)), "")
  }

  def apply(content: SortedSet[Int])(implicit model_decl: ModelDeclaration) = {
    new IntVar(model_decl, model_decl.addNewRepresentative(IntDomainStorage(content)), "")
  }

  def apply(value: Int)(implicit model_decl: ModelDeclaration) = {
    new IntVar(model_decl, model_decl.addNewRepresentative(IntDomainStorage(value)), "")
  }

  def apply(minValue: Int, maxValue: Int, name: String)(implicit model_decl: ModelDeclaration) = {
    new IntVar(model_decl, model_decl.addNewRepresentative(IntDomainStorage(minValue, maxValue, name)), name)
  }

  def apply(content: Set[Int], name: String)(implicit model_decl: ModelDeclaration) = {
    new IntVar(model_decl, model_decl.addNewRepresentative(IntDomainStorage(content, name)), name)
  }

  def apply(content: SortedSet[Int], name: String)(implicit model_decl: ModelDeclaration) = {
    new IntVar(model_decl, model_decl.addNewRepresentative(IntDomainStorage(content, name)), name)
  }

  def apply(value: Int, name: String)(implicit model_decl: ModelDeclaration) = {
    new IntVar(model_decl, model_decl.addNewRepresentative(IntDomainStorage(value, name)), name)
  }
}