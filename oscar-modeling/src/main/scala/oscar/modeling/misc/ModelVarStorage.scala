package oscar.modeling.misc

import oscar.modeling.vars.Var

import scala.collection.mutable

object ModelVarStorage {
  /**
    * Creates a new ModelVarStorage
    * @tparam VarType Var type that will store its domain in this object
    * @tparam StoredObject type of domain
    */
  def apply[VarType <: Var, StoredObject]() = new ModelVarStorage[VarType, StoredObject](IndexedSeq[StoredObject]())

  /**
    * Init this ModelVarStorage with a copy of the content of another one, with a type conversion
    * @param to_copy: ModelVarStorage to copy
    * @param convert: Convert function, gives instance of type StoredObject for old objects
    */
  def apply[VarType <: Var, StoredObject, OldType](to_copy: ModelVarStorage[VarType, OldType], convert: OldType => StoredObject) = {
    val map = mutable.Map[OldType, StoredObject]()
    new ModelVarStorage[VarType, StoredObject](to_copy.array.map(item => map.getOrElseUpdate(item, convert(item))))
  }
}

/**
  * Stores variable domains (and allow to replace/union domains)
  * Immutable
  */
class ModelVarStorage[VarType <: Var, StoredObject](private val array: IndexedSeq[StoredObject]) extends Serializable {
  /**
    * Get the domain of `v`
    * @param v the variable of which we want to find the domain
    */
  def get(v: VarType): StoredObject = array(v.varid)

  /**
    * Get the domain of the variable with id `v`
    * @param v the variable of which we want to find the domain
    */
  def get(v: Int): StoredObject = array(v)

  /**
    * Add a new element
    * @param elem domain to be added
    * @return the id of the element, to be used by the var as internal value
    */
  def add(elem: StoredObject): (Int, ModelVarStorage[VarType, StoredObject]) = {
    val newid = array.length
    (newid, new ModelVarStorage[VarType, StoredObject](array :+ elem))
  }

  /**
    * Create a new ModelVarStorage where all the objects that are keys in toReplace are replaced by their value in the map
    * @param toReplace object to be replaced -> object that will replace it
    */
  def replace(toReplace: Map[StoredObject, StoredObject]): ModelVarStorage[VarType, StoredObject] = {
    new ModelVarStorage(array.map(item => toReplace.getOrElse(item, item)))
  }

  /**
    * Create a new ModelVarStorage where all the objects in toReplace are replaced by `by`
    * @param toReplace set of objects to replace
    * @param by object by which the items will be replaced
    */
  def replace(toReplace: Set[StoredObject], by: StoredObject): ModelVarStorage[VarType, StoredObject] = {
    replace(toReplace.map(item => (item, by)).toMap)
  }

  /**
    * Make a "union" of vars domain replace1 and replace2 by making the vars link to newVal
    * @param replace1 first domain to replace
    * @param replace2 second domain to replace
    * @param newVal new domain
    */
  def union(replace1: StoredObject, replace2: StoredObject, newVal: StoredObject): ModelVarStorage[VarType, StoredObject] = {
    replace(Set(replace1, replace2), newVal)
  }

  /**
    * Create a new ModelVarStorage where all the `toReplace` are replaced by `by`
    * @param toReplace object to be replaced
    * @param by object to use instead
    */
  def replace(toReplace: StoredObject, by: StoredObject): ModelVarStorage[VarType, StoredObject] = {
    replace(Map((toReplace, by)))
  }

  /**
    * Create a new ModelVarStorage where all the objects that are keys in toReplace are replaced
    * @param toReplace object to be replaced -> object that will replace it
    */
  def replaceVars(toReplace: Map[VarType, StoredObject]): ModelVarStorage[VarType, StoredObject] = {
    val newMap = mutable.Map[StoredObject, StoredObject]()
    for((v, o) <- toReplace) {
      val curVal = get(v)
      newMap.get(curVal) match {
        case Some(`o`) => //ok, do nothing, expected behaviour
        case Some(other) => throw new Exception("Two variables have currently the same domain, but toReplace contains two different new domains")
        case None => newMap += ((curVal, o))
      }
    }
    replace(newMap.toMap)
  }

  /**
    * Create a new ModelVarStorage where all the objects in toReplace are replaced by `by`
    * @param toReplace set of objects to replace
    * @param by object by which the items will be replaced
    */
  def replaceVars(toReplace: Set[VarType], by: StoredObject): ModelVarStorage[VarType, StoredObject] = {
    replaceVars(toReplace.map(item => (item, by)).toMap)
  }

  /**
    * Create a new ModelVarStorage where all the `toReplace` are replaced by `by`
    * @param toReplace object to be replaced
    * @param by object to use instead
    */
  def replaceVars(toReplace: VarType, by: StoredObject): ModelVarStorage[VarType, StoredObject] = {
    replace(Map((get(toReplace), by)))
  }

  /**
    * Make a "union" of vars domain replace1 and replace2 by making the vars link to newVal
    * @param replace1 first domain to replace
    * @param replace2 second domain to replace
    * @param newVal new domain
    */
  def union(replace1: VarType, replace2: VarType, newVal: StoredObject): ModelVarStorage[VarType, StoredObject] = {
    replaceVars(Set(replace1, replace2), newVal)
  }
}
