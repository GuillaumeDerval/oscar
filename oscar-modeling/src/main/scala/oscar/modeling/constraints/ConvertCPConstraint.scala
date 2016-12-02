package oscar.modeling.constraints

import oscar.cp
import oscar.cp.CPSolver
import oscar.cp.constraints.Automaton
import oscar.modeling.vars.cp.CPIntVar
import oscar.modeling.vars.{BoolVar, IntVar}

/**
  * Convert an oscar.cp constraint to an oscar.modeling constraint
  *
  * @author Guillaume Derval (guillaume.derval@uclouvain.be)
  */
object ConvertCPConstraint {

  /**
    * Convert an oscar.cp constraint to an oscar.modeling constraint
    * @param args the arguments to the constructor of the class
    */
  def apply[T <: oscar.cp.Constraint](args: Any*)(implicit m : Manifest[T]): CPInstantiableConstraint = {
    new CPInstantiableConstraint {
      override def cpPost(cpSolver: CPSolver): Unit = cpSolver.post(getConstraint[T](args: _*))
    }
  }

  /**
    * Convert args from oscar.modeling and returns a oscar.cp.Constraint
    * @param args the arguments to the constructor of the class
    */
  def getConstraint[T <: oscar.cp.Constraint](args: Any*)(implicit m : Manifest[T]): T = {
    val constructors = m.runtimeClass.getConstructors
    var i = 0
    while(i != constructors.length) {
      try {
        val constructor = constructors(i)
        val parameters = constructor.getParameterTypes
        val realArgs = args.map(convertArg).zip(parameters).map(x => boxIfNeeded(x._2, x._1))
        return constructor.newInstance(realArgs: _*).asInstanceOf[T]
      }
      catch {
        case _: Throwable => i+=1 //could not instantiate this constructor, let's try another one
      }
    }
    throw new RuntimeException("No valid constructor found")
  }

  /**
    * Convert various types of arguments containing IntVar to the same type containing cp.CPIntVar
    */
  private def convertArg(arg: Any): Any = arg match {
    case intvar: IntVar => intvar.getRepresentative.asInstanceOf[CPIntVar].realCPVar
    case boolvar: BoolVar => boolvar.getRepresentative.asInstanceOf[CPIntVar].realCPVar.asInstanceOf[cp.CPBoolVar]
    case int: Int => int
    case long: Long => long
    case double: Double => double
    case bool: Boolean => bool
    case char: Char => char
    case string: String => string
    case automaton: Automaton => automaton
    case set: Set[_] => set.map(convertArg)
    case seq: Seq[_] => seq.map(convertArg)
    case list: List[_] => list.map(convertArg)
    case array: Array[Int] => array
    case array: Array[Long] => array
    case array: Array[Double] => array
    case array: Array[Boolean] => array
    case array: Array[Char] => array
    case array: Array[String] => array
    case array: Array[IntVar] => array.map(convertArg(_).asInstanceOf[cp.CPIntVar])
    case array: Array[BoolVar] => array.map(convertArg(_).asInstanceOf[cp.CPBoolVar])
    case array: Array[Array[_]] =>
      val values = array.map(convertArg(_))
      val valueType = values(0).getClass
      val newArray = java.lang.reflect.Array.newInstance(valueType, array.length)
      java.lang.System.arraycopy(values, 0, newArray, 0, array.length)
      newArray
    case _ => throw new RuntimeException("Unknown parameter type "+arg.getClass.toString)
  }

  /**
    * Box primivite classes. Needed because http://bugs.java.com/bugdatabase/view_bug.do?bug_id=6456930
    */
  private def boxIfNeeded(c: Class[_], v: Any): Object = c match {
    case java.lang.Integer.TYPE => new java.lang.Integer(v.asInstanceOf[Int])
    case java.lang.Float.TYPE => new java.lang.Float(v.asInstanceOf[Double])
    case java.lang.Double.TYPE => new java.lang.Double(v.asInstanceOf[Double])
    case java.lang.Long.TYPE => new java.lang.Long(v.asInstanceOf[Long])
    case java.lang.Character.TYPE => new java.lang.Character(v.asInstanceOf[Char])
    case java.lang.Boolean.TYPE => new java.lang.Boolean(v.asInstanceOf[Boolean])
    case _ => c.cast(v).asInstanceOf[Object]
  }
}
