package oscar.cbls.test.invariants.bench

import org.scalacheck.{Gen, Prop}
import oscar.cbls.invariants.core.computation.{Variable, CBLSSetVar, CBLSIntVar, Store}
import scala.collection.immutable.{SortedMap, SortedSet}
import org.scalatest.prop.Checkers

/**
 * This class represents a move in the model, that is, one or several
 * modifications of the variables of the model.
 *
 * We distinguish between some identified "extremum" moves which can be used
 * as well on IntVar as on IntSetVar.
 */
abstract class Move

case class PlusOne() extends Move
case class MinusOne() extends Move
case class ToZero() extends Move
case class ToMin() extends Move
case class ToMax() extends Move
case class Random() extends Move
case class RandomDiff() extends Move

/**
 * This object contains a set of functions and methods to generate random
 * moves and variables, which we need for the tests.
 * @author yoann.guyot@cetic.be
 */
object InvGen {
  /**
   * Function to generate a random move.
   */
  val move = Gen.oneOf(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(),
    Random(), RandomDiff())

  def randomTuples(nbVal: Int, range: Range): List[(Int, Int)] = {
    val valList = Gen.containerOfN[List, Int](nbVal,
      Gen.choose(range.min, range.max).sample.get).sample.get
    valList.map((value: Int) => (
      value, Gen.choose(range.min, range.max).sample.get))
  }

  def randomIntSortedMap(nbVal: Int, valRange: Range, boundRange: Range): SortedMap[Int, Int] = {
    val valList = Gen.containerOfN[List, Int](nbVal,
      Gen.choose(valRange.min, valRange.max).sample.get).sample.get
    val map = valList.map((value: Int) => (
      value, Gen.choose(boundRange.min, boundRange.max).sample.get))
    SortedMap(map: _*)
  }

  /**
   * Method to generate a random IntVar:
   * - a random value which satisfies the given constraint is chosen in the
   * given range
   * - a random lower case character is chosen to be used
   *  as the name of the variable
   * The generated variable is added to the given model.
   */
  def randomIntVar(range: Range, model: Store, constraint: Int => Boolean) =
    for {
      v <- Gen.choose(range.min, range.max) suchThat (constraint(_))
      c <- Gen.alphaChar
    } yield new RandomIntVar(
      new CBLSIntVar(model, v, range, c.toString.toLowerCase), constraint)

  /**
   * Method to generate a list of nbVars random IntVar. Uses randomIntVar
   * method to generate each variable.
   */
  def randomIntVars(nbVars: Int, range: Range, model: Store, constraint: Int => Boolean) = {
    Gen.containerOfN[List, RandomIntVar](nbVars, randomIntVar(range, model, constraint))
  }

  /**
   * Method to generate a random IntSetVar of given size:
   * - a list of nbVars random values are chosen in the given range
   * - a random upper case character is chosen to be used
   *  as the name of the variable
   * A sorted set is made of the list of values, and the generated variable
   * is added to the given model.
   */
  def randomFixedIntSetVar(nbVars: Int, range: Range, model: Store) = for {
    c <- Gen.alphaChar
    v <- Gen.containerOfN[List, Int](nbVars, Gen.choose(range.min, range.max))
  } yield new RandomIntSetVar(
      new CBLSSetVar(model, SortedSet(v: _*), range, c.toString.toUpperCase))

  /**
   * Method to generate a random IntSetVar of size less or equal to the given
   * limit. Same as randomFixedIntSetVar, except the size is chosen randomly.
   */
  def randomIntSetVar(upToSize: Int, range: Range, model: Store) = for {
    c <- Gen.alphaChar
    s <- Gen.choose(1, upToSize)
    v <- Gen.containerOfN[List, Int](s, Gen.choose(range.min, range.max))
  } yield new RandomIntSetVar(new CBLSSetVar(model, SortedSet(v: _*), range,
      c.toString.toUpperCase))

  /**
   * Method to generate a list of IntSetVars. Uses randomIntSetVar.
   */
  def randomIntSetVars(nbVars: Int, upToSize: Int, range: Range, model: Store) = {
    Gen.containerOfN[List, RandomIntSetVar](nbVars,
      randomIntSetVar(upToSize, range, model))
  }
}

/**
 * A RandomVar contains a variable which can be modified using a Move.
 */
abstract class RandomVar {
  def randomVar(): Variable

  def move(move: Move)

  override def toString = randomVar.toString
}

/**
 * A RandomIntVar is a RandomVar containing an IntVar.
 * It can also contains a constraint which is applied when the variable is
 * moving.
 */
case class RandomIntVar(intVar: CBLSIntVar,
                        constraint: Int => Boolean = (v: Int) => true) extends RandomVar {

  override def randomVar(): CBLSIntVar = intVar

  def applyConstraint(newVal: Int) {
    if (constraint(newVal)) {
      randomVar := newVal
    }
  }

  /**
   * Defines the different possible moves for a RandomIntVar. Most are quite
   * obvious: PlusOne applies +1 to the IntVar value, ToZero sets the value
   * to zero, ToMax sets the value to the max value of the variable range.
   * Random sets the value to a randomly chosen one (in the variable range).
   * RandomDiff sets the value to a randomly chosen one, but different from
   * the previous one.
   */
  override def move(move: Move) = {
    move match {
      case PlusOne() => {
        val newVal = randomVar.value + 1
        if (randomVar.domain.contains(newVal)) applyConstraint(newVal)
        else applyConstraint(randomVar.min)
      }
      case MinusOne() => {
        val newVal = randomVar.value - 1
        if (randomVar.domain.contains(newVal)) applyConstraint(newVal)
        else applyConstraint(randomVar.max)
      }
      case ToZero() => {
        if (randomVar.domain.contains(0)) applyConstraint(0)
      }
      case ToMax() => {
        applyConstraint(randomVar.max)
      }
      case ToMin() => {
        applyConstraint(randomVar.min)
      }
      case Random() => {
        applyConstraint(Gen.choose(randomVar.min, randomVar.max).sample.get)
      }
      case RandomDiff() => {
        val randomOpt = (Gen.choose(randomVar.min, randomVar.max)
          suchThat (_ != randomVar.value)).sample
        if (randomOpt.isDefined) applyConstraint(randomOpt.get)
      }
    }
  }
}

/**
 * A RandomIntSetVar is a RandomVar containing an IntSetVar.
 */
case class RandomIntSetVar(intSetVar: CBLSSetVar) extends RandomVar {
  override def randomVar(): CBLSSetVar = intSetVar

  /**
   * Defines the different possible moves for a RandomIntSetVar.
   * PlusOne adds a new random value to the set whereas MinusOne removes one,
   * ToZero makes the set an empty one, ToMax adds all the values of the
   * variable range to the set whereas ToMin makes the set a singleton (of
   * which value is randomly chosen).
   * Random replaces the set with a random one (values and size are random)
   * but to avoid explosions, new size cannot be more than current size + 1.
   * RandomDiff replaces it with a random one with which intersection is empty,
   * if such a change is not possible, nothing's done.
   */
  override def move(move: Move) = {
    move match {
      case PlusOne() => { // Adds an element to the set
        randomVar :+= Gen.choose(randomVar.min, randomVar.max).sample.get
      }
      case MinusOne() => { // Removes an element from the set
        if (!randomVar.value.isEmpty) randomVar :-= Gen.oneOf(randomVar.value.toSeq).sample.get
        //else randomVar.value = Seq.fill(randomVar.value.size)(util.Random.nextInt)
      }
      case ToZero() => { // Removes all elements from the set
        randomVar.value.foreach(value => randomVar.deleteValue(value))
      }
      case ToMax() => { // Adds all elements between min and max to the set
        (randomVar.min to randomVar.max).foreach(v => randomVar :+= v)
      }
      case ToMin() => { // Reduces the set to a singleton
        randomVar.value.foreach(value => randomVar.deleteValue(value))
        randomVar :+= Gen.choose(randomVar.min, randomVar.max).sample.get
      }
      case Random() => { // Replaces the set with a randomly generated one
      val newSize = Gen.choose(1, randomVar.value.size + 1).sample.get
        val newVal = Gen.containerOfN[List, Int](newSize,
          Gen.choose(randomVar.min, randomVar.max)).sample.get
        randomVar := SortedSet(newVal: _*)
      }
      case RandomDiff() => {
        // Replaces the set with a randomly generated one
        // with which intersection is empty
        val newSize = Gen.choose(1, randomVar.value.size + 1).sample.get
        val newValOpt = Gen.containerOfN[List, Int](newSize,
          Gen.choose(randomVar.min, randomVar.max)
            suchThat (!randomVar.value.contains(_))).sample
        if (newValOpt.isDefined) randomVar := SortedSet(newValOpt.get: _*)
      }
    }
  }
}

/**
 * This class is intended to be used as a test bench for an invariant.
 * It contains a property which is : "Given a model, for any move applied to
 * its variables, its invariants hold.". In practice, we create a model with
 * only one invariant, generate most possible extreme moves of its
 * variables, and check this invariant at each move.
 *
 * When the invariant is created, we distinguish between input variables on
 * which moves can be applied, and output variables which will be updated by
 * the invariant only.
 *
 * Its argument 'verbose' is for debug messages printing :
 * 0 (or less) for no debug
 * 1 for a minimum debug
 * 2 (or more) for total debug
 * @author yoann.guyot@cetic.be
 */
class InvBench(verbose: Int = 0) {
  var property: Prop = false
  val checker = new InvariantChecker(verbose)
  val model = new Store(false, Some(checker), false, false, false)



  var inputVars: List[RandomVar] = List()
  var outputVars: List[RandomVar] = List()

  /**
   * These methods add variables to the bench.
   * input is true if the variable is an input variable, and false if it is an
   * output variable.
   */
  def addVar(input: Boolean, v: RandomVar) {
    addVar(input, List(v))
  }

  def addVar(input: Boolean, vars: Iterable[RandomVar]) {
    for (v <- vars) {
      if (input) inputVars = v :: inputVars
      else outputVars = v :: outputVars
    }
  }

  /**
   * Method for generating a new random IntVar to add to the bench and to its
   * model.
   */
  def genIntVar(
                 range: Range,
                 isInput: Boolean = true,
                 constraint: Int => Boolean = (v: Int) => true): CBLSIntVar = {
    val riVar = InvGen.randomIntVar(range, model, constraint).sample.get
    addVar(isInput, riVar)
    riVar.randomVar
  }

  /**
   * Method for generating an array of random IntVar to add to the bench and to its
   * model.
   */
  def genIntVars(
                  nbVars: Int = 4,
                  range: Range = 0 to 100,
                  isInput: Boolean = true,
                  constraint: Int => Boolean = (v: Int) => true): List[CBLSIntVar] = {
    val riVars = InvGen.randomIntVars(nbVars, range, model, constraint).sample.get
    addVar(isInput, riVars)
    riVars.map((riv: RandomIntVar) => {
      riv.randomVar
    })
  }

  def genIntVarsArray(
                       nbVars: Int = 4,
                       range: Range = 0 to 100,
                       isInput: Boolean = true,
                       constraint: Int => Boolean = (v: Int) => true): Array[CBLSIntVar] = {
    genIntVars(nbVars, range, isInput, constraint).toArray
  }

  /**
   * Method for generating a sorted set of random IntVar to add to the bench
   * and to its model.
   */
  def genSortedIntVars(
                        nbVars: Int,
                        range: Range,
                        isInput: Boolean = true,
                        constraint: Int => Boolean = (v: Int) => true): SortedSet[CBLSIntVar] = {
    val riVars = InvGen.randomIntVars(nbVars, range, model, constraint).sample.get
    addVar(isInput, riVars)
    val iVars = riVars.map((riv: RandomIntVar) => { riv.randomVar })
    SortedSet(iVars: _*)
  }

  def genBoundedValues(
                        nbVars: Int,
                        rangeValue: Range,
                        rangeBound: Range,
                        isInput: Boolean = true,
                        constraint: Int => Boolean = (v: Int) => true): SortedMap[Int, CBLSIntVar] = {
    val boundVars = genIntVars(nbVars, rangeBound, isInput, constraint)
    val map = boundVars.map((boundVar: CBLSIntVar) =>
      (Gen.choose(rangeValue.min, rangeValue.max).sample.get, boundVar))
    SortedMap(map: _*)
  }

  /**
   * Method for generating a random IntSetVar to add to the bench and to its
   * model.
   */
  def genIntSetVar(
                    nbVars: Int = 5,
                    range: Range = 0 to 100,
                    isInput: Boolean = true) = {
    val risVar = InvGen.randomFixedIntSetVar(nbVars, range, model).sample.get
    addVar(isInput, risVar)
    risVar.randomVar
  }

  /**
   * Method for generating an array of random IntSetVar to add to the bench
   * and to its model.
   */
  def genIntSetVars(
                     nbVars: Int = 4,
                     upToSize: Int = 20,
                     range: Range = 0 to 100,
                     isInput: Boolean = true): Array[CBLSSetVar] = {
    val risVars = InvGen.randomIntSetVars(nbVars, upToSize, range, model).sample.get
    addVar(isInput, risVars)
    risVars.map((risv: RandomIntSetVar) => {
      risv.randomVar
    }).toArray
  }

  /**
   * For debug only
   */
  def printVars(name: String, vars: List[RandomVar]) {
    if (vars.length > 0) {
      println(name + " vars:")
      vars.foreach((rv: RandomVar) => println(rv.toString()))
      println
    }
  }

  /**
   * This method runs the bench.
   */
  def run() = {
    model.close()
    //println("Model closed")
    model.propagate()

    try {
      property = org.scalacheck.Prop.forAll(InvGen.move) {
        randomMove: Move =>
          if (verbose > 0) {
            println("---------------------------------------------------")
            printVars("Input", inputVars)
            printVars("Output", outputVars)
            print(randomMove.toString() + " ")
          }
          val randomVar = Gen.oneOf(inputVars).sample.get
          if (verbose > 0) print(randomVar.toString() + " => ")
          randomVar.move(randomMove)
          if (verbose > 0) println(randomVar.toString() + "\n")
          model.propagate()
          if (verbose > 0) println
          checker.isChecked
      }
    } catch {
      case e: Exception =>
        println("Exception caught: " + e)
    }
    Checkers.check(property)
  }
}
