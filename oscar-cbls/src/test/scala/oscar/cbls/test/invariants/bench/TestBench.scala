package oscar.cbls.test.invariants.bench

/*******************************************************************************
  * OscaR is free software: you can redistribute it and/or modify
  * it under the terms of the GNU Lesser General Public License as published by
  * the Free Software Foundation, either version 2.1 of the License, or
  * (at your option) any later version.
  *
  * OscaR is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU Lesser General Public License  for more details.
  *
  * You should have received a copy of the GNU Lesser General Public License along with OscaR.
  * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
  ******************************************************************************/

import org.scalacheck.{Gen, Prop}
import oscar.cbls.algo.seq.functional.{ConcreteIntSequence, IntSequence}
import oscar.cbls.core.computation._
import scala.collection.immutable.{SortedMap, SortedSet}
import org.scalatest.prop.Checkers

/**
 * This class represents a move in the model, that is, one or several
 * modifications of the variables of the model.
 *
 * We distinguish between some identified "extremum" moves which can be used
 * as well on IntVar as on IntSetVar.
 */
abstract sealed class Move

case class PlusOne() extends Move
case class MinusOne() extends Move
case class ToZero() extends Move
case class ToMin() extends Move
case class ToMax() extends Move
case class Random() extends Move
case class RandomDiff() extends Move
case class Shuffle() extends Move
case class MultipleMove() extends Move

/**
 * This object contains a set of functions and methods to generate random
 * moves and variables, which we need for the tests.
  *
  * @author yoann.guyot@cetic.be
 */
object InvGen {
  /**
   * Function to generate a random move.
   */
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

  /**
    * Method to generate a random IntSeqVar of given size:
    * - a list of nbVars random values are chosen in the given range
    * - a random upper case character is chosen to be used
    *  as the name of the variable
    * A IntSequence is made of the list of values, and the generated variable
    * is added to the given model.
    */
  def randomFixedIntSeqVar(nbVars: Int, range: Range, model: Store) = for {
    c <- Gen.alphaChar
    v <- Gen.containerOfN[List, Int](nbVars, Gen.choose(range.min, range.max))
  } yield new RandomIntSeqVar(
    new CBLSSeqVar(model, IntSequence(v), range.max, c.toString.toUpperCase))

  /**
    * Method to generate a random IntSeqVar of size less or equal to the given
    * limit. Same as randomFixedIntSeqVar, except the size is chosen randomly.
    */
  def randomIntSeqVar(upToSize: Int, range: Range, model: Store) = for {
    c <- Gen.alphaChar
    s <- Gen.choose(1, upToSize)
    v <- Gen.containerOfN[List, Int](s, Gen.choose(range.min, range.max))
  } yield new RandomIntSeqVar(
    new CBLSSeqVar(model, IntSequence(v), range.max, c.toString.toUpperCase))

  /**
    * Method to generate a list of IntSeqVars. Uses randomIntSeqVar.
    */
  def randomIntSeqVars(nbVars: Int, upToSize: Int, range: Range, model: Store) = {
    Gen.containerOfN[List, RandomIntSeqVar](nbVars,
      randomIntSeqVar(upToSize, range, model))
  }

  /**
    * Method to generate a fixed IntSeqVar of size equal to the given limit.
    */
  def notRandomIntSeqVar(upToSize: Int, model: Store) = for {
    c <- Gen.alphaChar
    s <- Gen.choose(1, upToSize)
  } yield new NotRandomIntSeqVar(
    new CBLSSeqVar(model, IntSequence(List.tabulate(s)(n => n)), s, c.toString.toUpperCase))

  /**
    * Method to generate a list of IntSeqVars. Uses fixedIntSeqVar.
    */
  def notRandomIntSeqVars(nbVars: Int, upToSize: Int, model: Store) = {
    Gen.containerOfN[List, NotRandomIntSeqVar](nbVars,
      notRandomIntSeqVar(upToSize, model))
  }

  /**
    * Method to generate an empty routeOfNodes used to test routing invariants.
    */
  def routeOfNodes(upToSize: Int, v: Int, model: Store) = for {
    c <- Gen.alphaChar
  } yield new RouteOfNodes(
    new CBLSSeqVar(model, IntSequence(0 until v), upToSize, c.toString.toUpperCase),v)

  /**
    * Method to generate an empty routeOfNodes used to test checkPoints.
    */
  def routeOfNodesForCheckPoint(upToSize: Int, v: Int, model: Store, checker:InvariantChecker) = for {
    c <- Gen.alphaChar
  } yield new RouteOfNodesForCheckPoint(
    new CBLSSeqVar(model, IntSequence(0 until v), upToSize, c.toString.toUpperCase),v,checker)
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
      case Random() | Shuffle() | MultipleMove() => {
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
      case Random() | Shuffle() | MultipleMove() => { // Replaces the set with a randomly generated one
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
  * A RandomIntSeqVar is a RandomVar containing an IntSeqVar.
  */
case class RandomIntSeqVar(intSeqVar: CBLSSeqVar) extends RandomVar{
  override def randomVar(): CBLSSeqVar = intSeqVar

  /**
    * Defines the different possible moves for a RandomIntSeqVar.
    * PlusOne adds a new random value to the seq whereas MinusOne removes one,
    * ToZero makes the seq an empty one, ToMax adds all the values of the
    * variable range to the seq whereas ToMin makes the seq a singleton (of
    * which value is randomly chosen).
    * Random replaces the seq with a random one (values and size are random)
    * but to avoid explosions, new size cannot be more than current size + 1.
    * RandomDiff replaces it with a random one with which intersection is empty,
    * if such a change is not possible, nothing's done.
    * Shuffle shuffles the positions of each value contained in the seq
    */
  override def move(move: Move)= {
    println(move.toString)
    move match{
      case PlusOne() =>
        randomVar().insertAtPosition(Gen.choose(randomVar().min,randomVar().max).sample.get,Gen.choose(randomVar().min,randomVar().newValue.size).sample.get)
      case MinusOne() =>
        if(!randomVar().newValue.isEmpty) randomVar().remove(Gen.choose(randomVar().min,randomVar().newValue.size-1).sample.get)
      case ToZero() =>
        for(i <- randomVar().min until randomVar().newValue.size)
          randomVar().remove(0)
      case ToMax() =>
        (randomVar().min to randomVar().max).foreach(v => if(randomVar().newValue.size < randomVar().max) randomVar().insertAtPosition(v,Gen.choose(randomVar().min,randomVar().newValue.size).sample.get))
      case ToMin() =>
        for(i <- randomVar().min until randomVar().newValue.size)
          randomVar().remove(0)
        randomVar().insertAtPosition(Gen.choose(randomVar().min,randomVar().max).sample.get,0)
      case Random() | Shuffle() =>
        val newSize = Gen.choose(1, Math.min(randomVar().newValue.size + 1,randomVar().max)).sample.get
        val newVal = Gen.containerOfN[Iterable, Int](newSize,
          Gen.choose(randomVar().min, randomVar().max)).sample.get
        randomVar() := IntSequence(newVal)
      case RandomDiff() =>
        val newSize = Gen.choose(1, Math.min(randomVar().newValue.size + 1,randomVar().max)).sample.get
        val newValOpt = Gen.containerOfN[Iterable, Int](newSize,
          Gen.choose(randomVar().min, randomVar().max)
            suchThat (!randomVar().newValue.contains(_))).sample
        if (newValOpt.isDefined) randomVar := IntSequence(newValOpt.get)
      case Shuffle() | MultipleMove() =>
        for(p <- 0 until randomVar().newValue.size) {
          val newPos = Gen.choose(0, randomVar().newValue.size - 1).sample.get
          if (newPos-1 != p)
            randomVar().move(p, p, newPos-1, false)
        }
    }
  }
}

/**
  * A NotRandomIntSeqVar is a NotRandomVar containing an IntSeqVar.
  */
case class NotRandomIntSeqVar(intSeqVar: CBLSSeqVar) extends RandomVar{
  override def randomVar(): CBLSSeqVar = intSeqVar

  /**
    * Defines the different possible moves for a RandomIntSeqVar.
    * PlusOne adds a new random value to the seq whereas MinusOne removes one,
    * ToZero makes the seq an empty one, ToMax adds all the values of the
    * variable range to the seq whereas ToMin makes the seq a singleton (of
    * which value is randomly chosen).
    * Random replaces the seq with a random one (values and size are random)
    * but to avoid explosions, new size cannot be more than current size + 1.
    * RandomDiff replaces it with a random one with which intersection is empty,
    * if such a change is not possible, nothing's done.
    * Shuffle shuffles the positions of each value contained in the seq
    */
  override def move(move: Move)= {
    val inSeq = randomVar().newValue.unorderedContentNoDuplicate
    val notInSeq = List.tabulate(randomVar().max)(i => i).filterNot(inSeq.contains(_))
    move match{
      case PlusOne() =>
        if(!notInSeq.isEmpty)
          randomVar().insertAtPosition(Gen.oneOf(notInSeq).sample.get,Gen.choose(0,inSeq.size).sample.get)
      case MinusOne() =>
        if(!randomVar().newValue.isEmpty) randomVar().remove(Gen.choose(0,inSeq.size-1).sample.get)
      case ToZero() =>
        for(i <- randomVar().min until randomVar().newValue.size)
          randomVar().remove(0)
      case ToMax() =>
        notInSeq.foreach(v => if(randomVar().newValue.size < randomVar().max) randomVar().insertAtPosition(v,Gen.choose(randomVar().min,randomVar().newValue.size).sample.get))
      case ToMin() =>
        for(i <- randomVar().min until randomVar().newValue.size)
          randomVar().remove(0)
        randomVar().insertAtPosition(Gen.choose(randomVar().min,randomVar().max).sample.get,0)
      case Random() | MultipleMove() =>
        val newSize = Gen.choose(1, Math.min(randomVar().newValue.size + 1,randomVar().max)).sample.get
        val fullList = scala.util.Random.shuffle(List.tabulate(randomVar().max)(n => n))
        val newVal = List.tabulate(newSize)(n => fullList(n)).toIterable
        randomVar() := IntSequence(newVal)
      case RandomDiff() =>
        val newSize = Gen.choose(0, Math.max(randomVar().min,randomVar().max - randomVar().newValue.size)).sample.get
        val fullList = scala.util.Random.shuffle(notInSeq)
        val newVal = List.tabulate(newSize)(n => fullList(n)).toIterable

        if (newVal != null) randomVar := IntSequence(newVal)
      case Shuffle() =>
        for(p <- 0 until randomVar().newValue.size) {
          val newPos = Gen.choose(0, randomVar().newValue.size - 1).sample.get
          if (newPos != p)
            randomVar().move(p, p, newPos, false)
        }

    }
  }
}

/**
  * A RouteOfNodes is a Var containing an IntSeqVar build for routing problems.
  */
case class RouteOfNodes(intSeqVar: CBLSSeqVar, v:Int) extends RandomVar{
  override def randomVar(): CBLSSeqVar = intSeqVar

  /**
    * Defines the different possible moves for a RandomIntSeqVar.
    * PlusOne adds a new random value to the seq whereas MinusOne removes one,
    * ToZero makes the seq an empty one, ToMax adds all the values of the
    * variable range to the seq whereas ToMin makes the seq a singleton (of
    * which value is randomly chosen).
    * Random replaces the seq with a random one (values and size are random)
    * but to avoid explosions, new size cannot be more than current size + 1.
    * RandomDiff replaces it with a random one with which intersection is empty,
    * if such a change is not possible, nothing's done.
    * Shuffle shuffles the positions of each value contained in the seq
    */
  override def move(move: Move)= {
    val inSeq = randomVar().newValue.dropWhile(_ != 0).toList
    val notInSeq = List.tabulate(randomVar().max)(n => n).filterNot(inSeq.contains(_))
    move match{
      case PlusOne() =>
        if(notInSeq.nonEmpty)
          randomVar().insertAtPosition(Gen.oneOf(notInSeq).sample.get,Gen.choose(1,inSeq.size).sample.get)
      case MinusOne() =>
        if(inSeq.size > v) randomVar().remove(inSeq.indexOf(Gen.oneOf(inSeq.filterNot(_ < v)).sample.get))
      case ToZero() =>
        if(inSeq.size > v){
          var currentPos = 1
          while(randomVar().newValue.size > v) {
            if(randomVar().newValue.valueAtPosition(currentPos).get < v)
              currentPos += 1
            else
              randomVar().remove(currentPos)
          }
        }
      case ToMax() =>
        var valuesInserted = 0
        notInSeq.foreach(value =>{
          randomVar().insertAtPosition(value,Gen.choose(1,randomVar().newValue.size).sample.get)
          valuesInserted += 1
        })
      case ToMin() =>
        if(inSeq.size > v){
          var currentPos = 1
          while(randomVar().newValue.size > v) {
            if(randomVar().newValue.valueAtPosition(currentPos).get < v)
              currentPos += 1
            else
              randomVar().remove(currentPos)
          }
        }
        randomVar().insertAtPosition(Gen.choose(v,randomVar().max-1).sample.get,Gen.choose(1,v).sample.get)
      case Random() =>
        val nbOfValueToAdd = Gen.choose(0, Math.min(randomVar().newValue.size + 1,randomVar().max)-v).sample.get
        val fullList = scala.util.Random.shuffle(List.tabulate(randomVar().max-v)(n => n+v))
        val newVal = List.tabulate(v)(n => n)
        randomVar() := IntSequence(newVal)
        for(i <- 0 until nbOfValueToAdd)
          randomVar().insertAtPosition(fullList(i),Gen.choose(1,randomVar().newValue.size-1).sample.get)
      case RandomDiff() =>
        val nbOfValueToAdd = Gen.choose(0, Math.max(randomVar().min,notInSeq.size)).sample.get
        val fullList = scala.util.Random.shuffle(notInSeq)
        val newVal = List.tabulate(v)(n => n)
        randomVar() := IntSequence(newVal)
        for(i <- 0 until nbOfValueToAdd)
          randomVar().insertAtPosition(fullList(i),Gen.choose(1,randomVar().newValue.size-1).sample.get)
      case Shuffle() | MultipleMove() =>
        for(p <- 0 until randomVar().value.size){
          if(randomVar().newValue.valueAtPosition(p).get>=v) {
            val newPos = Gen.choose(1, randomVar().newValue.size - 1).sample.get
            if (newPos != p)
              randomVar().move(p, p, newPos, false)
          }
        }
    }
  }
}

/**
  * A RouteOfNodesForCheckPoint is a Var containing an IntSeqVar build for routing problems.
  * The aim of this case class is to test the checkPoint  mechanism.
  */
case class RouteOfNodesForCheckPoint(intSeqVar: CBLSSeqVar, v:Int, checker:InvariantChecker) extends RandomVar{
  override def randomVar(): CBLSSeqVar = intSeqVar

  /**
    * Defines the different possible moves for a RandomIntSeqVar.
    * PlusOne adds a new random value to the seq whereas MinusOne removes one,
    * ToZero makes the seq an empty one, ToMax adds all the values of the
    * variable range to the seq whereas ToMin makes the seq a singleton (of
    * which value is randomly chosen).
    * Random replaces the seq with a random one (values and size are random)
    * but to avoid explosions, new size cannot be more than current size + 1.
    * RandomDiff replaces it with a random one with which intersection is empty,
    * if such a change is not possible, nothing's done.
    * Shuffle shuffles the positions of each value contained in the seq
    */
  override def move(move: Move)= {
    var inSeq = randomVar().newValue.toList
    var notInSeq = List.tabulate(randomVar().max)(n => n).filterNot(inSeq.contains(_))
    move match{
      case PlusOne() =>
        val checkPoint = randomVar().defineCurrentValueAsCheckpoint(true)
        for(nbOfMove <- notInSeq.indices) {
          for (i <- 0 until nbOfMove+1) {
            inSeq = randomVar().newValue.toList
            notInSeq = List.tabulate(randomVar().max)(n => n).filterNot(inSeq.contains(_))
            randomVar().insertAtPosition(Gen.oneOf(notInSeq).sample.get, Gen.choose(1, inSeq.size).sample.get)
          }
          randomVar().rollbackToTopCheckpoint(checkPoint)
          checker.check(randomVar().value == checkPoint, Some("Error : " + randomVar().value.toString + ", or : " + randomVar().newValue.toString + " != " + checkPoint.toString))
        }
        inSeq = randomVar().newValue.toList
        notInSeq = List.tabulate(randomVar().max)(n => n).filterNot(inSeq.contains(_))
        if(notInSeq.nonEmpty)
          randomVar().insertAtPosition(Gen.oneOf(notInSeq).sample.get, Gen.choose(1, inSeq.size).sample.get)
      case MinusOne() =>
        val checkPoint = randomVar().defineCurrentValueAsCheckpoint(true)
        for(nbOfMove <- inSeq.filterNot(_ < v).indices) {
          for (i <- 0 until nbOfMove+1) {
            inSeq = randomVar().newValue.toList
            randomVar().remove(inSeq.indexOf(Gen.oneOf(inSeq.filterNot(_ < v)).sample.get))
          }
          randomVar().rollbackToTopCheckpoint(checkPoint)
          checker.check(randomVar().value == checkPoint, Some("Error : " + randomVar().value.toString + ", or : " + randomVar().newValue.toString + " != " + checkPoint.toString))
        }
        checker.check(randomVar().value == checkPoint, Some("Error : " + randomVar().value.toString + ", or : " + randomVar().newValue.toString + " != " + checkPoint.toString))
        inSeq = randomVar().newValue.toList
        if(inSeq.size > v)
          randomVar().remove(inSeq.indexOf(Gen.oneOf(inSeq.filterNot(_ < v)).sample.get))
      case Shuffle() =>
        val checkPoint = randomVar().defineCurrentValueAsCheckpoint(true)
        for(nbOfMove <- inSeq.filterNot(_ < v).indices) {
          for (i <- 0 until nbOfMove+1){
            inSeq = randomVar().newValue.toList
            val p1 = inSeq.indexOf(Gen.oneOf(inSeq.filterNot(_ < v)).sample.get)
            val p2 = inSeq.indexOf(Gen.oneOf(inSeq.drop(p1).takeWhile(_ >= v)).sample.get)
            val newPos = Gen.choose(1, inSeq.size - 1).sample.get
            if(newPos < p1 || newPos > p2)
              randomVar().move(p1, p2, newPos, false)
          }
          randomVar().rollbackToTopCheckpoint(checkPoint)
          checker.check(randomVar().value == checkPoint, Some("Error : " + randomVar().value.toString + ", or : " + randomVar().newValue.toString + " != " + checkPoint.toString))
        }
        for(i <- 0 until inSeq.size - v){
          inSeq = randomVar().newValue.toList
          val p1 = inSeq.indexOf(Gen.oneOf(inSeq.filterNot(_ < v)).sample.get)
          val p2 = inSeq.indexOf(Gen.oneOf(inSeq.drop(p1).takeWhile(_ >= v)).sample.get)
          val newPos = Gen.choose(1, randomVar().newValue.size - 1).sample.get
          if(newPos < p1 || newPos > p2)
            randomVar().move(p1, p2, newPos, false)
        }

      case MultipleMove() =>
        val checkPoint = randomVar().defineCurrentValueAsCheckpoint(true)
        val moves = List("add", "remove", "shuffle")
        for(nbOfMove <- 1 to 50) {
          for (i <- 0 until nbOfMove) {
            inSeq = randomVar().newValue.toList
            notInSeq = List.tabulate(randomVar().max)(n => n).filterNot(inSeq.contains(_))
            Gen.oneOf(moves).sample.get match{
              case "add" =>
                if(notInSeq.nonEmpty)
                  randomVar().insertAtPosition(Gen.oneOf(notInSeq).sample.get, Gen.choose(1, inSeq.size).sample.get)
              case "remove" =>
                if(inSeq.size > v)
                  randomVar().remove(inSeq.indexOf(Gen.oneOf(inSeq.filterNot(_ < v)).sample.get))
              case "shuffle" =>
                if(inSeq.size > v) {
                  val p1 = inSeq.indexOf(Gen.oneOf(inSeq.filterNot(_ < v)).sample.get)
                  val p2 = inSeq.indexOf(Gen.oneOf(inSeq.drop(p1).takeWhile(_ >= v)).sample.get)
                  val newPos = Gen.choose(1, randomVar().newValue.size - 1).sample.get
                  if(newPos < p1 || newPos > p2)
                    randomVar().move(p1, p2, newPos, false)
                }
            }
          }
          randomVar().rollbackToTopCheckpoint(checkPoint)
          checker.check(randomVar().value == checkPoint, Some("Error : " + randomVar().value.toString + ", or : " + randomVar().newValue.toString + " != " + checkPoint.toString))
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
  *
  * @author yoann.guyot@cetic.be
 */
class InvBench(verbose: Int = 0, moves:List[Move]) {
  var property: Prop = false
  val checker = new InvariantChecker(verbose)
  val model = new Store(false, Some(checker), true, false, false)

  val move = Gen.oneOf(moves)

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
    * Method for generating a random IntSeqVar to add to the bench
    * and to its model.
    */
  def genIntSeqVar(
                  maxLength: Int = 5,
                  range: Range = 0 to 100,
                  isInput:Boolean = true) = {
    val risVar = InvGen.randomIntSeqVar(maxLength, range, model).sample.get
    addVar(isInput, risVar)
    risVar.randomVar
  }

  /**
    * Method for generating an array of random IntSeqVar to add to the bench
    * and to its model.
    */
  def genIntSeqVars(
                   nbVars: Int = 4,
                   upToSize: Int = 20,
                   range: Range = 0 to 100,
                   isInput: Boolean = true): Array[CBLSSeqVar] = {
    val risVars = InvGen.randomIntSeqVars(nbVars, upToSize, range, model).sample.get
    addVar(isInput, risVars)
    risVars.map((risv: RandomIntSeqVar) => {
      risv.randomVar
    }).toArray
  }

  def genNotRandomIntSeqVar(
                            upToSize: Int = 20,
                            isInput: Boolean = true) = {
    val risVar = InvGen.notRandomIntSeqVar(upToSize,model).sample.get
    addVar(isInput,risVar)
    risVar.randomVar
  }

  def genRouteOfNodes(
                       upToSize: Int = 20,
                       v: Int = 5,
                       isInput: Boolean = true) = {
    val risVar = InvGen.routeOfNodes(upToSize,v,model).sample.get
    addVar(isInput,risVar)
    risVar.randomVar
  }

  def genRouteOfNodesForCheckPoint(
                       upToSize: Int = 20,
                       v: Int = 5,
                       isInput: Boolean = true) = {
    val risVar = InvGen.routeOfNodesForCheckPoint(upToSize,v,model,checker).sample.get
    addVar(isInput,risVar)
    risVar.randomVar
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
      property = org.scalacheck.Prop.forAll(move) {
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
