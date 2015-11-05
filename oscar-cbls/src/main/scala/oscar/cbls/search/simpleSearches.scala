package oscar.cbls.search

import oscar.cbls.constraints.core.ConstraintSystem
import oscar.cbls.invariants.core.computation.{CBLSIntVar, CBLSSetVar, IntValue}
import oscar.cbls.modeling.AlgebraTrait
import oscar.cbls.objective.Objective
import oscar.cbls.search.algo.{HotRestart, IdenticalAggregator}
import oscar.cbls.search.core._
import oscar.cbls.search.move.{AssignMove, CompositeMove, Move, SwapMove}

import scala.collection.immutable.SortedSet

/**
 * will find a variable in the array, and find a value from its range that improves the objective function
 *
 * @param vars an array of [[oscar.cbls.invariants.core.computation.CBLSIntVar]] defining the search space
 * @param name the name of the neighborhood
 * @param best true for the best move, false for the first move, default false
 * @param searchZone a subset of the indices of vars to consider.
 *                   If none is provided, all the array will be considered each time
 * @param symmetryClassOfVariables a function that input the ID of a variable and returns a symmetry class;
 *                      ony one of the variable in each class will be considered to make search faster
 *                      Int.MinValue is considered different to itself
 *                      if you set to None this will not be used at all
 *                      variables of the same class with different values will not be considered as symmetrical
 * @param symmetryClassOfValues a function that inputs the ID of a variable and a possible value for this variable,
 *                              and returns a symmetry class for this variable and value
 *                              only values belonging to different symmetry classes will be tested
 *                             Int.MinValue is considered different to itself
 *                             (this is only useful if your model is awfully expensive to evaluate)
 * @param domain a function that receives a variable and its Id in the vars array
 *               and returns the domain that is searched for the variable
 *               by default, the domain of the variable is explored
 * @param hotRestart  if true, the exploration order in case you ar not going for the best is a hotRestart
 *                    even if you specify a searchZone that is: the exploration starts again
 *                    at the position where it stopped, and consider the indices in increasing order
 *                    if false, consider the exploration range in natural order from the first position.
 */
case class AssignNeighborhood(vars:Array[CBLSIntVar],
                              name:String = "AssignNeighborhood",
                              best:Boolean = false,
                              searchZone:() => Iterable[Int] = null,
                              symmetryClassOfVariables:Option[Int => Int] = None,
                              symmetryClassOfValues:Option[Int => Int => Int] = None,
                              domain:(CBLSIntVar,Int) => Iterable[Int] = (v,i) => v.domain,
                              hotRestart:Boolean = true)
  extends EasyNeighborhood(best,name) with AlgebraTrait{
  //the indice to start with for the exploration
  var startIndice:Int = 0

  override def exploreNeighborhood() {

    val iterationSchemeOnZone =
      if (searchZone == null) {
        if (hotRestart && !best) {
          vars.indices startBy startIndice
        }else vars.indices
      }else if (hotRestart && !best) HotRestart(searchZone(), startIndice)
      else searchZone()

    val iterationScheme = symmetryClassOfVariables match {
      case None => iterationSchemeOnZone
      case Some(s) => IdenticalAggregator.removeIdenticalClassesLazily(iterationSchemeOnZone, (index:Int) => (s(index),vars(index).value))
    }

    for (i <- iterationScheme) {

      val currentVar = vars(i)
      val oldVal = currentVar.value
      val domainIterationScheme = symmetryClassOfValues match {
        case None => domain(currentVar, i)
        case Some(s) => IdenticalAggregator.removeIdenticalClassesLazily(domain(currentVar, i), s(i))
      }

      for (newVal <- domainIterationScheme if newVal != oldVal) {

        //AVANT
        val newObj = obj.assignVal(currentVar,newVal)

        if (moveRequested(newObj) && submitFoundMove(AssignMove(currentVar, newVal, newObj, name))){
          startIndice = i + 1
          return
        }
/*
        //APRES
        if (exploreMove(_.assignVal(currentVar,newVal),
          AssignMove(currentVar, newVal, _, name))){
          startIndice = i + 1
          return
        }
*/

      }
    }
  }

  //this resets the internal state of the Neighborhood
  override def reset(): Unit = {
    startIndice = 0
  }
}

/**
 * will iteratively swap the value of two different variables in the array
 *
 * @param vars an array of [[oscar.cbls.invariants.core.computation.CBLSIntVar]] defining the search space
 * @param searchZone1 a subset of the indices of vars to consider for the first moved point
 *                   If none is provided, all the array will be considered each time
 * @param searchZone2 a subset of the indices of vars to consider for the second moved point
 *                   If none is provided, all the array will be considered each time
 * @param symmetryCanBeBrokenOnIndices if set to true, the neighborhood will break symmetries on indices of swapped vars
 *                            that is: thee first variable will always have an indice strictly smaller than the second swapped variable
 *                            typically, you always want it except if you have specified one or two searchZones, and they are different
 * @param symmetryCanBeBrokenOnValue if set to true, the neighborhood will break symmetries on values of swapped vars
 *                            that is: thee first variable will always have a value strictly smaller than the value of second swapped variable
 *                            you do not want to have both symmetryCanBeBrokenOnIndices and symmetryCanBeBrokenOnValue
 * @param name the name of the neighborhood
 * @param symmetryClassOfVariables1 a function that input the ID of a variable and returns a symmetry class;
 *                      for each role of the move, ony one of the variable in each class will be considered for the vars in searchZone1
 *                      this makes search faster
 *                      Int.MinValue is considered different to itself
 *                      if you set to None this will not be used at all
 * @param symmetryClassOfVariables2 a function that input the ID of a variable and returns a symmetry class;
 *                      for each role of the move, ony one of the variable in each class will be considered for the vars in searchZone2
 *                      this makes search faster
 *                      Int.MinValue is considered different to itself
 *                      if you set to None this will not be used at all
 * @param hotRestart  if true, the exploration order in case you ar not going for the best
 *                    is a hotRestart for the first swapped variable
 *                    even if you specify a searchZone that is: the exploration starts again
 *                    at the position where it stopped, and consider the indices in increasing order
 *                    if false, consider the exploration range in natural order from the first position.
 **/
case class SwapsNeighborhood(vars:Array[CBLSIntVar],
                             name:String = "SwapsNeighborhood",
                             searchZone1:()=>Iterable[Int] = null,
                             searchZone2:()=>Iterable[Int] = null,
                             symmetryCanBeBrokenOnIndices:Boolean = true,
                             symmetryCanBeBrokenOnValue:Boolean = false,
                             best:Boolean = false,
                             symmetryClassOfVariables1:Option[Int => Int] = None,
                             symmetryClassOfVariables2:Option[Int => Int] = None,
                             hotRestart:Boolean = true)
  extends EasyNeighborhood(best,name) with AlgebraTrait{
  //the indice to start with for the exploration
  var startIndice:Int = 0
  override def exploreNeighborhood(){

    val firstIterationSchemeZone =
      if (searchZone1 == null) {
        if (hotRestart && !best) {
          if (startIndice >= vars.size) startIndice = 0
          vars.indices startBy startIndice
        } else vars.indices
      } else if (hotRestart && !best) HotRestart(searchZone1(), startIndice) else searchZone1()

    val firstIterationScheme = symmetryClassOfVariables1 match {
      case None => firstIterationSchemeZone
      case Some(s) => IdenticalAggregator.removeIdenticalClassesLazily(firstIterationSchemeZone, s)
    }

    val secondIterationSchemeZone = if (searchZone2 == null) vars.indices else searchZone2()

    val secondIterationScheme = symmetryClassOfVariables2 match {
      case None => secondIterationSchemeZone
      case Some(s) => IdenticalAggregator.removeIdenticalClassesLazily(secondIterationSchemeZone, s)
    }

    //TODO : check if two different values are considered as asymmetrical
    for (i: Int <- firstIterationScheme) {
      val firstVar = vars(i)
      val oldValOfFirstVar = firstVar.value

      for (j: Int <- secondIterationScheme;
           secondVar = vars(j);
           oldValOfSecondVar = secondVar.value
           if (!symmetryCanBeBrokenOnIndices || i < j) //we break symmetry on variables
             && i != j
             && (!symmetryCanBeBrokenOnValue || oldValOfFirstVar < oldValOfSecondVar) //we break symmetry on values
             && oldValOfFirstVar != oldValOfSecondVar
             && secondVar.domain.contains(oldValOfFirstVar)
             && firstVar.domain.contains(oldValOfSecondVar)) {

        val newObj = obj.swapVal(firstVar,secondVar)

        if (moveRequested(newObj) && submitFoundMove(SwapMove(firstVar, secondVar, newObj, name))) {
          startIndice = i + 1
          return
        }
      }
    }
  }

  //this resets the internal state of the Neighborhood
  override def reset(): Unit = {
    startIndice = 0
  }
}

/**
 * Will randomize the array, typically to get out of a local minimal
 * This will not consider the objective function, even if it includes some strong constraints
 *
 *
 * @param vars an array of [[oscar.cbls.invariants.core.computation.CBLSIntVar]] defining the search space
 * @param degree the number of variables to change randomly
 * @param searchZone a subset of the indices of vars to consider.
 *                   If none is provided, all the array will be considered each time
 * @param valuesToConsider: the set of values to consider for the given variable
 * @param name the name of the neighborhood
 */
case class RandomizeNeighborhood(vars:Array[CBLSIntVar],
                                 degree:Int = 1,
                                 name:String = "RandomizeNeighborhood",
                                 searchZone:CBLSSetVar = null,
                                 valuesToConsider:(CBLSIntVar,Int) => Iterable[Int] = (variable,_) => variable.domain)
  extends Neighborhood with AlgebraTrait with SearchEngineTrait{

  override def getMove(obj: Objective, acceptanceCriteria: (Int, Int) => Boolean = null): SearchResult = {
    if(amIVerbose) println("applying " + name)

    var toReturn:List[Move] = List.empty

    if(searchZone != null && searchZone.value.size <= degree){
      //We move everything
      for(i <- searchZone.value){
        toReturn = AssignMove(vars(i),selectFrom(vars(i).domain),Int.MaxValue) :: toReturn
      }
    }else{
      var touchedVars:Set[Int] = SortedSet.empty
      for(r <- 1 to degree){
        val i = selectFrom(vars.indices,(j:Int) => (searchZone == null || searchZone.value.contains(j)) && !touchedVars.contains(j))
        touchedVars = touchedVars + i
        val oldVal = vars(i).value
        toReturn = AssignMove(vars(i),selectFrom(valuesToConsider(vars(i),i), (_:Int) != oldVal),Int.MaxValue) :: toReturn
      }
    }
    if(amIVerbose) println(name + ": move found")
    CompositeMove(toReturn, Int.MaxValue, name)
  }
}

/**
 * will randomize the array, by performing swaps only.
 * This will not consider the objective function, even if it includes some strong constraints
 *
 * @param vars an array of [[oscar.cbls.invariants.core.computation.CBLSIntVar]] defining the search space
 * @param degree the number of variables to change randomly
 * @param searchZone a subset of the indices of vars to consider.
 *                   If none is provided, all the array will be considered each time
 * @param name the name of the neighborhood
 */
case class RandomSwapNeighborhood(vars:Array[CBLSIntVar],
                                  degree:Int = 1,
                                  name:String = "RandomSwapNeighborhood",
                                  searchZone:CBLSSetVar = null)
  extends Neighborhood with AlgebraTrait with SearchEngineTrait{

  override def getMove(obj: Objective, acceptanceCriteria: (Int, Int) => Boolean = null): SearchResult = {
    if(amIVerbose) println("applying " + name)

    var toReturn:List[Move] = List.empty

    var touchedVars:Set[Int] = SortedSet.empty
    val varsToMove = if (searchZone == null) vars.length else searchZone.value.size
    for(r <- 1 to degree if varsToMove - touchedVars.size >= 2){
      val i = selectFrom(vars.indices,(i:Int) => (searchZone == null || searchZone.value.contains(i)) && !touchedVars.contains(i))
      touchedVars = touchedVars + i
      val j = selectFrom(vars.indices,(j:Int) => (searchZone == null || searchZone.value.contains(j)) && !touchedVars.contains(j))
      touchedVars = touchedVars + j
      toReturn = SwapMove(vars(i), vars(j), Int.MaxValue) :: toReturn
    }

    if(amIVerbose) println(name + ": move found")
    CompositeMove(toReturn, Int.MaxValue, name)
  }
}

/**
 *  will chose a variable in the array of variable that maximizes its violation (ties broken randomly)
 *  and find a value from its range that improves the objective function
 *  the new value can be either the best one or the first one that improves according to parameter "best"
 *
 *  notice that the search of variable is performed linearly, as for the search of new value.
 *  For a smarter search, one should use [[oscar.cbls.search.AssignNeighborhood]] and a searchZone set with [[oscar.cbls.invariants.lib.minmax.ArgMax]]
 *
 * @param c the constraint system
 * @param variables the array of variable that define the search space of this neighborhood
 * @param best true: the new value is the best one, false, the new value is the first found one that improves
 */
class ConflictAssignNeighborhood(c:ConstraintSystem, variables:List[CBLSIntVar], best:Boolean = false)
  extends Neighborhood with SearchEngineTrait{

  var varArray = variables.toArray
  val violations:Array[IntValue] = varArray.clone().map(c.violation(_))
  override def getMove(obj: Objective, acceptanceCriteria: (Int, Int) => Boolean = (oldObj,newObj) => oldObj > newObj): SearchResult = {
    val oldObj = c.violation.value
    val MaxViolVarID = selectMax(varArray.indices,violations(_:Int).value)

    val NewVal = if(best) selectMin(varArray(MaxViolVarID).domain)(c.assignVal(varArray(MaxViolVarID),_:Int))
    else selectFirst(varArray(MaxViolVarID).domain, (newVal:Int) => c.assignVal(varArray(MaxViolVarID),newVal) < oldObj)
    val newObj = c.assignVal(varArray(MaxViolVarID),NewVal)

    if(acceptanceCriteria(oldObj,newObj)){
      AssignMove(varArray(MaxViolVarID),NewVal,newObj)
    }else{
      NoMoveFound
    }
  }
}

//TODO: prévoir un shiftneighborhood, qui fait des shift de partie de tableau