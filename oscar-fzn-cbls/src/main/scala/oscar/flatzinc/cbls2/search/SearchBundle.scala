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
/**
 * @author Gustav Björdal
 * @author Jean-Noël Monette
 */
package oscar.flatzinc.cbls2.support

import oscar.cbls.core.computation._
import oscar.cbls.core.constraint._
import oscar.cbls.core.objective.{Objective => CBLSObjective}
import oscar.cbls.lib.invariant.logic.SelectLEHeapHeap
import oscar.flatzinc.cbls2.FZCBLSModel
import oscar.cbls.lib.search.LinearSelector
import oscar.flatzinc.cbls.support.CBLSIntVarDom
import oscar.flatzinc.cbls.support.Neighbourhood
import oscar.flatzinc.cbls.support.CBLSIntConstDom
import oscar.flatzinc.cbls.support.Move
import oscar.flatzinc.model.Objective




abstract class SearchProcedure extends LinearSelector {
  
  def run(): Unit
  def selectMinImb[R,S](r: Iterable[R] , s: R => Iterable[S],f: (R,S) => Int, st: ((R,S) => Boolean) = ((r:R, s:S) => true)): (R,S) = {
    //TODO: check that it is fine
    val flattened:Iterator[(R,S)] = for (rr <- r.toIterator; ss <- s(rr).toIterator) yield (rr,ss)
    selectMin[(R,S)](flattened.toIterable)((rands:(R,S)) => {/*Console.err.println(rands);*/ f(rands._1,rands._2)}, (rands:(R,S)) => st(rands._1,rands._2))
  }
  
  def showViolatedConstraints(c: ConstraintSystem){
    for(cc <- c.violatedConstraints){
      println(cc + " "+cc.violation.value)
    }
  }
}
class Chain(val a: SearchProcedure*) extends SearchProcedure {
  def run() = {
    a.foreach(_.run())
  }
}

class ActionSearch(action:() => Unit) extends SearchProcedure {
  def run() = action()
}
class FakeSearch extends SearchProcedure {
  def run()={}
}

class SearchControl(val m: FZCBLSModel,val MaxTimeMilli: Int,val stopOnSat:Boolean){
  def stop(): Boolean = {
    m.getWatch() >= MaxTimeMilli || //ran out of time
    (currentBestViolation==0) //found a solution
  }
  var foundSolution = false;
  var currentBestViolation = Int.MaxValue
  
  def handlePossibleSolution(){
    val curviol = m.c.violation.value
    if(currentBestViolation > curviol){
      currentBestViolation = curviol
      if(!foundSolution) m.log("Best Violation: "+curviol+ "\tat "+m.getWatch()+ " ms")
    }
    if(curviol==0){
      m.handleSolution();
      foundSolution = true
    }
  }
  def stopAll(): Boolean = {
    m.getWatch() >= MaxTimeMilli || 
    (m.model.search.obj match{
      case Objective.SATISFY => foundSolution
      case Objective.MAXIMIZE => 
        if(foundSolution && m.objectiveVar.value == m.objectiveVar.max){
          println("==========")
          true
        }else false
      case Objective.MINIMIZE => 
        if(foundSolution && m.objectiveVar.value == m.objectiveVar.min){
          println("==========")
          true
        }else false
    })
  }
  def run(search: SearchProcedure){
    //currentBestViolation = m.c.violation.value
    //handlePossibleSolution()
    while(!stopAll()){
      m.updateVarDomains()//TODO: Is this necessary or wished?
      currentBestViolation = m.c.violation.value
      search.run()
    }
  }
  
}


abstract class NeighbourhoodSearch(val m: FZCBLSModel,val sc: SearchControl) extends SearchProcedure {
  val log = m.log
  val neighbourhoods: List[Neighbourhood] = m.neighbourhoods 
  val searchVariables = neighbourhoods.foldLeft(Set.empty[CBLSIntVarDom])((acc: Set[CBLSIntVarDom], x: Neighbourhood) => acc ++ x.getVariables().filterNot(_.isInstanceOf[CBLSIntConstDom])).toArray

  
}


class NeighbourhoodTabuSearch(m: FZCBLSModel, sc: SearchControl) extends NeighbourhoodSearch(m,sc){
  
  val variableMap = (0 until searchVariables.length).foldLeft(Map.empty[CBLSIntVar, Int])((acc, x) => acc + (searchVariables(x) -> x));  
  
  val it = CBLSIntVar(m.m, 1, 0 to Int.MaxValue, "it");
  val tabu: Array[CBLSIntVar] = searchVariables.map(v => CBLSIntVar(sc.m.m, 0, 0 to Int.MaxValue,  "Tabu_" + v.name)).toArray;
  val nonTabuVariables = SelectLEHeapHeap(tabu.asInstanceOf[Array[IntValue]], it);
  val MaxTenure = (searchVariables.length * 0.6).toInt;
  val MinTenure = 2 + 0 * (searchVariables.length * 0.1).toInt;
  val tenureIncrement = Math.max(1, (MaxTenure - MinTenure) / 10);
  var tenure = MinTenure
  var ecnt = 0;
  var bcnt = 0;
  val baseSearchSize = 100;
  val searchFactor = 20;
  
  def acceptMove(best: Int,nonTabuSet: Set[CBLSIntVar])(m:Move): Boolean = {
    //changed forall to exists after a suggestion of Gustav.
    m.getModified.exists(nonTabuSet.contains(_)) || m.value < best 
  }
  
  def makeMove(extendedSearch: Boolean){
    
    //if(it.value%10==0){
    //    log(2,"it: "+it.value+" violation: "+m.c.violation.value+" objective: "+m.objective.getObjectiveValue())
    //  }
    log(3,"it: "+it.value+" violation: "+m.c.violation.value/*+" objective: "+m.objectiveVar.value*/)
    //if(m.c.violation.value==0)System.exit(0)
    //if(log.level>=4) for(cc <- m.c.violatedConstraints){
    //    log(4,cc + " "+cc.violation.value)
    //}
    //log(3,"Non tabu variables: "+nonTabuVariables.value.size)
    val nonTabuSet = nonTabuVariables.value.map(searchVariables(_).asInstanceOf[CBLSIntVar]);
    val bestValue = sc.currentBestViolation
    if(extendedSearch) ecnt+=1 else bcnt+=1
    val bestNeighbour = selectMin(neighbourhoods.map((n: Neighbourhood) =>
      if (extendedSearch) {
        //log(3,"E "+n)
        n.getExtendedMinObjective(it.value, acceptMove(bestValue,nonTabuSet)/*, bestNow*/)
      } else {
        //log(3,"S "+n)
        n.getMinObjective(it.value, acceptMove(bestValue,nonTabuSet))
      }))(_.value)
//      println("X")
    if(bestNeighbour!=null){
      if(log.level > 0 && bestNeighbour.getModified.forall(!nonTabuSet.contains(_))){
        log(2,"Aspiration");
        log(3,bestNeighbour.value.toString +" < "+bestValue.toString /*+" ; "+m.objectiveVar.value*/)
      }
      log(3,bestNeighbour.toString)
      log(4,tabu.filter(t => t.value > it.value).toList.toString())
      bestNeighbour.commit();
      sc.handlePossibleSolution()
    }else
      log("No move exists!");
    val modifiedVars = if(bestNeighbour!=null) bestNeighbour.getModified else Set.empty[CBLSIntVar]
    for (v <- modifiedVars) {
      val index = variableMap(v);
      //This could be it.value + tenure + random(tenureIncrement) to introduce more randomness
      //tabu(index) := it.value + tenure;
      tabu(index) := it.value + Math.min(MaxTenure, tenure + RandomGenerator.nextInt(tenureIncrement));
    }
    it:+=1;
  }

  override def run()= {
    sc.handlePossibleSolution()
    log("Starting Satisfaction Search")
    var extendedSearch = false;
    var roundsWithoutSat = 0;
    val maxRounds = 5;

    var timeOfBest = m.getWatch();
    var itSinceBest = 0;
    var bestViolation = Int.MaxValue


    var wait = 0;
    val waitDec = 1;
    
    while (!sc.stop()) {
      makeMove(extendedSearch)
      
      if (wait > 0) {
        wait -= waitDec;
      } else {
        itSinceBest += 1;
      }
      if (m.c.violation.value < bestViolation) {
        bestViolation = m.c.violation.value
        itSinceBest = 0;
        timeOfBest = m.getWatch()
        tenure = Math.max(MinTenure, tenure - 1)
        if (tenure == MinTenure) {
          extendedSearch = false;
        }
      }
      if (m.c.violation.value > bestViolation * 10) {
        extendedSearch = true;
      }
      if (itSinceBest > tenure + baseSearchSize + searchFactor * (tenure / tenureIncrement)) {
        extendedSearch = true;
        itSinceBest = 0;
        tenure = Math.min(MaxTenure, tenure + tenureIncrement);
        if (tenure == MaxTenure) {
          //Wait will be long enough to clear the tabu list.
          wait = tenure + baseSearchSize;
          bestViolation = Int.MaxValue
          tenure = MinTenure;
          roundsWithoutSat += 1;
          if (roundsWithoutSat >= maxRounds) {
            log("Reset neighourhoods")
            for (n <- neighbourhoods)
              n.reset();
            roundsWithoutSat = 0;
            bestViolation = m.c.violation.value
          }
        }
      }
      if(m.getWatch() > timeOfBest + 300000){
        timeOfBest = m.getWatch()
        log("Reset neighourhoods 5 minutes")
        for (n <- neighbourhoods)
          n.reset();
      }
    }
    log("Completed Satisfaction Search at "+m.getWatch())
    log("nb moves ext: "+ecnt+"\tbsc: "+bcnt)
  }
}