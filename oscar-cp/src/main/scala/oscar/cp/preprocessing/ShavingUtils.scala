package oscar.cp.preprocessing

import oscar.cp._
import oscar.algo.search.DFSearch
import oscar.cp.core.CPOutcome

/**
  * Sascha Van Cauwelaert
  * Renaud Hartert ren.hartert@gmail.com */
object ShavingUtils {

  /** Strenghten the lower bound of the objective by complete shaving on the forwardVariables */
  def strengthenLowerBound(problem: CPStore, forwardVariables: Array[CPIntVar], objective: CPIntVar): CPOutcome = {
    
    val search = new DFSearch(problem)
    val branching = binaryFirstFail(forwardVariables)  
    
    val originalBound = objective.min
    var bestBound = Int.MaxValue
    search.onSolution {
      val bound = objective.min
      if (bound < bestBound) bestBound = bound
    }
    
    search.start(branching, _ => bestBound == originalBound) 
    
    // Update the bound 
    objective.updateMin(bestBound)
    problem.propagate()
  }
  
  /** Strenghten the upper bound of the objective by complete shaving on the forwardVariables */
  def strengthenUpperBound(problem: CPStore, forwardVariables: Array[CPIntVar], objective: CPIntVar): CPOutcome = {
    
    val search = new DFSearch(problem)
    val branching = binaryFirstFail(forwardVariables)  
    
    val originalBound = objective.max
    var bestBound = Int.MinValue
    search.onSolution {
      val bound = objective.max
      if (bound > bestBound) bestBound = bound
    }
    
    search.start(branching, _ => bestBound == originalBound)
    
    // Update the bound
    objective.updateMax(bestBound)
    problem.propagate()
  }
  
  /** Reduce the domain of variables by complete shaving on the forwardVariables */
  def reduceDomains(problem: CPStore, forwardVariables: Array[CPIntVar], variables: Array[CPIntVar]): CPOutcome = {
    
    val nVariables = variables.length
    val Variables = 0 until nVariables
    
    val search = new DFSearch(problem)
    val branching = binaryFirstFail(forwardVariables)  
    
    val originalSize = Array.tabulate(variables.length)(i => variables(i).size)
    val reducedDomains = Array.fill(variables.length)(Set[Int]())
    
    search.onSolution {
      var i = variables.length
      while (i > 0) {
        i -= 1
        for (value <- variables(i).iterator) reducedDomains(i) += value
      }
    }
    
    search.start(branching, _ => Variables.forall(i => reducedDomains(i).size == originalSize(i)))
    
    // Reduce the domains
    for (i <- Variables) {
      val domain = variables(i).iterator
      for (value <- domain) if (!reducedDomains(i).contains(value)) variables(i).removeValue(value)
    }

    problem.propagate()   
  }

  def minShavingDummy(problem: CPStore, variable: CPIntVar): CPOutcome = {
    var fail = true
    var min = variable.min
    val max = variable.max
    while (fail && min <= max + 1) {
      problem.pushState()
      fail = problem.post(variable == min) == CPOutcome.Failure
      problem.pop()
      min += 1
    }
    variable.updateMin(min - 1)
  }

  def boundsShaving(problem: CPSolver, variables: Array[CPIntVar]): CPOutcome = {
    var domainChange = true
    while (domainChange) {
      domainChange = false
      minShaving(problem, variables) match {
        case CPOutcome.Failure => return CPOutcome.Failure
        case CPOutcome.Suspend => domainChange = true
        case _ =>
      }
      maxShaving(problem, variables) match {
        case CPOutcome.Failure => return CPOutcome.Failure
        case CPOutcome.Suspend => domainChange = true
        case _ =>
      }
    }
    CPOutcome.Suspend
  }

  def minShaving(problem: CPStore, variables: Array[CPIntVar]): CPOutcome = {
    var i = 0
    var domainChange = false
    while (i < variables.length) {
      val variable = variables(i)
      val min = variable.min
      var u = variable.max
      var l = min
      var m = 0
      var fail = false
      while (l != u) {
        m = l + (u - l) / 2
        problem.pushState()
        fail = problem.post(variable <= m) == CPOutcome.Failure
        problem.pop()
        if (fail) {
          l = m + 1
        }
        else {
          u = m
        }
      }
      if (l != min) {
        domainChange = true
        if (variable.updateMin(l) == CPOutcome.Failure) {
          return CPOutcome.Failure
        }
      }
      i += 1
    }
    if (domainChange) {
      CPOutcome.Suspend
    }
    else {
      CPOutcome.Success
    }
  }

  def maxShaving(problem: CPSolver, variables: Array[CPIntVar]): CPOutcome = {
    var i = 0
    var domainChange = false
    while (i < variables.length) {
      val variable = variables(i)
      val max = variable.max
      var u = max
      var l = variable.min
      var m = 0
      var fail = false
      while (l != u) {
        m = l + (u - l) / 2

        problem.pushState()
        val fail3 = problem.isFailed()
        problem.cleanQueues()
        fail = problem.post(variable > m) == CPOutcome.Failure
        problem.pop()
//        problem.cleanQueues()
//        problem.pushState()
//        val fail2 = problem.post(variable > m) == CPOutcome.Failure
//        problem.pop()
//        if (fail != fail2) {
//          print()
//        }
        if (fail) {
          u = m
//          for (v <- u + 1 to max) {
//            problem.pushState()
//            val fail3 = problem.post(variable > m) == CPOutcome.Failure
////            problem.post(variable == v)
////            val fail2 = problem.propagate() == CPOutcome.Failure
//            problem.pop()
//            if (!fail3) {
//              println("CHIASSE")
//            }
//          }
        }
        else {
          l = m + 1
        }
      }
      if (u != max) {
//        for (v <- u + 1 to max) {
//          problem.pushState()
//          fail = problem.post(variable == v) == CPOutcome.Failure
//          problem.pop()
//          if (!fail) {
//            println("CHIASSE")
//          }
//        }

        domainChange = true
        if (problem.post(variable <= u) == CPOutcome.Failure) {
          return CPOutcome.Failure
        }
      }
      i += 1
    }
    if (domainChange) {
      CPOutcome.Suspend
    }
    else {
      CPOutcome.Success
    }
  }
}