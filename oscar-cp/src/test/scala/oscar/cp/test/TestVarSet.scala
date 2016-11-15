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
package oscar.cp.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import oscar.algo.search.Outcome
import oscar.cp._
import oscar.cp.core.CPPropagStrength

/**
 * @author: Pierre Schaus pschaus@gmail.com
 */
class TestVarSet extends FunSuite with ShouldMatchers  {


  test("Test Set 1") {
    
    val cp = CPSolver()
    var x = new CPSetVar(cp, -2 , 4)
    var inpropag = false
    class SetCons(val X: CPSetVar) extends Constraint(X.store, "TestSet") {
      
    	override def setup(l: CPPropagStrength): Outcome = {
    	    X.callPropagateWhenDomainChanges(this)
    		return Outcome.Suspend
    	}

    	override def propagate(): Outcome = {
    	  inpropag = true
          return Outcome.Suspend
        }

    }
    

    cp.add(new SetCons(x))
    
    inpropag = false
    cp.add(x ++ 0)
    inpropag should be(true)
    
    inpropag = false
    cp.add(x ++ 0)
    inpropag should be(false)    
    
    inpropag = false
    cp.add(x -- -2)
    inpropag should be(true)

    inpropag = false
    cp.add(x -- 4)
    inpropag should be(true)

    inpropag = false
    cp.add(x -- 4)
    inpropag should be(false)

    inpropag = false
    cp.add(x -- 1)
    inpropag should be(true)       

  }
  
  test("Test Set 2") {
    
    val cp = CPSolver()
    var x = new CPSetVar(cp, -2 , 4)
    var inpropag = false
    var inexclude = false
    var inrequire = false
    
    def reset() = {
      inpropag = false
      inexclude = false
      inrequire = false
    }
    class SetCons(val X: CPSetVar) extends Constraint(X.store, "TestSet") {

      
    	override def setup(l: CPPropagStrength): Outcome = {
    	    X.callPropagateWhenDomainChanges(this)
    	    X.callValExcludedWhenExcludedValue(this)
    	    X.callValRequiredWhenRequiredValue(this)
    		return Outcome.Suspend
    	}

    	override def propagate(): Outcome = {
    	  inpropag = true
          return Outcome.Suspend
        }
    	
    	override def valExcluded(x: CPSetVar, v: Int): Outcome = {
    	  inexclude = true
    	  return Outcome.Suspend
    	}
    	
    	
    	override def valRequired(x: CPSetVar, v: Int): Outcome = {
    	  inrequire = true
    	  return Outcome.Suspend
    	}
    }
    

    cp.add(new SetCons(x))
    
    reset()
    cp.add(x ++ 0)
    inpropag should be(true)
    inrequire should be(true)
    inexclude should be(false)
    
    reset()
    cp.add(x ++ 0)
    inpropag should be(false)    
    inrequire should be(false)
    inexclude should be(false)
    
    
    reset()
    cp.add(x ++ -2)
    inpropag should be(true)
    inrequire should be(true)
    inexclude should be(false)

    reset()
    cp.add(x -- 4)
    inpropag should be(true)
    inrequire should be(false)
    inexclude should be(true)

    reset()
    cp.add(x -- 4)
    inpropag should be(false)
    inrequire should be(false)
    inexclude should be(false)
    

    reset()
    cp.add(x -- 1)
    inpropag should be(true) 
    inrequire should be(false)
    inexclude should be(true)    

  }  
  
  
  test("Test Set 3") {
    
    val cp = CPSolver()
    var x = new CPSetVar(cp, -2 , 4)
    var inpropag = false
    var inexclude = false
    var inrequire = false
    var idx = -100
    var value = -100
    
    def reset() = {
      inpropag = false
      inexclude = false
      inrequire = false
      idx = -100
      value = -100
    }
    class SetCons(val X: CPSetVar) extends Constraint(X.store, "TestSet") {
      
    	override def setup(l: CPPropagStrength): Outcome = {
    	    X.callPropagateWhenDomainChanges(this)
    	    X.callValExcludedIdxWhenExcludedValue(this,100)
    	    X.callValRequiredIdxWhenRequiredValue(this,1000)
    		return Outcome.Suspend
    	}

    	override def propagate(): Outcome = {
    	  inpropag = true
          return Outcome.Suspend
        }
    	
    	override def valExcludedIdx(x: CPSetVar, i: Int, v: Int): Outcome = {
    	  inexclude = true
    	  idx = i
    	  value = v
    	  return Outcome.Suspend
    	}
    	
    	
    	override def valRequiredIdx(x: CPSetVar, i: Int, v: Int): Outcome = {
    	  inrequire = true
     	  idx = i
    	  value = v   	  
    	  return Outcome.Suspend
    	}
    }
    

    cp.add(new SetCons(x))
    
    reset()
    cp.add(x ++ 0)
    inpropag should be(true)
    inrequire should be(true)
    inexclude should be(false)
    idx should be(1000)
    value should be(0)
    
    reset()
    cp.add(x ++ 0)
    inpropag should be(false)    
    inrequire should be(false)
    inexclude should be(false)
    idx should be(-100)
    value should be(-100)
    
    
    reset()
    cp.add(x ++ -2)
    inpropag should be(true)
    inrequire should be(true)
    inexclude should be(false)
    idx should be(1000)
    value should be(-2)    

    reset()
    cp.add(x -- 4)
    inpropag should be(true)
    inrequire should be(false)
    inexclude should be(true)
    idx should be(100)
    value should be(4)     

    reset()
    cp.add(x -- 4)
    inpropag should be(false)
    inrequire should be(false)
    inexclude should be(false)
    idx should be(-100)
    value should be(-100)    
    

    reset()
    cp.add(x -- 1)
    inpropag should be(true) 
    inrequire should be(false)
    inexclude should be(true)
    idx should be(100)
    value should be(1)      

  }  

  test("Test Set 4") {
    
    val cp = CPSolver()
    var x = new CPSetVar(cp, -2 , 4)
    var inpropag = false
    var inexclude = false
    var inrequire = false
    
    def reset() = {
      inpropag = false
      inexclude = false
      inrequire = false
    }
    class SetCons(val X: CPSetVar) extends Constraint(X.store, "TestSet") {

      
    	override def setup(l: CPPropagStrength): Outcome = {
    	    X.callPropagateWhenDomainChanges(this)
    	    X.callValExcludedWhenExcludedValue(this)
    	    X.callValRequiredWhenRequiredValue(this)
    		return Outcome.Suspend
    	}

    	override def propagate(): Outcome = {
    	  inpropag = true
          return Outcome.Suspend
        }
    	
    	override def valExcluded(x: CPSetVar, v: Int): Outcome = {
    	  inexclude = true
    	  return Outcome.Suspend
    	}
    	
    	
    	override def valRequired(x: CPSetVar, v: Int): Outcome = {
    	  inrequire = true
    	  return Outcome.Suspend
    	}
    }
    

    cp.add(new SetCons(x))
    
    reset()
    cp.add(x ++ 0)
    inpropag should be(true)
    inrequire should be(true)
    inexclude should be(false)
    
    reset()
    cp.add(x ++ 0)
    inpropag should be(false)    
    inrequire should be(false)
    inexclude should be(false)
    
    
    reset()
    cp.add(x ++ -2)
    inpropag should be(true)
    inrequire should be(true)
    inexclude should be(false)

    reset()
    cp.add(x -- 4)
    inpropag should be(true)
    inrequire should be(false)
    inexclude should be(true)

    reset()
    cp.add(x -- 4)
    inpropag should be(false)
    inrequire should be(false)
    inexclude should be(false)
    

    reset()
    cp.add(x -- 1)
    inpropag should be(true) 
    inrequire should be(false)
    inexclude should be(true)    

  }  
  
  
  test("Test Set 5") {
    
    val cp = CPSolver()
    var x = new CPSetVar(cp, -2 , 4)
    var inpropag = false
    var inexclude = false
    var inrequire = false
    var idx = -100
    var excluded = Set[Int]()
    
    def reset() = {
      inpropag = false
      inexclude = false
      inrequire = false
      idx = -100
    }
    class SetCons(val X: CPSetVar) extends Constraint(X.store, "TestSet") {
      
    	override def setup(l: CPPropagStrength): Outcome = {
    	    X.callPropagateWhenDomainChanges(this)
    	    X.callValExcludedIdxWhenExcludedValue(this,100)
    	    X.callValRequiredIdxWhenRequiredValue(this,1000)
    		return Outcome.Suspend
    	}

    	override def propagate(): Outcome = {
    	  inpropag = true
          return Outcome.Suspend
        }
    	
    	override def valExcludedIdx(x: CPSetVar, i: Int, v: Int): Outcome = {
    	  excluded = excluded + v
    	  inexclude = true
    	  idx = i
    	  return Outcome.Suspend
    	}
    	
    	
    	override def valRequiredIdx(x: CPSetVar, i: Int, v: Int): Outcome = {
    	  inrequire = true
     	  idx = i  	  
    	  return Outcome.Suspend
    	}
    }
    

    cp.add(new SetCons(x))
    
    reset()
    cp.add(x ++ 0)
    cp.add(new Constraint(cp, "TestSet") {
      override def setup(l: CPPropagStrength): Outcome = x.excludesAll()  // -2,-1,1,2,3,4 should be notified as removed
    })
    
    x.possibleSet should be(Set(0))
    x.requiredSet should be(Set(0))

    inpropag should be(true)
    inrequire should be(true)
    inexclude should be(true)
    excluded should be(Set(-2,-1,1,2,3,4))
    idx should be(100)    

  }
  
  test("Test Set 6") {
    
    val cp = CPSolver()
    var x = new CPSetVar(cp, -2 , 4)
    var inpropag = false
    var inexclude = false
    var inrequire = false
    var idx = -100
    var included = Set[Int]()
    
    def reset() = {
      inpropag = false
      inexclude = false
      inrequire = false
      idx = -100
    }
    class SetCons(val X: CPSetVar) extends Constraint(X.store, "TestSet") {
      
    	override def setup(l: CPPropagStrength): Outcome = {
    	    X.callPropagateWhenDomainChanges(this)
    	    X.callValExcludedIdxWhenExcludedValue(this,100)
    	    X.callValRequiredIdxWhenRequiredValue(this,1000)
    		return Outcome.Suspend
    	}

    	override def propagate(): Outcome = {
    	  inpropag = true
          return Outcome.Suspend
        }
    	
    	override def valExcludedIdx(x: CPSetVar, i: Int, v: Int): Outcome = {
    	  
    	  inexclude = true
    	  idx = i
    	  return Outcome.Suspend
    	}
    	
    	
    	override def valRequiredIdx(x: CPSetVar, i: Int, v: Int): Outcome = {
    	  included = included + v
    	  inrequire = true
     	  idx = i  	  
    	  return Outcome.Suspend
    	}
    }
    

    cp.add(new SetCons(x))
    
    reset()
    cp.add(x -- 0)
    cp.add(new Constraint(cp, "TestSet") {
      override def setup(l: CPPropagStrength): Outcome = x.requiresAll()  // -2,-1,1,2,3,4 should be notified as removed
    })
    x.possibleSet should be(Set(-2,-1,1,2,3,4))
    x.requiredSet should be(Set(-2,-1,1,2,3,4))
    
    inpropag should be(true)
    inrequire should be(true)
    inexclude should be(true)
    included should be(Set(-2,-1,1,2,3,4))
    idx should be(1000)    

  }    
  
}
