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

package oscar.cp.constraints

import oscar.cp.core._
import java.util.{Arrays => JArrays}

import java.util.Comparator
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.variables.CPBoolVar

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
class UnaryResourceWithOptionalActivities(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], required: Array[CPBoolVar]) extends Constraint(starts(0).store) {
  priorityL2 = 1
  idempotent = false
  def this(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], resources: Array[CPIntVar], id: Int) = this(starts,durations,ends,resources.map(_.isEq(id)))
  
  def this(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar]) = {
    this (starts,durations,ends,Array.fill(starts.size)(CPBoolVar(true)(starts(0).store)))
  }

  val n = starts.size // number of activities

  assert(durations.size == n)
  assert(ends.size == n)
  assert(required.size == n)

  val activities = Array.tabulate(n) { i =>
    new ActivityUnary(s, starts(i), durations(i), ends(i), required(i), i)
  }
  // mirror activities
  val mactivities = Array.tabulate(n) { i =>
    new MirrorActivityUnary(activities(i))
  }

  val ect = Array.tabulate(n)(i => activities(i))
  val est = Array.tabulate(n)(i => activities(i))
  val lct = Array.tabulate(n)(i => activities(i))
  val lst = Array.tabulate(n)(i => activities(i))

  val mect: Array[ActivityUnary] = Array.tabulate(n)(i => mactivities(i))
  val mest: Array[ActivityUnary] = Array.tabulate(n)(i => mactivities(i))
  val mlct: Array[ActivityUnary] = Array.tabulate(n)(i => mactivities(i))
  val mlst: Array[ActivityUnary] = Array.tabulate(n)(i => mactivities(i))

  val newEst = Array.fill(n)(Int.MinValue)
  val newLct = Array.fill(n)(Int.MaxValue)

  val lambdaThetaTree = new LambdaThetaTreee2(n)

  val estComp = new ESTComparator2()
  val lstComp = new LSTComparator2()
  val ectComp = new ECTComparator2()
  val lctComp = new LCTComparator2()

  var failure = false

  override def setup(l: CPPropagStrength): CPOutcome = {

    // to do, link start, end, dur

    for (i <- 0 until n) {
      starts(i).callPropagateWhenBoundsChange(this)
      ends(i).callPropagateWhenBoundsChange(this)
      if (!required(i).isBound) {
        required(i).callPropagateWhenBind(this)
      }
    }

    if (propagate() == CPOutcome.Failure) {
      return CPOutcome.Failure
    }

    return CPOutcome.Suspend
  }

  override def propagate(): CPOutcome = {
    //println("propagate start...")
    //println(activities.mkString("\n"))
    
    for (i <- 0 until n) {
      activities(i).update() // forces update of start, end, dur
    }
    //println("after update...")
    //println(activities.mkString("\n"))
    
    
    failure = false
//    do {
      do {
        do {
          if (!overloadChecking()) {
            return CPOutcome.Failure
          }
        } while (!failure && detectablePrecedences())
      } while (!failure && notFirstNotLast() && !failure)
//    } while (!failure && edgeFinder())

    if (failure) {
      return CPOutcome.Failure
    } else {
      return CPOutcome.Suspend
    }

  }  
  

  private def updateEst() {
    JArrays.sort(est, estComp)
    for (i <- 0 until n) {
      est(i).estPos = i
    }
  }
  
  private def updateMEst() {
    JArrays.sort(mest, estComp)
    for (i <- 0 until n) {
      mest(i).estPos = i
    }
  }

  private def overloadChecking(): Boolean = {
    //println("overload checking start")
    // Init
    updateEst() // update the activity wrappers such that they now their position according to a non decreasing est sorting

    // left to right
    JArrays.sort(lct, lctComp)
    lambdaThetaTree.reset()
    for (i <- 0 until n) {
      val aw = lct(i)
      if (!aw.isForbidden) { // skip forbidden activities
        lambdaThetaTree.insert(aw, aw.estPos)
        if (aw.isOptional) {
          lambdaThetaTree.grey(aw.estPos)
        } else if (aw.isMandatory) {
          if (lambdaThetaTree.ect > aw.lct) {
            return false
          }
        }
        while (lambdaThetaTree.ectOpt > aw.lct) {
          val j = lambdaThetaTree.responsibleEct
          val actj = est(j).index
          val ok = activities(actj).setForbidden()
          assert(ok != CPOutcome.Failure)
          lambdaThetaTree.remove(j)
        }
      }
    }
    
    // right to left
    updateMEst()
    JArrays.sort(mlct, lctComp)
    lambdaThetaTree.reset()
    for (i <- 0 until n) {
      val aw = mlct(i)
      if (!aw.isForbidden) { // skip forbidden activities
        lambdaThetaTree.insert(aw, aw.estPos)
        if (aw.isOptional()) {
          lambdaThetaTree.grey(aw.estPos)
        } else if (aw.isMandatory) {
          if (lambdaThetaTree.ect > aw.lct) {
            return false
          }
        }
        while (lambdaThetaTree.ectOpt > aw.lct) {
          val j = lambdaThetaTree.responsibleEct
          val actj = mest(j).index
          val ok = mactivities(actj).setForbidden()
          assert(ok != CPOutcome.Failure)
          lambdaThetaTree.remove(j)
        }
      }
    }
    //println("end of overload:")
    //println(activities.mkString("\n"))
    return true
  }

  private def notFirstNotLast(): Boolean = {

    // Init
    updateEst()
    for (i <- 0 until n) {
      newEst(i) = activities(i).est
      newLct(i) = activities(i).lct
    }

    // Push in one direction.
    JArrays.sort(lst, lstComp) // order activities in non decreasing lct_j - p_j (i.e. latest starting time)
    JArrays.sort(lct, lctComp)

    lambdaThetaTree.reset()
    var j = 0
    while (j < n && !lst(j).isMandatory()) { // skip non mandatory
      j += 1
    }
    for (i <- 0 until n) {
      val awi = lct(i)
      if (lct(i).isMandatory()) {
        while (j < n && awi.lct > lst(j).lst) {
          if (j > 0 && lambdaThetaTree.ect > lst(j).lst) {
            newLct(lst(j).index) = lst(j - 1).lst
          }
          lambdaThetaTree.insert(lst(j), lst(j).estPos)
          j += 1
          while (j < n && !lst(j).isMandatory()) { // skip non mandatory
            j += 1
          }
        }
        val inserted = lambdaThetaTree.isInserted(awi.estPos)
        if (inserted) {
          lambdaThetaTree.remove(awi.estPos)
        }
        val ectThetaLessi = lambdaThetaTree.ect
        if (inserted) {
          lambdaThetaTree.insert(awi, awi.estPos)
        }
        //if (ectThetaLessi > awi.lct && j > 0) {
        if (ectThetaLessi > awi.lst && j > 0) {
          newLct(awi.index) = Math.min(newLct(awi.index), lst(j - 1).lst)
        }
      }
    }

    // Push in other direction.
    JArrays.sort(mlst, lstComp)
    JArrays.sort(mlct, lctComp)
    lambdaThetaTree.reset()
    j = 0
    while (j < n && !mlst(j).isMandatory()) { // skip non mandatory
      j += 1
    }
    for (i <- 0 until n) {
      val awi = mlct(i)
      if (awi.isMandatory()) {
        while (j < n && awi.lct > mlst(j).lst) {
          if (j > 0 && lambdaThetaTree.ect > mlst(j).lst) {
            newEst(mlst(j).index) = -mlst(j - 1).lst
          }
          lambdaThetaTree.insert(mlst(j), mlst(j).estPos)
          j += 1
          while (j < n && !mlst(j).isMandatory()) { // skip non mandatory
            j += 1
          }
        }
        val inserted = lambdaThetaTree.isInserted(awi.estPos)
        if (inserted) {
          lambdaThetaTree.remove(awi.estPos)
        }
        val mectThetaLessi = lambdaThetaTree.ect
        if (inserted) {
          lambdaThetaTree.insert(awi, awi.estPos)
        }
        //if (mectThetaLessi > awi.lct && j > 0) {
        if (mectThetaLessi > awi.lst && j > 0) {
          newEst(awi.index) = Math.max(newEst(awi.index), -mlst(j - 1).lst)
        }
      }
    }

    // Apply modifications
    var modified = false
    for (i <- 0 until n) {
      if (required(i).isTrue) {
        if (activities(i).lct > newLct(i) || activities(i).est < newEst(i)) {
          modified = true
          // println("NFNL pushed")

          if (activities(i).start.updateMin(newEst(i)) == CPOutcome.Failure) {
            failure = true
          }

          if (activities(i).end.updateMax(newLct(i)) == CPOutcome.Failure) {
            failure = true
          }
        }
      }
    }
    return modified

  } //end of notFirstNotLast 
  
  
	private def detectablePrecedences(): Boolean = {
		// Init
		updateEst()
		// Propagate in one direction
		JArrays.sort(ect, ectComp) // order activities in non decreasing order of est_i + p_i (i.e. earliest completion time)
		JArrays.sort(lst, lstComp) // order activities in non decreasing lct_j - p_j (i.e. latest starting time)
		lambdaThetaTree.reset()

		var j = 0
		for (i <-  0 until n) { // for i in T in non decreasing order of est_i + p_i
			val awi = ect(i)
			if (awi.isMandatory()) {
				while (j < n && !lst(j).isMandatory()) { // skip non mandatory
					j += 1
				}
				if (j < n) {
					var awj = lst(j)
					while (awi.ect > awj.lst && j < n) {
						lambdaThetaTree.insert(awj, awj.estPos)
						j += 1
						while (j < n && !lst(j).isMandatory()) { // skip non mandatory
							j += 1
						}
						if (j < n) {
						  awj = lst(j)
						}
					}
				}
				val esti = awi.est
				val inserted = lambdaThetaTree.isInserted(awi.estPos)
				if (inserted) {
					lambdaThetaTree.remove(awi.estPos)
				}
				val oesti = lambdaThetaTree.ect
				if (inserted) {
					lambdaThetaTree.insert(awi, awi.estPos)
				}
				if (oesti > esti) {
					newEst(awi.index) = oesti
				} else {
					newEst(awi.index) = Int.MinValue
				}
			}
		}

		// Propagate in other direction
    JArrays.sort(mlst, lstComp)
		JArrays.sort(mect, ectComp)
		lambdaThetaTree.reset()
		j = 0
		for (i <- 0 until n) {
			val awi = mect(i)
			if (awi.isMandatory()) {
				if (j < n) {
					while (j < n && !mlst(j).isMandatory()) { // skip non mandatory
						j += 1
					}
					var awj = mlst(j)
					while (awi.ect > awj.lst && j < n) {
						lambdaThetaTree.insert(awj, awj.estPos)
						j += 1
						while (j < n && !mlst(j).isMandatory()) { // skip non mandatory
							j += 1
						}						
						if (j < n) {
						  awj = mlst(j)
						}
						
					}
				}
				val lcti = awi.est
				val inserted = lambdaThetaTree.isInserted(awi.estPos)
				if (inserted) {
					lambdaThetaTree.remove(awi.estPos)
				}
				val olcti = lambdaThetaTree.ect
				if (inserted) {
					lambdaThetaTree.insert(awi, awi.estPos)
				}
				if (olcti > lcti) {
					newLct(awi.index) = -olcti
				} else {
					newLct(awi.index) = Int.MaxValue
				}
			}
		}

		// Apply modifications
		var modified = false
		for (i <- 0 until n) {
			if (required(i).isTrue) {
				if (newEst(i) != Int.MinValue) {
					modified = true
          // println("DP pushed")
					if (activities(i).start.updateMin(newEst(i)) == CPOutcome.Failure) {
						failure = true
					}
				}
				if (newLct(i) != Int.MaxValue) {
					modified = true
					if (activities(i).end.updateMax(newLct(i)) == CPOutcome.Failure) {
						failure = true
					}
				}
			}
		}
		return modified
	}
  

  private def edgeFinder(): Boolean = {
    
    
    
    //println("--------------edge--------")
    //activities.foreach(a => println(a))
    // Init
    updateEst()
    for (i <- 0 until n) {
      newEst(i) = activities(i).est
      newLct(i) = activities(i).lct
    }

    // Push in one direction.
    JArrays.sort(lct, lctComp)
    lambdaThetaTree.reset()

    for (i <- 0 until n) {
      if (est(i).isMandatory) {
        lambdaThetaTree.insert(est(i), i)
      }
    }
    var j = n - 1
    while (j >= 0 && !lct(j).isMandatory) {
      j -= 1
    }
    while (j >= 0) { // we consider mandatory only
      var awj = lct(j)
      lambdaThetaTree.grey(awj.estPos)
      j -= 1
      while (j >= 0 && !lct(j).isMandatory) { j -= 1 }
      if (j >= 0) {
        awj = lct(j)
        if (lambdaThetaTree.ect > awj.lct) {
          failure = true // Resource is overloaded
          return false
        }
        while (lambdaThetaTree.ectOpt > awj.lct) {
          val i = lambdaThetaTree.responsibleEct
          assert(i >= 0)
          val acti = est(i).index
          if (lambdaThetaTree.ect > newEst(acti)) {
            newEst(acti) = lambdaThetaTree.ect
          }
          lambdaThetaTree.remove(i)
        }
      }

    }
    // Push in other direction.

    updateMEst()
    JArrays.sort(mlct, lctComp)
    
    //println("est:"+mest.mkString(","))
    //println("lct:"+mlct.mkString(","))
    //println("mandatory lct:"+mlct.map(_.isMandatory).mkString(","))
    
    lambdaThetaTree.reset()
    for (i <- 0 until n) {
      if (mest(i).isMandatory) {
        //println("insert:"+mest(i)+" in position "+i)
        lambdaThetaTree.insert(mest(i), i)
      }
    }
    //println("initial tree ect:"+lambdaThetaTree.ect)
    j = n - 1
    while (j >= 0 && !mlct(j).isMandatory) {
      //println("activity at "+ j +" mandatory?"+lct(j).isMandatory+ " " + lct(j))
      j -= 1
    }
    
    
    
    while (j >= 0) {
      var awj = mlct(j)
      //println("j:"+j+" activity awj:"+awj)
      //println("greying position "+awj.estPos)
      lambdaThetaTree.grey(awj.estPos)
      j -= 1
      while (j >= 0 && !mlct(j).isMandatory) { j -= 1 }
      if (j >= 0) {
        awj = mlct(j)
        
        if (lambdaThetaTree.ect > awj.lct) {
          //println("failure here?"+lambdaThetaTree.ect +">"+ awj.lct )
          failure = true // Resource is overloaded
          return false
        }
        while (lambdaThetaTree.ectOpt > awj.lct) {
          val i = lambdaThetaTree.responsibleEct
          assert(i >= 0)
          val acti = mest(i).index
          if (-lambdaThetaTree.ect < newLct(acti)) {
            newLct(acti) = -lambdaThetaTree.ect
          }
          lambdaThetaTree.remove(i)
        }
      }

    }
    
    
    // Apply modifications.
    var modified = false
    for (i <- 0 until n) {
      if (required(i).isTrue) {
        
        if (activities(i).est < newEst(i)) {
          println("EF pushed")
          modified = true
          if (activities(i).start.updateMin(newEst(i)) == CPOutcome.Failure) {
            failure = true
            return false
          }
        }
        
        if (activities(i).lct > newLct(i)) {
          println("EF pushed")
          modified = true
          if (activities(i).end.updateMax(newLct(i)) == CPOutcome.Failure) {
            failure = true
            return false
          }
        }
        
      }

    }
    return modified
  }

}

/**
 * This object is used internally by UnaryResource, it is not intended to be used in modeling
 * @author Pierre Schaus pschaus@gmail.com
 */
class ActivityUnary(val cp: CPStore, startVar: CPIntVar, durVar: CPIntVar, endVar: CPIntVar, requiredVar: CPBoolVar, val index: Int) {

  var estPos = -1

  // The variables
  def start = startVar
  def end = endVar
  def dur = durVar
  def required = requiredVar

  val  infty: Int = Int.MaxValue << 2;
  
  // Earliest starting time
  def est = start.min
  // Latest starting time
  def lst = start.max // if (required.isTrue) start.max else infty
  // Earliest completion time assuming the smallest duration
  def ect = end.min
  // Latest completion time assuming the smallest duration
  def lct = end.max // if (required.isTrue) end.max else infty

  // Current minimal duration of this activity
  def minDuration = dur.min
  // Current maximal duration of this activity
  def maxDuration = dur.max

  /**
   * @return true if the activity may be possibly be scheduled on this resource
   */
  def isOptional() = !required.isBound

  /**
   * @return true if the activity must be scheduled on this resource
   */
  def isMandatory() = required.isTrue

  /**
   * @return true if the activity cannot be scheduled on this resource
   */
  def isForbidden() = required.isFalse

  /**
   * @return set the activity forbidden on this resource
   */
  def setForbidden(): CPOutcome = required.assign(0)

  override def toString = "ActivityUnary" + "(est: " + est + ", d: " + dur + ", lct: " + lct + ", req:"+ required + ", estPos:" + estPos + ")"

  def adjustStart(v: Int): CPOutcome = {

    if (startVar.updateMin(v) == CPOutcome.Failure)
      return CPOutcome.Failure

    if (endVar.updateMin(v + durVar.min) == CPOutcome.Failure)
      return CPOutcome.Failure

    return CPOutcome.Suspend
  }

  /**
   * forces the update of start, end, dur
   */
  def update(): CPOutcome = {

    // end <= start
    if (end.updateMin(start.min) == CPOutcome.Failure) {
      CPOutcome.Failure
    } else if (start.updateMax(end.max) == CPOutcome.Failure) {
      CPOutcome.Failure
    } // end = start + dur
    else if (end.updateMax(start.max + dur.max) == CPOutcome.Failure) {
      CPOutcome.Failure
    } else if (end.updateMin(start.min + dur.min) == CPOutcome.Failure) {
      CPOutcome.Failure
    } // start = end - dur
    else if (start.updateMax(end.max - dur.min) == CPOutcome.Failure) {
      CPOutcome.Failure
    } else if (start.updateMin(end.min - dur.max) == CPOutcome.Failure) {
      CPOutcome.Failure
    } // dur = end - start
    else if (dur.updateMax(end.max - start.min) == CPOutcome.Failure) {
      CPOutcome.Failure
    } else if (dur.updateMin(end.min - start.max) == CPOutcome.Failure) {
      CPOutcome.Failure
    } else CPOutcome.Suspend
  }
}

class MirrorActivityUnary(val act: ActivityUnary) extends ActivityUnary(act.cp, act.start, act.dur, act.end, act.required, act.index) {

  override def start: CPIntVar = throw new UninitializedFieldError("not available")
  override def end: CPIntVar = throw new UninitializedFieldError("not available")

  // Earliest starting time
  override def est = -act.lct
  // Latest starting time
  override def lst = -act.ect
  // Earliest completion time assuming the smallest duration
  override def ect = -act.lst
  // Latest completion time assuming the smallest duration
  override def lct = -act.est

  override def adjustStart(v: Int): CPOutcome = {

    if (end.updateMax(-v) == CPOutcome.Failure)
      return CPOutcome.Failure

    if (start.updateMax(-v + dur.min) == CPOutcome.Failure)
      return CPOutcome.Failure

    return CPOutcome.Suspend
  }

  override def toString() = "mirror" + super.toString()

}

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
class LambdaThetaTreee2(private val size: Int) {

  // http://en.wikipedia.org/wiki/Binary_heap#Adding_to_the_heap
  private var isize = 1
  //enumerate mupltiples of two 2, 4, 6, 8 ... until isize larger than size
  while (isize < size) {
    isize <<= 1 //shift the pattern to the left by 1 (i.e. multiplies by 2)
  }
  //number of nodes in a complete  binary tree with isize leaf nodes is (isize*2)-1
  val nodes = Array.fill((isize << 2) - 1)(new LTNode())
  isize -= 1

  def reset() {
    nodes.foreach(_.reset())
  }

  def ect(pos: Int) = nodes(pos).ect

  def ect: Int = ect(0)

  def ectOpt(pos: Int) = nodes(pos).ectOpt

  def ectOpt: Int = ectOpt(0)

  def sumPOpt(pos: Int) = nodes(pos).sumPOpt

  def sumP: Int = sumP(0)
  
  def sumPOpt: Int = sumPOpt(0)

  def responsibleEct(pos: Int) = nodes(pos).responsibleEct

  def responsibleEct: Int = responsibleEct(0)

  def responsibleSumP(pos: Int) = nodes(pos).responsibleSumP

  def sumP(pos: Int) = nodes(pos).sumP

  def isInserted(pos: Int) = nodes(pos + isize).hasActivity

  def father(pos: Int) = (pos - 1) >> 1 // //the father of node in pos is (pos-1)/2

  def left(pos: Int) = (pos << 1) + 1 // the left child of pos is pos*2+1

  def right(pos: Int) = (pos + 1) << 1 //the right child of pos is (pos+1)*2

  def insert(act: ActivityUnary, pos: Int) {
    //the last size nodes are the leaf nodes so the first one is isize (the number of internal nodes)
    val p = isize + pos
    val node = nodes(p)
    node.activity = act
    node.ect = act.ect
    node.sumP = act.minDuration
    node.ectOpt = act.ect
    node.sumPOpt = act.minDuration
    node.responsibleEct = -1
    node.responsibleSumP = -1
    //println("insert into node "+p+":"+node)
    reCompute(father(p))
  }

  def grey(pos: Int) {
    var p = isize + pos
    val node = nodes(p)
    node.ect = Int.MinValue
    node.sumP = 0
    node.responsibleEct = pos
    node.responsibleSumP = pos
    reCompute(father(p))
  }

  def remove(pos: Int) {
    val p = isize + pos
    val node = nodes(p)
    node.reset()
    reCompute(father(p))
  }

  private def reComputeAux(pos: Int) {
    val n = nodes(pos)
    val pr = sumP(right(pos))
    n.sumP = sumP(left(pos)) + pr
    n.ect = ect(right(pos)).max(ect(left(pos)) + pr)

    if (responsibleEct(left(pos)) == -1 && responsibleEct(right(pos)) == -1 && responsibleSumP(left(pos)) == -1 && responsibleSumP(right(pos)) == -1) {
      n.sumPOpt = n.sumP
      n.ectOpt = n.ect
      n.responsibleEct = -1
      n.responsibleSumP = -1
    } else {
      val lo = sumPOpt(left(pos)) + sumP(right(pos))
      val ro = sumP(left(pos)) + sumPOpt(right(pos))
      if (lo > ro) {
        n.sumPOpt = lo
        n.responsibleSumP = responsibleSumP(left(pos))
      } else {
        n.sumPOpt = ro
        n.responsibleSumP = responsibleSumP(right(pos))
      }
      val ect1 = ectOpt(right(pos))
      val ect2 = ect(left(pos)) + sumPOpt(right(pos))
      val ect3 = ectOpt(left(pos)) + sumP(right(pos))
      if (ect1 >= ect2 && ect1 >= ect3) { // ect1 max
        n.ectOpt = ect1
        n.responsibleEct = responsibleEct(right(pos))
      } else if (ect2 >= ect1 && ect2 >= ect3) { // ect2 max
        n.ectOpt = ect2
        n.responsibleEct = responsibleSumP(right(pos))
      } else { // ect3 max
        n.ectOpt = ect3
        n.responsibleEct = responsibleEct(left(pos))
      }
      assert(n.responsibleSumP != -1)
    }
  }
/*
  private def reComputeTop() {
    val n = nodes(0)
    n.ect = ect(2) max (ect(1) + sumP(2))
    n.sumP = sumP(0) + sumP(1)
    
    if (responsibleEct(1) == -1 && responsibleEct(2) == -1) {
      n.sumPOpt = n.sumP
      n.ectOpt = n.ect
      n.responsibleEct = -1
      n.responsibleSumP = -1
    } else {
      val ect1 = ectOpt(2)
      val ect2 = ect(1) + sumPOpt(2)
      val ect3 = ectOpt(1) + sumP(2)
      if (ect1 >= ect2 && ect1 >= ect3) {
        n.ectOpt = ect1
        n.responsibleEct = responsibleEct(2)
      } else if (ect2 >= ect1 && ect2 >= ect3) {
        n.ectOpt = ect2
        n.responsibleEct = responsibleSumP(2)
      } else { // ect3 >= ect1 && ect3 >= ect2
        n.ectOpt = ect3
        n.responsibleEct = responsibleEct(1)
      }
    }
  }
*/
  private def reCompute(pos: Int) {
    var p = pos
    while (p > 0) {
      reComputeAux(p)
      p = father(p)
    }
    reComputeAux(0)
    //reComputeTop()
  }

  class LTNode {

    var activity: ActivityUnary = null
    var sumP: Int = 0 // SUMP
    var ect: Int = Int.MinValue // ECT
    var ectOpt: Int = Int.MinValue // ECT_OPT
    var sumPOpt: Int = 0 // SUM_OPT
    var responsibleEct: Int = -1
    var responsibleSumP: Int = -1

    def hasActivity = activity != null

    def reset() {
      activity = null
      sumP = 0
      ect = Int.MinValue
      ectOpt = Int.MinValue
      sumPOpt = 0
      responsibleEct = -1
      responsibleSumP = -1
    }

    override def toString: String = {
      "---LTNode--------\n" +
        "sump:" + sumP + "\n" +
        "sump_opt:" + sumPOpt + "\n" +
        "resp_sump:" + responsibleSumP + "\n" +
        "ect:" + ect + "\n" +
        "ect_opt:" + ectOpt + "\n" +
        "resp_ect:" + responsibleEct + "\n" +
        (if (hasActivity) ("est:" + activity.est) else "no activity")
    }

  }

}

class ESTComparator2 extends Comparator[ActivityUnary] {
  def compare(act0: ActivityUnary, act1: ActivityUnary): Int = {
    act0.est - act1.est
  }
}
class LSTComparator2 extends Comparator[ActivityUnary] {
  def compare(act0: ActivityUnary, act1: ActivityUnary): Int = {
    act0.lst - act1.lst
  }
}
class LCTComparator2 extends Comparator[ActivityUnary] {
  def compare(act0: ActivityUnary, act1: ActivityUnary): Int = {
    act0.lct - act1.lct
  }
}
class ECTComparator2 extends Comparator[ActivityUnary] {
  def compare(act0: ActivityUnary, act1: ActivityUnary): Int = {
    act0.ect - act1.ect
  }
}
