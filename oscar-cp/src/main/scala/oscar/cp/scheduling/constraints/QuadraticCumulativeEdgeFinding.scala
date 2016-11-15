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
package oscar.cp.scheduling.constraints


/*
import oscar.cp.scheduling.CumulativeActivity
import oscar.cp.scheduling.MirrorCumulativeActivity

class QuadraticCumulativeEdgeFinding(cp: CPStore, allTasks : Array[CumulativeActivity], C : Int, r : Int) extends Constraint(cp, "Quadratic Cumulative Edge-Finding") {

	// The tasks
	var lToRTasks : Array[CumulativeActivity] = allTasks
	var rToLTasks : Array[CumulativeActivity] = lToRTasks.map(new MirrorCumulativeActivity(_))
	
	var nTasks = lToRTasks.size
	
	// New bound
	val LB    = new Array[Int](nTasks) 
	val Dupd  = new Array[Int](nTasks) 
	val SLupd = new Array[Int](nTasks) 
	val E     = new Array[Int](nTasks) 
	
	override def setup(l: CPPropagStrength) : Outcome = {
	
		priorityL2 = 0
		
        val oc = propagate()
        
        if (oc == Outcome.Suspend) {
        	for (i <- 0 until lToRTasks.size) {
        		if (!lToRTasks(i).start.isBound) 
        			lToRTasks(i).start.callPropagateWhenBoundsChange(this)
	        	if (!lToRTasks(i).dur.isBound)
	        		lToRTasks(i).dur.callPropagateWhenBoundsChange(this)
	        	if (!lToRTasks(i).end.isBound) 
	        		lToRTasks(i).end.callPropagateWhenBoundsChange(this)
	        	if (!lToRTasks(i).height.isBound) 
	        		lToRTasks(i).height.callPropagateWhenBoundsChange(this)
	        	if (!lToRTasks(i).resource.isBound)
	        		lToRTasks(i).resource.callPropagateWhenDomainChanges(this)
    		}
        }
        
        return oc   
  	}
	
	override def propagate(): Outcome = {
		
		lToRTasks = allTasks.filter(_.resource.isBoundTo(r))
		rToLTasks = rToLTasks.filter(_.resource.isBoundTo(r))
		
		nTasks = lToRTasks.size

		// Adjusts starting time
		if (nTasks > 0) {
			if (edgeFind(lToRTasks) == Outcome.Failure) {
				return Outcome.Failure
			}
		}
				
		var t = 0
		while (t < nTasks) {
			lToRTasks(t).update()
			t += 1
		}
			
		nTasks = rToLTasks.size
			
		// Adjusts ending time
		if (nTasks > 0) {
			if (edgeFind(rToLTasks) == Outcome.Failure) {
			  return Outcome.Failure
			}
				
		}
			
		return Outcome.Suspend
	}
	
	private def edgeFind(tasks : Array[CumulativeActivity]): Outcome = {
		
		// Init
		var i = 0
		while (i < nTasks) {
			
			LB(i)    = tasks(i).est
			Dupd(i)  = Int.MinValue
			SLupd(i) = Int.MinValue
			E(i)     = Int.MinValue
			
			i += 1
		}
		
		// Non-decreasing sequences
		val lctList = (0 until nTasks).toArray.sortBy(t => tasks(t).lct)
		val estList = (0 until nTasks).toArray.sortBy(t => tasks(t).est)
		
		var u = 0
		while(u < nTasks) {

			val U = lctList(u)
			
			if (tasks(U).resource.isBoundTo(r)) {
			
				var energy    = 0
				var maxEnergy = 0
				var r_rho     = Int.MinValue
				
				var ii = nTasks-1
				while (ii >= 0) {
					
					val i = estList(ii)
					
					if (tasks(i).lct <= tasks(U).lct) {
						
						energy += tasks(i).minEnergy
						
						val density1 = energy.toFloat / (tasks(U).lct - tasks(i).est)
						val density2 = maxEnergy.toFloat / (tasks(U).lct - r_rho)
						
						if (density1 > density2) {
							maxEnergy = energy
							r_rho = tasks(i).est
						}
					} else {
						
						val rest = maxEnergy - (C - tasks(i).minHeight)*(tasks(U).lct - BigInt(r_rho))
						
						if (rest > 0) {
							Dupd(i) = max(Dupd(i), r_rho + (rest.toFloat/tasks(i).minHeight).ceil.toInt)
						}
					}
					
					E(i) = energy
					
					ii -= 1
				}
				
				var minSL = Int.MaxValue
				var r_tau = tasks(U).lct
				
				ii = 0
				while (ii < nTasks) {
					
					val i = estList(ii)
					
					if (C*(tasks(U).lct - tasks(i).est) - E(i) < minSL) {
						r_tau = tasks(i).est
						minSL = C*(tasks(U).lct - r_tau) - E(i)
					}
					
					if (tasks(i).lct > tasks(U).lct) {
						
						val rest = tasks(i).minHeight*(tasks(U).lct - r_tau) - minSL
						
						if (r_tau <= tasks(U).lct && rest > 0)
							SLupd(i) = max(SLupd(i), r_tau + (rest.toFloat/tasks(i).minHeight).ceil.toInt)
							
						if (tasks(i).est + tasks(i).minDuration >= tasks(U).lct || minSL - tasks(i).minEnergy < 0)
							LB(i) = max(LB(i), max(Dupd(i), SLupd(i)))
					}
					
					ii += 1
				}		
			}
			// Next activity
			u += 1
		}
		
		for (i <- 0 until nTasks)
			if (tasks(i).adjustStart(LB(i)) == Outcome.Failure)
				return Outcome.Failure
				
		return Outcome.Suspend
	}

}*/
