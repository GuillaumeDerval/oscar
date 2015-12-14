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
package oscar.cp.constraints;


import oscar.cp.core.CPOutcome;
import oscar.cp.core.CPPropagStrength;
import oscar.cp.core.variables.CPIntVar;
import oscar.cp.core.Constraint;
import oscar.cp.core.CPStore;


/**
 * Global Cardinality Constraint with variable cardinality
 * @author Pierre Schaus pschaus@gmail.com
 */
public class GCCVarAC extends Constraint {


	private final int NONE = -Integer.MIN_VALUE;

	//var
	private CPIntVar [] x;
	private CPIntVar [] o;
	private int minValInit;
	private int minVal;
	private int maxVal;
	private int nbVals;

	// value
	private int []  low;
	private int []  up;
	private int []  flow;

	// flow
	private int   sizeFlow;
	private int[] varMatch;
	private int[] next;
	private int[] prev;
	private int[] valMatch;
	private int[] varSeen;
	private int[] valSeen;
	private int   magic;

	private int dfs;
	private int component;

	private int[] varComponent;
	private int[] varDfs;
	private int[] varHigh;

	private int[] valComponent;
	private int[] valDfs;
	private int[] valHigh;

	int sinkComponent;
	int sinkDfs;
	int sinkHigh;

	private int[] stack;
	private int[] type;
	int top;


    /**
     * Constraint the values minval + i to appear between o[i] times in x
     * @param x
     * @param minval
     * @param o
     * @see  GCC
     * @see  SoftGCCAC
     */
	public GCCVarAC(CPIntVar[] x, int minval,CPIntVar [] o) {
		super(x[0].store(),"GCCVar");
		this.x = x;
		this.o = o;
		this.minValInit = minval;
		this.minVal = minval;
		this.maxVal = minval+o.length-1;
		nbVals = maxVal-minVal+1;
		this.priorityL2_$eq(CPStore.MAXPRIORL2()-3);
	}

	@Override
	public CPOutcome setup(CPPropagStrength l) {

		if (!findValueRange()) {
			return CPOutcome.Failure; //failure update the bounds of variables o
		}
		allocateFlow();
		findInitialFlow();

		if (!findMaximalFlow()) {
			return CPOutcome.Failure;
		}	
		if (!findFeasibleFlow()) {	
			return CPOutcome.Failure;
		}

		allocateSCC();
		prune();
		if (!pruneBounds()) {
			return CPOutcome.Failure;
		}
		for(int k = 0 ; k < x.length; k++) {
			if (!x[k].isBound()) {
				x[k].callPropagateWhenDomainChanges(this);
			}
		}
		for (int i = 0; i < o.length; i++) {
			o[i].callPropagateWhenBoundsChange(this);
		}
		return CPOutcome.Suspend;
	}
	
	@Override
	public CPOutcome propagate() {
	   updateBounds();
	   for(int k = 0; k < x.length; k++) {
	      if (varMatch[k] != NONE) {
	         if (!x[k].hasValue(varMatch[k])) {
	            unassign(k);
	         }
	      }
	   }
	   for(int k = minVal; k <= maxVal; k++)
	      while (flow[k-minVal] > up[k-minVal])
	         unassign(valMatch[k-minVal]);
	   if (!findMaximalFlow()) {
	      return CPOutcome.Failure;
	   }
	   if (!findFeasibleFlow()) {
	      return CPOutcome.Failure;
	   }
	   prune();
	   if (!pruneBounds())
	      return CPOutcome.Failure;
	   return CPOutcome.Suspend;
	}


	private boolean findValueRange() {
		int prev_minval = minVal;

		for(int i = 0; i < x.length; i++) {
			minVal = Math.min(minVal,x[i].getMin());
			maxVal = Math.max(maxVal,x[i].getMax());		
		}
		int d = prev_minval-minVal;

		nbVals = maxVal - minVal + 1;

		// low
		low = new int[nbVals];

		// up
		up = new int[nbVals];
		for(int k = 0; k < nbVals; k++){
			up[k] = x.length;
		}		

		for(int i = 0 ; i < o.length; i++) {
			if (o[i].getMin() > 0){
				low[i+d] = o[i].getMin() ;
			} else {
				if (o[i].updateMin(0) ==  CPOutcome.Failure) {
					return false;
				}
			}
			if (o[i].getMax() < x.length) {
				up[i+d] = o[i].getMax();
			} else {
				if (o[i].updateMax(x.length) ==  CPOutcome.Failure) {
					return false;
				}
			}
		}
		return true;
	}


	private void allocateFlow() {
		// flow
		flow = new int[nbVals];
		// first variable matched
		valMatch = new int [nbVals];
		for(int k = 0; k < nbVals; k++){
			valMatch[k] = NONE;  // unmatched
		}
		// next variable matched
		next = new int[x.length];
		for(int k = 0; k < x.length; k++){
			next[k] = NONE;  // no next
		}
		// previous variable matched
		prev = new int[x.length];
		for(int k = 0; k < x.length; k++){
			prev[k] = NONE;  // no prev
		}
		// variable assignment
		varMatch = new int [x.length];
		for(int k = 0 ; k < x.length; k++){
			varMatch[k] = NONE; // unmatched
		}
		// flag
		varSeen = new int[x.length];
		// flag
		valSeen = new int[nbVals];
		magic = 0;	
	}

	//assigns value v to variable k and update structures: sizeFlow, flow, varMatch, prev, next, valMatch
	private void assign(int k,int v){
		sizeFlow++;
		unassign(k);
		// k is now first on the list of v
		varMatch[k] = v;
		flow[v-minVal]++;
		int nk = valMatch[v-minVal];
		next[k] = nk;
		prev[k] = NONE;
		if (nk != NONE)
			prev[nk] = k;
		valMatch[v-minVal] = k;
	}

	//unassings variable k and updates appropriately the structures: sizeFlow, flow, varMatch, prev, next, valMatch
	private void unassign(int k){
		if (varMatch[k] != NONE) { // this guy is assigned; must be removed
			sizeFlow--;
			int w = varMatch[k];
			flow[w-minVal]--;
			if (valMatch[w-minVal] == k) { // first in the list
				int nk = next[k];
				valMatch[w-minVal] = nk;
				if (nk != NONE)
					prev[nk] = NONE; // nk is now first
			}
			else { // not first
				int pk = prev[k];
				int nk = next[k];
				next[pk] = nk;
				if (nk != NONE)
					prev[nk] = pk;
			}
			varMatch[k] = NONE;
		}
	}

	//finds a initial flow for both the underflow and overflow
	private void findInitialFlow() {
		sizeFlow = 0;
		for(int k = 0; k < x.length; k++) {
			int mx = x[k].getMin();
			int Mx = x[k].getMax();
			for(int i = mx; i <= Mx; i++){
				if (flow[i-minVal] < up[i-minVal])
					if (x[k].hasValue(i)) {
						assign(k,i);
						break;
					}
			}
		}
	}

	private boolean findMaximalFlow() {
		if (sizeFlow < x.length) {
			for(int k = 0; k < x.length; k++) {
				if (varMatch[k] == NONE) {
					magic++;
					if (!findAugmentingPath(k))
						return false;
				}
			}
		}
		return true;
	}

	private boolean findAugmentingPath(int k) {
		if (varSeen[k] != magic) {
			varSeen[k] =  magic;
			int mx = x[k].getMin();
			int Mx = x[k].getMax();
			for(int v = mx; v <= Mx; v++) {
				if (varMatch[k] != v) {
					if (x[k].hasValue(v)) {
						if (findAugmentingPathValue(v)) {
							assign(k,v);
							return true;
						}
					}
				}
			}
		}
		return false;
	}

	private boolean findAugmentingPathValue(int v) {
		int vind = v-minVal;
		if (valSeen[vind] != magic) {
			valSeen[vind] = magic;
			if (flow[vind] < up[vind])
				return true;
			else if (flow[vind] > 0) {
				int i = valMatch[vind];
				while (i != NONE) {
					if (findAugmentingPath(i))
						return true;
					i = next[i];
				}
			}
		}
		return false;
	}

	private boolean findFeasibleFlow() {
		for(int v = minVal; v <= maxVal; v++) {
			while (flow[v-minVal] < low[v-minVal])
				if (!findFeasibleFlowTo(v))
					return false;
		}
		return true;
	}

	private boolean findFeasibleFlowTo(int q) { //q is a value
		magic++;
		for(int v = minVal; v <= maxVal; v++) {
			if (flow[v-minVal] > low[v-minVal])
				if (findFeasibleFlowValue(v,q))
					return true;
		}
		return false;
	}

	private boolean findFeasibleFlowValue(int v,int q) { //try to transfer some flow from v to q
		int vind = v-minVal;
		if (valSeen[vind] != magic) {
			valSeen[vind] = magic;
			int i = valMatch[vind];
			while (i != NONE) {
				if (varMatch[i] != q && x[i].hasValue(q)) {
					assign(i,q);
					return true;
				}
				i = next[i];
			}
			i = valMatch[vind];
			while (i != NONE) {
				if (findFeasibleFlowVar(i,q))
					return true;
				i = next[i];
			}
		}
		return false;
	}

	private boolean findFeasibleFlowVar(int k,int q) { //k is a var index, q is a value
		if (varSeen[k] != magic) {
			varSeen[k] = magic;
			int mx = x[k].getMin();
			int Mx = x[k].getMax();
			for(int v = mx; v <= Mx; v++) {
				if (q != v && varMatch[k] != v) {
					if (x[k].hasValue(v)) {
						if (findFeasibleFlowValue(v,q)) {
							assign(k,v);
							return true;
						}
					}
				}
			}
		}
		return false;
	}

	private void allocateSCC() {
		varComponent = new int[x.length];
		varDfs = new int[x.length];
		varHigh = new int [x.length];

		valComponent = new int [nbVals];
		valDfs = new int[nbVals];
		valHigh = new int[nbVals];

		stack = new int [x.length+nbVals+2];
		type  = new int [x.length+nbVals+2];
	}

	private void prune() {
		findSCC();
		for(int k = 0; k < x.length; k++) {
			int mx = x[k].getMin();
			int Mx = x[k].getMax();
			for(int w = mx; w <= Mx; w++) {
				if (varMatch[k] != w) {
					if (varComponent[k] != valComponent[w-minVal]) {
						if (x[k].hasValue(w)) {
							CPOutcome ok = x[k].removeValue(w);
							assert(ok != CPOutcome.Failure);
						}
					}
				}
			}
		}
	}

	private void initSCC() {
		for(int k = 0 ; k < x.length; k++) {
			varComponent[k] = 0;
			varDfs[k] = 0;
			varHigh[k] = 0;
		}
		for(int k = minVal; k <= maxVal; k++) {
			valComponent[k-minVal] = 0;
			valDfs[k-minVal] = 0;
			valHigh[k-minVal] = 0;
		}
		sinkComponent = 0;
		sinkDfs = 0;
		sinkHigh = 0;

		top = 0;
		dfs = x.length + (maxVal-minVal+1) + 1;
		component = 0;
	}

	private void findSCC() {
		initSCC();
		for(int k = 0; k < x.length; k++) {
			if (varDfs[k] == 0) {
				findSCCvar(k);
			}
		}
	}

	private void findSCCvar(int k) {
		varDfs[k] = dfs--;
		varHigh[k] = varDfs[k];
		stack[top] = k;
		type[top] = 0;
		top++;
		assert(top <= x.length + maxVal-minVal + 2);
		int mx = x[k].getMin();
		int Mx = x[k].getMax();
		for(int w = mx; w <= Mx; w++) {
			int wind = w-minVal;
			if (varMatch[k] != w) {
				if (x[k].hasValue(w)) {
					if (valDfs[wind] == 0) {
						findSCCval(w);
						if (valHigh[wind] > varHigh[k])
							varHigh[k] = valHigh[wind];
					}
					else if ( (valDfs[wind] > varDfs[k]) && (valComponent[wind] == 0)) {
						if (valDfs[wind] > varHigh[k])
							varHigh[k] = valDfs[wind];
					}
				}
			}
		}
		if (varHigh[k] == varDfs[k]) {
			component++;
			do {
				assert(top > 0);
				int v = stack[--top];
				int t = type[top];
				if (t == 0)
					varComponent[v] = component;
				else if (t == 1)
					valComponent[v-minVal] = component;
				else
					sinkComponent = component;
				if (t == 0 && v == k)
					break;
			} while (true);
		}
	}

	private void findSCCval(int v) {
		int vind = v-minVal;
		valDfs[vind] = dfs--;
		valHigh[vind] = valDfs[vind];
		stack[top] = v;
		type[top] = 1;
		top++;
		assert(top <= x.length + maxVal-minVal + 2);
		// first go to the variables assigned to this value

		int k = valMatch[vind];
		while (k != NONE) {
			if (varDfs[k] == 0) {
				findSCCvar(k);
				if (varHigh[k] > valHigh[vind])
					valHigh[vind] = varHigh[k];
			}
			else if ( (varDfs[k] > valDfs[vind]) && (varComponent[k] == 0)) {
				if (varDfs[k] > valHigh[vind])
					valHigh[vind] = varDfs[k];
			}
			k = next[k];
		}

		// next try to see if you can go to the sink

		if (flow[vind] < up[vind]) {
			// go to the sink
			if (sinkDfs == 0) {
				findSCCsink();
				if (sinkHigh > valHigh[vind])
					valHigh[vind] = sinkHigh;
			}
			else if ((sinkDfs > valDfs[vind]) && (sinkComponent == 0) && (sinkDfs > valHigh[vind])) {
				valHigh[vind] = sinkDfs;
			}
		}

		if (valHigh[vind] == valDfs[vind]) {
			component++;
			do {
				assert(top > 0);
				int i = stack[--top];
				int t = type[top];
				if (t == 0)
					varComponent[i] = component;
				else if (t == 1)
					valComponent[i-minVal] = component;
				else
					sinkComponent = component;
				if (t == 1 && i == v)
					break;
			} while (true);
		}
	}

	private void findSCCsink() {
		sinkDfs  = dfs--;
		sinkHigh = sinkDfs;
		stack[top] = NONE;
		type[top] = 2;
		top++;
		assert(top <= x.length + maxVal-minVal + 2);
		for(int i = 0; i < x.length; i++) {
			int w = varMatch[i];
			int wind = w-minVal;
			if (flow[wind] > low[wind]) {
				if (valDfs[wind] == 0) {
					findSCCval(w);
					if (valHigh[wind] > sinkHigh)
						sinkHigh = valHigh[wind];
				}
				else if ((valDfs[wind] > sinkDfs) && (valComponent[wind] == 0) && (valDfs[wind] > sinkHigh)) {
					sinkHigh = valDfs[wind];
				}
			}
		}

		if (sinkHigh == sinkDfs) {
			component++;
			do {
				assert(top > 0);
				int i = stack[--top];
				int t = type[top];
				if (t == 0)
					varComponent[i] = component;
				else if (t == 1)
					valComponent[i-minVal] = component;
				else
					sinkComponent = component;
				if (t == 2)
					break;
			} while (true);
		}
	}
	
	
	private boolean decreaseMax(int w) {
		int wind = w-minVal;
		while (flow[wind] > up[wind])
			unassign(valMatch[wind]);
		if (!findMaximalFlow())
			return false;
		if (!findFeasibleFlow())
			return false;
		return true;
	}

	private boolean increaseMin(int w) {
		int wind = w-minVal;
		while (flow[wind] < low[wind])
			if (!findFeasibleFlowTo(w))
				return false;
		return true;
	}
	
	private boolean pruneBounds() {
	   for(int i = 0 ; i < o.length; i++) {
	         int m = o[i].getMin();
	         int M = o[i].getMax();
	         if (m != M) {
	            // update the lower bounds
	            up[i+minValInit-minVal] = m;
	            while (!decreaseMax(i+minValInit)) { //not feasible with this value
	               up[i+minValInit-minVal]++;
	            }
	            if (o[i].updateMin(up[i+minValInit-minVal]) == CPOutcome.Failure) {
	               return false;
	            }
	            up[i+minValInit-minVal] = M;
	         }
	   }
	   
	   for(int i = 0 ; i < o.length; i++) {
		   int m = o[i].getMin();
		   int M = o[i].getMax();
		   if (m != M) {
			   // update the upper bounds
			   low[i+minValInit-minVal] = M;
			   while (!increaseMin(i+minValInit)) { //not feasible with this value
				   low[i+minValInit-minVal]--;
			   }
			   if (o[i].updateMax(low[i+minValInit-minVal]) == CPOutcome.Failure) {
				   return false;
			   } 
			   low[i+minValInit-minVal] = m;
		   }
	   }
	   return true;
	}

	private void updateBounds() {
		for(int i = 0 ; i < o.length; i++) {
			int v = o[i].getMin();
			if (v > 0)
				low[i+minValInit-minVal] = v;
			else
				low[i+minValInit-minVal] = 0;
			v = o[i].getMax();
			if (v < x.length)
				up[i+minValInit-minVal] = v;
			else
				up[i+minValInit-minVal] = x.length;
		}
	}

}//end of class GCCVar
