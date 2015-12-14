package oscar.cp.constraints;

import java.util.Arrays;
import java.util.Comparator;

import oscar.algo.reversible.ReversibleInt;
import oscar.cp.core.CPOutcome;
import oscar.cp.core.CPPropagStrength;
import oscar.cp.core.Constraint;
import oscar.cp.core.variables.CPBoolVar;
import oscar.cp.core.variables.CPIntVar;

public class BinaryKnapsackWithCardinality extends Constraint {

	CPBoolVar [] x;
	int [] w;
	CPIntVar c;
    int n;

    ReversibleInt packed;
    ReversibleInt nPacked;

	public BinaryKnapsackWithCardinality(CPBoolVar [] b, final int [] weights, CPIntVar load, int nbItems) {
		super(b[0].store(),"BinaryKnapsackWithCardinality");

		Integer [] perm = new Integer [weights.length];
		for (int i = 0; i < perm.length; i++) {
			if (weights[i] < 0) {
				throw new RuntimeException("weights must be non negative");
			}
			perm[i] = i;
		}

		Arrays.sort(perm, new Comparator<Integer>(){
			public int compare(Integer o1, Integer o2) {
				return weights[o2]-weights[o1];
			}
		});

		w = new int[weights.length];
		x = new CPBoolVar[weights.length];
		c = load;
        n = nbItems;
		for (int i = 0; i < x.length; i++) {
			w[i] = weights[perm[i]];
			x[i] = b[perm[i]];
		}
	}

	@Override
	public CPOutcome setup(CPPropagStrength l) {

        packed = new ReversibleInt(s(),0);
        nPacked = new ReversibleInt(s(),0);
        for (int i = 0; i < x.length; i++) {
            if (x[i].isBound()) {
                packed.setValue(packed.getValue() + w[i]);
                nPacked.incr();
            } else {
                x[i].callValBindIdxWhenBind(this,i);
                x[i].callPropagateWhenBind(this);
            }

        }

		return CPOutcome.Suspend;
	}

	@Override
	public CPOutcome valBindIdx(CPIntVar var, int idx) {
        if (var.getMin() == 1) {
            nPacked.incr();
            packed.setValue(packed.getValue() + w[idx]);
        }

         return CPOutcome.Suspend;
	}


	@Override
	public CPOutcome propagate() {

        int curn = nPacked.getValue();
        int curw = packed.getValue();
        for (int i = 0; i < x.length && curn < n; i++) {
            if (!x[i].isBound()) {
                curw += w[i];
                curn++;
            }
        }
        if (c.updateMax(curw) == CPOutcome.Failure) {
            return CPOutcome.Failure;
        }

        curn = nPacked.getValue();
        curw = packed.getValue();
        for (int i = x.length-1; i >=0  && curn < n; i--) {
            if (!x[i].isBound()) {
                curw += w[i];
                curn++;
            }
        }

        if (c.updateMin(curw) == CPOutcome.Failure) {
            return CPOutcome.Failure;
        }

		return CPOutcome.Suspend;
	}
}