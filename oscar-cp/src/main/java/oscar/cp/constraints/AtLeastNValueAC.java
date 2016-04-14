/*******************************************************************************
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 * <p>
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 * <p>
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
 * @author Pierre Schaus pschaus@gmail.com
 */
public class AtLeastNValueAC extends Constraint {

    private static final int NONE = -Integer.MIN_VALUE;

    private boolean posted;

    private CPIntVar[] x;

    private CPIntVar nValueVar;

    private int[] match;
    private int[] varSeen;

    private int min;
    private int max;
    private int valSize;
    private int[] valMatch;
    private int sizeMatching;
    private int[] valSeen;
    private int magic;

    private int dfs;
    private int component;

    private int[] varComponent;
    private int[] varDfs;
    private int[] varHigh;

    private int[] valComponent;
    private int[] valDfs;
    private int[] valHigh;

    private int[] stack;
    private int[] type;
    private int top;

    private int[][] domArray;


    public AtLeastNValueAC(CPIntVar[] x, CPIntVar nval) {
        super(x[0].store(), "AtLeastNValueAC");
        this.x = x;
        this.posted = false;
        this.nValueVar = nval;
        this.priorityL2_$eq(CPStore.MAXPRIORL2() - 3);
        this.idempotent_$eq(true);
    }

    public AtLeastNValueAC(CPIntVar[] x, CPIntVar nval, boolean dontPostFWC) {
        this(x, nval);
    }


    @Override
    public CPOutcome setup(CPPropagStrength l) {

        posted = true;

        if (nValueVar.getMin() < x.length) {
            if (s().post(new AtLeastNValueFWC(x, nValueVar)) == CPOutcome.Failure) {
                return CPOutcome.Failure;
            }
        } else { //allDifferent (AtLeastNValueAC is also used to implement the AllDiffAC)
            if (s().post(new AllDiffFWC(x)) == CPOutcome.Failure) {
                return CPOutcome.Failure;
            }
        }

        findValueRange();

        domArray = new int[x.length][valSize];

        initMatching();

        findInitialMatching();

        int sizeMatching = findMaximalMatching();

        if (nValueVar.updateMax(sizeMatching) == CPOutcome.Failure) {
            return CPOutcome.Failure;
        }

        if (nValueVar.getMin() > sizeMatching) {
            return CPOutcome.Failure;
        }

        allocateSCC();

        prune(sizeMatching);

        for (int k = 0; k < x.length; k++) {
            if (!x[k].isBound()) {
                x[k].callPropagateWhenDomainChanges(this);
            }
        }

        if (!nValueVar.isBound()) {
            nValueVar.callPropagateWhenBoundsChange(this);
        }

        return CPOutcome.Suspend;
    }

    public boolean hasValInBestAssignment(int i) {
        if (posted && i >= 0 && i < x.length && match[i] != NONE) return true;
        return false;
    }

    public int getValInBestAssignment(int i) {
        if (hasValInBestAssignment(i))
            return match[i];
        else if (i >= 0 && i < x.length && posted)
            return x[i].getMin();
        return Integer.MIN_VALUE;
    }

    @Override
    public CPOutcome propagate() {
        for (int k = 0; k < x.length; k++) {
            if (match[k] != NONE) {
                if (!x[k].hasValue(match[k])) {
                    valMatch[match[k] - min] = -1;
                    match[k] = NONE;
                    sizeMatching--;
                }
            }
        }

        int maxMatching = findMaximalMatching();

        if (nValueVar.updateMax(maxMatching) == CPOutcome.Failure) {
            return CPOutcome.Failure;
        }
        if (nValueVar.getMin() > maxMatching) {
            //System.out.println("fail detected");
            return CPOutcome.Failure;
        }
        //lower bound of nValueVar is pruned by CotCPAtLeastNValueIFW
        prune(maxMatching);
        return CPOutcome.Suspend;
    }

    private void findValueRange() {
        min = Integer.MAX_VALUE;
        max = Integer.MIN_VALUE;
        for (int i = 0; i < x.length; i++) {
            min = Math.min(min, x[i].getMin());
            max = Math.max(max, x[i].getMax());
        }
        valSize = max - min + 1;
        valMatch = new int[valSize];
        for (int k = 0; k < valSize; k++)
            valMatch[k] = -1;  // unmatched
    }

    private void initMatching() {
        magic = 0;
        match = new int[x.length];
        for (int k = 0; k < x.length; k++) {
            match[k] = NONE; // unmatched
        }
        varSeen = new int[x.length];
        valSeen = new int[valSize];
    }

    private void findInitialMatching() { //returns the size of the maximum matching
        sizeMatching = 0;
        for (int k = 0; k < x.length; k++) {
            int mx = x[k].getMin();
            int Mx = x[k].getMax();
            for (int i = mx; i <= Mx; i++)
                if (valMatch[i - min] < 0) // unmatched
                    if (x[k].hasValue(i)) {
                        match[k] = i;
                        valMatch[i - min] = k;
                        sizeMatching++;
                        break;
                    }
        }
    }

    private int findMaximalMatching() {
        if (sizeMatching < x.length) {
            for (int k = 0; k < x.length; k++) {
                if (match[k] == NONE) {
                    magic++;
                    if (findAlternatingPath(k)) {
                        sizeMatching++;
                    }
                }
            }
        }
        return sizeMatching;
    }

    private boolean findAlternatingPath(int i) {
        if (varSeen[i] != magic) {
            varSeen[i] = magic;
            int mx = x[i].getMin();
            int Mx = x[i].getMax();
            for (int v = mx; v <= Mx; v++) {
                if (match[i] != v) {
                    if (x[i].hasValue(v)) {
                        if (findAlternatingPathValue(v)) {
                            match[i] = v;
                            valMatch[v - min] = i;
                            return true;
                        }
                    }
                }
            }
        }
        return false;
    }

    private boolean findAlternatingPathValue(int v) {
        if (valSeen[v - min] != magic) {
            valSeen[v - min] = magic;
            if (valMatch[v - min] == -1)
                return true;
            if (findAlternatingPath(valMatch[v - min]))
                return true;
        }
        return false;
    }

    private void allocateSCC() {
        varComponent = new int[x.length * 2];
        varDfs = new int[x.length * 2];
        varHigh = new int[x.length * 2];

        valComponent = new int[valSize];
        valDfs = new int[valSize * 2];
        valHigh = new int[valSize * 2];

        stack = new int[(x.length + valSize) * 2];
        type = new int[(x.length + valSize) * 2];
    }

    private void initSCC() {
        for (int k = 0; k < x.length; k++) {
            varComponent[k] = 0;
            varDfs[k] = 0;
            varHigh[k] = 0;
        }
        for (int v = min; v <= max; v++) {
            valComponent[v - min] = 0;
            valDfs[v - min] = 0;
            valHigh[v - min] = 0;
        }
        top = 0;
        dfs = x.length + valSize;
        component = 0;
    }

    private void findSCC() {
        initSCC();
        for (int k = 0; k < x.length; k++) {
            if (varDfs[k] == 0 &&  !x[k].isBound())
                findSCCvar(k);
        }
    }

    private void findSCCvar(int k) {
        varDfs[k] = dfs--;
        varHigh[k] = varDfs[k];
        stack[top] = k;
        type[top] = 0;
        top++;
        assert (top <= x.length + valSize);
        //a variable can go to values in its domain that it doesn't match


        int nVal = x[k].fillArray(domArray[k]);
        for (int i = 0; i < nVal; i++) {
            int w = domArray[k][i];
            if (match[k] != w) {
                if (valDfs[w - min] == 0) {
                    findSCCval(w);
                    if (valHigh[w - min] > varHigh[k])
                        varHigh[k] = valHigh[w - min];
                } else if ((valDfs[w - min] > varDfs[k]) && (valComponent[w - min] == 0)) {
                    if (valDfs[w - min] > varHigh[k])
                        varHigh[k] = valDfs[w - min];
                }
            }

        }

        //matched variable can go to other unmatched variables
        if (match[k] != NONE) {
            for (int i = 0; i < x.length; i++) {
                if (match[i] == NONE && !x[i].isBound()) {
                    if (varDfs[i] == 0) {
                        findSCCvar(i);
                        if (varHigh[i] > varHigh[k])
                            varHigh[k] = varHigh[i];
                    } else if ((varDfs[i] > varDfs[k]) && (varComponent[i] == 0)) {
                        if (varDfs[i] > varHigh[k])
                            varHigh[k] = varDfs[i];
                    }
                }
            }
        }

        if (varHigh[k] == varDfs[k]) {
            component++;
            do {
                assert (top > 0);
                int v = stack[--top];
                int t = type[top];
                if (t == 0)
                    varComponent[v] = component;
                else
                    valComponent[v - min] = component;
                if (t == 0 && v == k)
                    break;
            } while (true);
        }
    }

    private void findSCCval(int k) {
        int i;
        valDfs[k - min] = dfs--;
        valHigh[k - min] = valDfs[k - min];
        stack[top] = k;
        type[top] = 1;
        top++;
        assert (top <= x.length + valSize);
        //machted value can go to the variable that match it
        if (valMatch[k - min] != -1) {
            int w = valMatch[k - min];
            if (varDfs[w] == 0) {
                findSCCvar(w);
                if (varHigh[w] > valHigh[k - min])
                    valHigh[k - min] = varHigh[w];
            } else if ((varDfs[w] > valDfs[k - min]) && (varComponent[w] == 0)) {
                if (varDfs[w] > valHigh[k - min])
                    valHigh[k - min] = varDfs[w];
            }
        } else {
            for (i = 0; i < x.length; i++) {
                //unmatched value can go to every matched value
                if (match[i] != NONE) {
                    int w = match[i];
                    if (valDfs[w - min] == 0) {
                        findSCCval(w);
                        if (valHigh[w - min] > valHigh[k - min])
                            valHigh[k - min] = valHigh[w - min];
                    } else if ((valDfs[w - min] > valDfs[k - min]) && (valComponent[w - min] == 0)) {
                        if (valDfs[w - min] > valHigh[k - min])
                            valHigh[k - min] = valDfs[w - min];
                    }
                }
                //unmatched value can go to every unmatched variable
                else {
                    if (varDfs[i] == 0) {
                        findSCCvar(i);
                        if (varHigh[i] > valHigh[k - min])
                            valHigh[k - min] = varHigh[i];
                    } else if ((varDfs[i] > valDfs[k - min]) && (varComponent[i] == 0)) {
                        if (varDfs[i] > valHigh[k - min])
                            valHigh[k - min] = varDfs[i];
                    }
                }
            }
        }
        if (valHigh[k - min] == valDfs[k - min]) {
            component++;
            do {
                assert (top > 0);
                int v = stack[--top];
                int t = type[top];
                if (t == 0)
                    varComponent[v] = component;
                else
                    valComponent[v - min] = component;
                if (t == 1 && v == k)
                    break;
            } while (true);
        }
    }

    private void prune(int sizeMatching) {
        if (sizeMatching > nValueVar.getMin()) {
            return; //no pruning possible
        }
        findSCC();
        for (int k = 0; k < x.length; k++) {
            if (!x[k].isBound()) {
                int nVal = x[k].fillArray(domArray[k]);
                for (int i = 0; i < nVal; i++) {
                    int w = domArray[k][i];
                    if (match[k] != w && varComponent[k] != valComponent[w - min]) {
                        x[k].removeValue(w);
                    }
                }
            }

        }
        //System.out.println("here");
    }

}
