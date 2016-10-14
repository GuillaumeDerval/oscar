package oscar.xcsp3;

import org.xcsp.common.XEnums;
import org.xcsp.common.predicates.XNodeExpr;
import org.xcsp.common.predicates.XNodeLeaf;
import org.xcsp.common.predicates.XNodeParent;
import org.xcsp.parser.XCallbacks2;
import org.xcsp.parser.XDomains;
import org.xcsp.parser.XParser;
import org.xcsp.parser.XVariables;

import java.util.*;
import java.util.stream.IntStream;

/**
 * An extension of the default XCallback parser that provides default decompositions for the most complex constraints
 */
public abstract class XCallbacksDecomp implements XCallbacks2 {
    protected boolean CONVERT_SYMBOLIC = true;
    protected HashMap<String, Integer> symbolicValues;
    protected ArrayList<String> symbolicValuesReversed;
    protected HashMap<String, XVariables.XVarInteger> symbolicVariables;

    protected String reservedArray = "oscarReserved";
    protected int currentReservedArrayIdx;
    public XCallbacksDecomp() {
        symbolicValues = new HashMap<>();
        symbolicValuesReversed = new ArrayList<>();
        symbolicVariables = new HashMap<>();
        currentReservedArrayIdx = 0;
    }

    /**
     * Generate a variable id that is unique and will not be sent in the solution
     */
    protected String generateReservedArrayIdx() {
        currentReservedArrayIdx++;
        return reservedArray + "[" + Integer.toString(currentReservedArrayIdx) + "]";
    }
    
    /**
     * Builds a new symbolic variable.
     *
     * This particular version automatically maps to buildVarInteger if CONVERT_SYMBOLIC is true
     */
    @Override
    public void buildVarSymbolic(XVariables.XVarSymbolic x, String[] values) {
        if(CONVERT_SYMBOLIC) {
            // map all symbolic values to integer values
            for(String v: values) {
                if(!symbolicValues.containsKey(v)) {
                    symbolicValuesReversed.add(v);
                    symbolicValues.put(v, symbolicValues.size());
                }
            }

            // compute the effective domain
            int[] domain = Arrays.stream(values).mapToInt(v -> symbolicValues.get(v)).toArray();

            // create a std XVarInteger
            XVariables.XVarInteger var = (XVariables.XVarInteger)XVariables.XVar.build(x.id, XVariables.TypeVar.integer, new XDomains.XDomInteger(domain));
            buildVarInteger(var, domain);

            // store it for future reference
            symbolicVariables.put(x.id, var);
        }
        else {
            unimplementedCase(x.id);
        }
    }

    /**
     * All Different constraint on Symbolic variables.
     *
     * Maps to buildCtrAllDifferent with Integer variables if CONVERT_SYMBOLIC is true
     */
    @Override
    public void buildCtrAllDifferent(String id, XVariables.XVarSymbolic[] lists) {
        if(CONVERT_SYMBOLIC)
            buildCtrAllDifferent(id, Arrays.stream(lists).map(x -> symbolicVariables.get(x.id)).toArray(XVariables.XVarInteger[]::new));
        else
            unimplementedCase(id);
    }

    /**
     * Extension constraint for Symbolic Variables
     *
     * Maps to buildCtrExtension with Integer variables if CONVERT_SYMBOLIC is true
     */
    @Override
    public void buildCtrExtension(String id, XVariables.XVarSymbolic x, String[] values, boolean positive, Set<XEnums.TypeFlag> flags) {
        if(CONVERT_SYMBOLIC)
            buildCtrExtension(id, symbolicVariables.get(x.id), Arrays.stream(values).mapToInt(i -> symbolicValues.get(i)).toArray(), positive, flags);
        else
            unimplementedCase(id);
    }

    protected XNodeLeaf<XVariables.XVar> convertSymbolicTreeLeaf(XNodeLeaf<XVariables.XVar> leaf) {
        if(leaf.type == XEnums.TypeExpr.VAR && leaf.value instanceof XVariables.XVarSymbolic)
            return new XNodeLeaf<>(XEnums.TypeExpr.VAR, symbolicVariables.get(((XVariables.XVarSymbolic)leaf.value).id));
        if(leaf.type == XEnums.TypeExpr.SYMBOL)
            return new XNodeLeaf<>(XEnums.TypeExpr.LONG, (long)symbolicValues.get((String)leaf.value));
        return leaf;
    }

    @SuppressWarnings("unchecked")
    protected XNodeParent<XVariables.XVar> convertSymbolicTree(XNodeParent<XVariables.XVar> syntaxTreeRoot) {

        return new XNodeParent<>(syntaxTreeRoot.type, Arrays.stream(syntaxTreeRoot.sons).map(x -> {
            if(x instanceof XNodeParent)
                return convertSymbolicTree((XNodeParent<XVariables.XVar>)x);
            else
                return convertSymbolicTreeLeaf((XNodeLeaf<XVariables.XVar>)x);
        }).toArray(XNodeExpr[]::new));
    }

    /**
     * Intension constraint for Symbolic Variables
     *
     * Maps to buildCtrIntension with Integer variables if CONVERT_SYMBOLIC is true
     */
    @Override
    public void buildCtrIntension(String id, XVariables.XVarSymbolic[] scope, XNodeParent<XVariables.XVar> syntaxTreeRoot) {
        if(CONVERT_SYMBOLIC)
            buildCtrIntension(id, Arrays.stream(scope).map(x -> symbolicVariables.get(x.id)).toArray(XVariables.XVarInteger[]::new), convertSymbolicTree(syntaxTreeRoot));
        else
            unimplementedCase(id);
    }

    /**
     * Extension constraint for Symbolic Variables
     *
     * Maps to buildCtrExtension with Integer variables if CONVERT_SYMBOLIC is true
     */
    @Override
    public void buildCtrExtension(String id, XVariables.XVarSymbolic[] list, String[][] tuples, boolean positive, Set<XEnums.TypeFlag> flags) {
        buildCtrExtension(id,
                Arrays.stream(list).map(x -> symbolicVariables.get(x.id)).toArray(XVariables.XVarInteger[]::new),
                Arrays.stream(tuples).map(x -> Arrays.stream(x).mapToInt(y -> symbolicValues.get(y)).toArray()).toArray(int[][]::new),
                positive, flags);
    }

    /**
     * All Diff Matrix.
     * This particular version maps all the line/columns to buildCtrAllDifferent.
     * @param id
     * @param matrix
     */
    @Override
    public void buildCtrAllDifferentMatrix(String id, XVariables.XVarInteger[][] matrix) {
        for(XVariables.XVarInteger[] line: matrix)
            this.buildCtrAllDifferent(id, line);

        IntStream.range(0, matrix[0].length).forEach(i -> buildCtrAllDifferent(id, Arrays.stream(matrix).map(l -> l[i]).toArray(XVariables.XVarInteger[]::new)));
    }

    /**
     * Element constraint from XCSP3. Ensures value is present in list.
     *
     * Map to buildCtrElement(list, value, index) with index being a new variable
     *
     * @param id
     * @param list
     * @param value
     */
    @Override
    public void buildCtrElement(String id, XVariables.XVarInteger[] list, XVariables.XVarInteger value) {
        XVariables.XVarInteger v = (XVariables.XVarInteger)XVariables.XVar.build(generateReservedArrayIdx(), XVariables.TypeVar.integer, new XDomains.XDomInteger(0, list.length-1));
        buildVarInteger(v, 0, list.length-1);
        buildCtrElement(id, list, 0, v, XEnums.TypeRank.ANY, value);
    }

    /**
     * Element constraint from XCSP3. Ensures value is present in list.
     *
     * Map to buildCtrElement(list, value, index) with index being a new variable
     *
     * @param id
     * @param list
     * @param value
     */
    @Override
    public void buildCtrElement(String id, XVariables.XVarInteger[] list, int value) {
        XVariables.XVarInteger v = (XVariables.XVarInteger)XVariables.XVar.build(generateReservedArrayIdx(), XVariables.TypeVar.integer, new XDomains.XDomInteger(0, list.length-1));
        buildVarInteger(v, 0, list.length-1);
        buildCtrElement(id, list, 0, v, XEnums.TypeRank.ANY, value);
    }

    /**
     * Compute possible ends for given job starts and lenghts
     */
    protected XVariables.XVarInteger[] buildEndsFromStartAndLength(XVariables.XVarInteger[] starts, XVariables.XVarInteger[] lengths) {
        XVariables.XVarInteger[] output = new XVariables.XVarInteger[starts.length];
        for(int i = 0; i < starts.length; i++) {
            int min = (int)(((XDomains.XDomInteger)starts[i].dom).getFirstValue() + ((XDomains.XDomInteger)lengths[i].dom).getFirstValue());
            int max = (int)(((XDomains.XDomInteger)starts[i].dom).getLastValue() + ((XDomains.XDomInteger)lengths[i].dom).getLastValue());
            output[i] = (XVariables.XVarInteger)XVariables.XVar.build(generateReservedArrayIdx(), XVariables.TypeVar.integer, new XDomains.XDomInteger(min, max));
            buildVarInteger(output[i], min, max);
        }
        return output;
    }

    /**
     * Compute possible ends for given job starts and lenghts
     */
    protected XVariables.XVarInteger[] buildEndsFromStartAndLength(XVariables.XVarInteger[] starts, int[] lengths) {
        XVariables.XVarInteger[] output = new XVariables.XVarInteger[starts.length];
        for(int i = 0; i < starts.length; i++) {
            int min = (int)(((XDomains.XDomInteger)starts[i].dom).getFirstValue())+lengths[i];
            int max = (int)(((XDomains.XDomInteger)starts[i].dom).getLastValue())+lengths[i];
            output[i] = (XVariables.XVarInteger)XVariables.XVar.build(generateReservedArrayIdx(), XVariables.TypeVar.integer, new XDomains.XDomInteger(min, max));
            buildVarInteger(output[i], min, max);
        }
        return output;
    }

    /**
     * Cumulative constraint. Infer the parameters ends from origins and length
     */
    @Override
    public void buildCtrCumulative(String id, XVariables.XVarInteger[] origins, int[] lengths, int[] heights, XParser.Condition condition) {
        buildCtrCumulative(id, origins, lengths, buildEndsFromStartAndLength(origins, lengths), heights, condition);
    }

    /**
     * Cumulative constraint. Infer the parameters ends from origins and length
     */
    @Override
    public void buildCtrCumulative(String id, XVariables.XVarInteger[] origins, int[] lengths, XVariables.XVarInteger[] heights, XParser.Condition condition) {
        buildCtrCumulative(id, origins, lengths, buildEndsFromStartAndLength(origins, lengths), heights, condition);
    }

    /**
     * Cumulative constraint. Infer the parameters ends from origins and length
     */
    @Override
    public void buildCtrCumulative(String id, XVariables.XVarInteger[] origins, XVariables.XVarInteger[] lengths, int[] heights, XParser.Condition condition) {
        buildCtrCumulative(id, origins, lengths, buildEndsFromStartAndLength(origins, lengths), heights, condition);
    }

    /**
     * Cumulative constraint. Infer the parameters ends from origins and length
     */
    @Override
    public void buildCtrCumulative(String id, XVariables.XVarInteger[] origins, XVariables.XVarInteger[] lengths, XVariables.XVarInteger[] heights, XParser.Condition condition) {
        buildCtrCumulative(id, origins, lengths, buildEndsFromStartAndLength(origins, lengths), heights, condition);
    }

    /**
     * Lex-matrix constraint.
     *
     * Map to the std. Lex constraint, on the columns and on the rows
     *
     * @param id
     * @param matrix
     * @param operator
     */
    @Override
    public void buildCtrLexMatrix(String id, XVariables.XVarInteger[][] matrix, XEnums.TypeOperator operator) {
        buildCtrLex(id, matrix, operator);

        XVariables.XVarInteger[][] mt = new XVariables.XVarInteger[matrix[0].length][matrix.length];
        for (int r = 0; r < matrix.length; r++)
            for (int c = 0; c < matrix[0].length; c++)
                mt[c][r] = matrix[r][c];

        buildCtrLex(id, mt, operator);
    }

    @Override
    public void buildCtrNoOverlap(String id, XVariables.XVarInteger[][] origins, int[][] lengths, boolean zeroIgnored) {
        //TODO semantics of zeroIgnored ???
        if(origins[0].length == 2) //2D case
            buildCtrNoOverlap2D(id,
                    Arrays.stream(origins).map(e -> e[0]).toArray(XVariables.XVarInteger[]::new),
                    Arrays.stream(lengths).mapToInt(e -> e[0]).toArray(),
                    Arrays.stream(origins).map(e -> e[1]).toArray(XVariables.XVarInteger[]::new),
                    Arrays.stream(lengths).mapToInt(e -> e[1]).toArray());
        else
            unimplementedCase();
    }

    @Override
    public void buildCtrNoOverlap(String id, XVariables.XVarInteger[][] origins, XVariables.XVarInteger[][] lengths, boolean zeroIgnored) {
        if(origins[0].length == 2) //2D case
            buildCtrNoOverlap2D(id,
                    Arrays.stream(origins).map(e -> e[0]).toArray(XVariables.XVarInteger[]::new),
                    Arrays.stream(lengths).map(e -> e[0]).toArray(XVariables.XVarInteger[]::new),
                    Arrays.stream(origins).map(e -> e[1]).toArray(XVariables.XVarInteger[]::new),
                    Arrays.stream(lengths).map(e -> e[1]).toArray(XVariables.XVarInteger[]::new));
        else
            unimplementedCase();
    }

    public void buildCtrNoOverlap2D(String id, XVariables.XVarInteger[] x, XVariables.XVarInteger[] dx, XVariables.XVarInteger[] y, XVariables.XVarInteger[] dy) {
        unimplementedCase();
    }

    public void buildCtrNoOverlap2D(String id, XVariables.XVarInteger[] x, int[] dx, XVariables.XVarInteger[] y, int[] dy) {
        unimplementedCase();
    }

    /**
     * Generate an <instantiation></instantiation> constraint that is the solution given in values.
     *
     * The main purpose of this function is to parse back XVarInteger to XVarSymbolic if it was previously converted
     *
     * @param allVariables list of variables in the solution, given by their id
     * @param allValues values[i] is the value assigned to variables[i]
     */
    public String generateInstantiationWithSymbolic(String[] allVariables, int[] allValues) {
        // Remove variables that have been generated
        ArrayList<String> variables = new ArrayList<>();
        ArrayList<Integer> values = new ArrayList<>();
        for(int i = 0; i < allVariables.length; i++) {
            if(!allVariables[i].startsWith(reservedArray)) {
                variables.add(allVariables[i]);
                values.add(allValues[i]);
            }
        }
            
        StringBuilder builder = new StringBuilder();
        builder.append("<instantiation>\n\t<list>\n\t\t");
        builder.append(String.join(" ", variables));
        builder.append("\n\t</list>\n\t<values>\n\t\t");
        for(int i = 0; i < values.size(); i++) {
            if(symbolicVariables.containsKey(variables.get(i)))
                builder.append(symbolicValuesReversed.get(values.get(i)));
            else
                builder.append(values.get(i));
            builder.append(' ');
        }
        builder.append("\n\t</values>\n</instantiation>");
        return builder.toString();
    }
}
