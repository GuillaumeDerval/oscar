import org.xcsp.common.XEnums;
import org.xcsp.parser.XCallbacks2;
import org.xcsp.parser.XDomains;
import org.xcsp.parser.XParser;
import org.xcsp.parser.XVariables;

import java.util.Arrays;
import java.util.UUID;
import java.util.stream.IntStream;

/**
 * An extension of the default XCallback parser that provides default decompositions for the most complex constraints
 */
public abstract class XCallbacksDecomp implements XCallbacks2 {
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
        XVariables.XVarInteger v = (XVariables.XVarInteger)XVariables.XVar.build(UUID.randomUUID().toString(), XVariables.TypeVar.integer, new XDomains.XDomInteger(0, list.length-1));
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
        XVariables.XVarInteger v = (XVariables.XVarInteger)XVariables.XVar.build(UUID.randomUUID().toString(), XVariables.TypeVar.integer, new XDomains.XDomInteger(0, list.length-1));
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
            output[i] = (XVariables.XVarInteger)XVariables.XVar.build(UUID.randomUUID().toString(), XVariables.TypeVar.integer, new XDomains.XDomInteger(min, max));
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
            output[i] = (XVariables.XVarInteger)XVariables.XVar.build(UUID.randomUUID().toString(), XVariables.TypeVar.integer, new XDomains.XDomInteger(min, max));
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
}
