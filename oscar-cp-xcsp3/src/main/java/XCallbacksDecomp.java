import org.xcsp.parser.XCallbacks2;
import org.xcsp.parser.XVariables;

import java.util.Arrays;

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
        for(int i = 0; i < matrix[0].length; i++) {
            int finalI = i;
            this.buildCtrAllDifferent(id, Arrays.stream(matrix).map(l -> l[finalI]).toArray(XVariables.XVarInteger[]::new));
        }
    }
}
