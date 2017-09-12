package oscar.cbls.modeling

import oscar.cbls.lib.search.combinators.CombinatorsAPI

/**
 * Created by rdl on 11-09-17.
 */
trait ModelingAPI extends Constraints
with LogicInvariants
with MinMaxInvariants
with NumericInvariants
with SetInvariants
with SeqInvariants
with CombinatorsAPI
with StandardNeighborhoods