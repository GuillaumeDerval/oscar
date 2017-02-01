package oscar.modeling.vars.mip

import oscar.modeling.vars.VarImplem

/**
  * An instantiated MIP variable
  */
trait MIPVar extends VarImplem
trait MIPHasSolution { var hasSolution: Boolean }