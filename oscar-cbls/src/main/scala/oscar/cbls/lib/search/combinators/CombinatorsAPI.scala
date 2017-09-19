package oscar.cbls.lib.search.combinators

import oscar.cbls.core.search.Neighborhood

/**
 * Created by rdl on 11-09-17.
 */
trait CombinatorsAPI {

  def bestSlopeFirst(l:List[Neighborhood],
                     tabuLength:Int = 10,
                     overrideTabuOnFullExhaust:Int = 9, refresh:Int = 100) =
    new BestSlopeFirst(l, tabuLength, overrideTabuOnFullExhaust, refresh)

  /**
   * collects statistics about the run time and progress achieved by neighborhood a
   * they can be obtained by querying this object with method toString
   * or globally on the whole neighborhood using the method statistics
   * WARNING: do not use this inside an AndThen,
   *          since the objective function is instrumented by this combinator, so the statistics will be counter-intuitive
   *
   * @param a
   * @param ignoreInitialObj
   */
  def profile(a:Neighborhood,ignoreInitialObj:Boolean = false) = new Profile(a,ignoreInitialObj)

}
