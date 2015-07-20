package oscar.cp.constraints

import oscar.cp._

/**
 * Created on 03/06/15.
 * @author Cyrille Dejemeppe (cyrille.dejemeppe@gmail.com)
 */

/**
 * @constructor Create a new reservoir resource with specified parameters. For any point of time,
 *              the amount of resource in the reservoir must be between its minimal and maximal capacity
 *
 * @param startVars Variables for task starting times
 * @param durationVars Variables for task durations
 * @param endVars Variables for task ending times
 * @param productionVars Variables for task productions; represents the amounts of the resource produced by tasks
 * @param consumptionVars Variables for task consumptions; represents the amounts of the resource consumed by tasks
 * @param temporaryProdCons Booleans set to true if corresponding task produces/consumes only during its duration
 * @param minCapacity The minimal capacity of the reservoir
 * @param maxCapacity The maximal capacity of the reservoir
 * @param initialAmount The initial amount of resource in the reservoir
 */
class ReservoirResource(startVars: Array[CPIntVar], durationVars: Array[CPIntVar], endVars: Array[CPIntVar], productionVars: Array[CPIntVar], consumptionVars: Array[CPIntVar], temporaryProdCons: Array[Boolean], minCapacity: Int, maxCapacity: Int, initialAmount: Int = 0) {
  private[this] val nTasks = startVars.length
  private[this] val cpSolver: CPStore = startVars(0).store
  private[this] val startingPointOfTime = startVars.map(myVar => myVar.min).min
  private[this] val horizon = endVars.map(myVar => myVar.max).max
  private[this] val producer = Array.tabulate(nTasks)(i => productionVars(i).max > 0)
  private[this] val consumer = Array.tabulate(nTasks)(i => consumptionVars(i).max > 0)

  /* Check if the maximal capacity of the reservoir is never exceeded.
   *
   * Represented with a cumulative resource as follows:
   * - Cumulative resource capacity <- \sum consumptionVars + maxCapacity - initialAmount
   * - Each temporary producer/consumer task is not transformed
   * - Each consumer task is transformed into a new task spanning from starting point of time to the start var of former consumer
   * - Each producer task is transformed into a new task spanning from the end var of former producer to horizon
   */
  private[this] val sumConsumption = consumptionVars.map(cVar => cVar.max).sum
  private[this] val cumulativeCapacity1 = CPIntVar(sumConsumption + maxCapacity - initialAmount)(cpSolver)
  private[this] val cumulativeStart1 = Array.tabulate(nTasks)(i => {
    if (temporaryProdCons(i))
      startVars(i)
    else if(consumer(i))
      CPIntVar(startingPointOfTime)(cpSolver)
    else
      endVars(i)
  })
  private[this] val cumulativeEnd1 = Array.tabulate(nTasks)(i => {
    if (temporaryProdCons(i))
      endVars(i)
    else if(producer(i))
      CPIntVar(horizon)(cpSolver)
    else
      startVars(i)
  })
  private[this] val cumulativeDuration1 = Array.tabulate(nTasks)(i => CPIntVar((cumulativeEnd1(i).min - cumulativeStart1(i).max) to (cumulativeEnd1(i).max - cumulativeStart1(i).min))(cpSolver))
  private[this] val cumulativeDemand = Array.tabulate(nTasks)(i => if(producer(i)) productionVars(i) else consumptionVars(i))

  for (i <- 0 until nTasks) {
    cpSolver.add(cumulativeStart1(i) + cumulativeDuration1(i) == cumulativeEnd1(i))
  }
  cpSolver.add(maxCumulativeResource(cumulativeStart1, cumulativeDuration1, cumulativeEnd1, cumulativeDemand, cumulativeCapacity1))

  /* Check if reservoir amount is never under its minimal capacity.
   *
   * Represented with a cumulative resource as follows:
   * - Cumulative resource capacity <- \sum productionVars - minCapacity + initialAmount
   * - Each temporary producer/consumer task is not transformed
   * - Each producer task is transformed into a new task spanning from starting point of time to the end var of former producer
   * - Each consumer task is transformed into a new task spanning from the start var of former consumer to horizon
   */
  private[this] val sumProduction = productionVars.map(cVar => cVar.max).sum
  private[this] val cumulativeCapacity2 = CPIntVar(sumProduction - minCapacity + initialAmount)(cpSolver)
  private[this] val cumulativeStart2 = Array.tabulate(nTasks)(i => {
    if (temporaryProdCons(i))
      startVars(i)
    else if(consumer(i))
      startVars(i)
    else
      CPIntVar(startingPointOfTime)(cpSolver)
  })
  private[this] val cumulativeEnd2 = Array.tabulate(nTasks)(i => {
    if (temporaryProdCons(i))
      endVars(i)
    else if(producer(i))
      endVars(i)
    else
      CPIntVar(horizon)(cpSolver)
  })
  private[this] val cumulativeDuration2 = Array.tabulate(nTasks)(i => CPIntVar((cumulativeEnd2(i).min - cumulativeStart2(i).max) to (cumulativeEnd2(i).max - cumulativeStart2(i).min))(cpSolver))

  for (i <- 0 until nTasks) {
    cpSolver.add(cumulativeStart2(i) + cumulativeDuration2(i) == cumulativeEnd2(i))
  }
  cpSolver.add(maxCumulativeResource(cumulativeStart2, cumulativeDuration2, cumulativeEnd2, cumulativeDemand, cumulativeCapacity2))
}

object ReservoirResource {
  /**
   * @constructor Create a new reservoir resource with specified parameters. For any point of time,
   *              the amount of resource in the reservoir must be between its minimal and maximal capacity
   *
   * @param startVars Variables for task starting times
   * @param durationVars Variables for task durations
   * @param endVars Variables for task ending times
   * @param productionVars Variables for task productions; represents the amounts of the resource produced by tasks
   * @param consumptionVars Variables for task consumptions; represents the amounts of the resource consumed by tasks
   * @param minCapacity The minimal capacity of the reservoir
   * @param maxCapacity The maximal capacity of the reservoir
   * @param initialAmount The initial amount of resource in the reservoir
   */
  def apply(startVars: Array[CPIntVar], durationVars: Array[CPIntVar], endVars: Array[CPIntVar], productionVars: Array[CPIntVar], consumptionVars: Array[CPIntVar], minCapacity: Int, maxCapacity: Int, initialAmount: Int) =
    new ReservoirResource(startVars,durationVars, endVars, productionVars, consumptionVars, Array.fill(startVars.length)(false), minCapacity, maxCapacity, initialAmount)

  /**
   * @constructor Create a new reservoir resource with specified parameters. For any point of time,
   *              the amount of resource in the reservoir must be between its minimal and maximal capacity
   *
   * @param startVars Variables for task starting times
   * @param durationVars Variables for task durations
   * @param endVars Variables for task ending times
   * @param productionVars Variables for task productions; represents the amounts of the resource produced by tasks
   * @param consumptionVars Variables for task consumptions; represents the amounts of the resource consumed by tasks
   * @param temporaryProdCons Booleans set to true if corresponding task produces/consumes only during its duration
   * @param minCapacity The minimal capacity of the reservoir
   * @param maxCapacity The maximal capacity of the reservoir
   * @param initialAmount The initial amount of resource in the reservoir
   */
  def apply(startVars: Array[CPIntVar], durationVars: Array[CPIntVar], endVars: Array[CPIntVar], productionVars: Array[CPIntVar], consumptionVars: Array[CPIntVar], temporaryProdCons: Array[Boolean], minCapacity: Int, maxCapacity: Int, initialAmount: Int) =
    new ReservoirResource(startVars,durationVars, endVars, productionVars, consumptionVars, temporaryProdCons, minCapacity, maxCapacity, initialAmount)
}