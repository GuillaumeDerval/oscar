package oscar.cp.searches.lns.operators

import oscar.algo.search.{Branching, SearchStatistics}
import oscar.cp.{CPIntVar, CPSolver}
import oscar.cp.searches.lns.CPIntSol
import oscar.cp.searches.lns.search.ALNSConfig
import oscar.cp.searches.lns.selection._

import scala.collection.mutable.ArrayBuffer

/**
  * Companion object for the ALNSBuilder class. Contains the keys and default values for the ALNS elements
  * supported by the ALNSBuilder.
  */
object ALNSBuilder{

  // Random keyword:
  val Random = "Random"

  /**
    * Available relax operators:
    */

  // K successive relaxation:
  val KSuccessive = "KSuccessive"

  // Propagation guided relaxation:
  val PropGuided = "PropGuided"

  // Reversed propagation guided relaxation:
  val RevPropGuided  = "RevPropGuided"

  // Value guided relaxation:
  val ValGuided = "ValGuided"
  val DefValGuidedParam2 = Array("Random", "MaxGroup", "MinGroup", "MaxVal", "MinVal")

  // Full relaxation:
  val FullRelax = "FullRelax"

  // Default relaxation parameters (percentage)
  val DefRelaxParam = Array(0.25, 0.50, 0.75) //Percentage of the neighbourhood which is relaxed

  //TODO: implement other relaxation functions

  /**
    * Available search operators:
    */
  // Conflict ordering search:
  val ConfOrder = "ConfOrder"

  // First fail search:
  val FirstFail = "FirstFail"

  // Last conflict search:
  val LastConf = "LastConf"

  // Binary split search:
  val BinSplit = "BinSplit"

  //Extentionnal Oriented search:
  val ExtOriented = "ExtOriented"

  //Max weighted degree
  val WeightDeg = "WeightDeg"

  // Available value Heuristic functions:
  val ValHeurisMin = "Min"
  val ValHeurisMax = "Max"
  val ValHeurisBoth = "Both"

  //TODO: implement other search heuristics

  /**
    * Available adaptive stores:
    */
  val RWheel = "RWheel"
  val Priority = "Priority"

  /**
    * Available performance metrics:
    */
  val LastImprov = "LastImprov"
  val AvgImprov = "AvgImprov"
  val TTI = "TTI"

  /**
    * Default values:
    */
  protected val DefParamSelectKey = RWheel
  protected val DefParamMetricKey = AvgImprov
  protected val DefNoParamFailThreshold = 3
  protected val DefWithParamFailThreshold = 0
}

/**
  * Builder class for ALNS elements (operators, adaptive stores and performance metrics). The elements are built based
  * on the key(s) given when calling a method. These keys are defined in the companion object.
  */
class ALNSBuilder(solver: CPSolver, vars: Array[CPIntVar], config: ALNSConfig){

  // lazy vals used for some operators:
  lazy val N: Int = vars.length // The number of vars
  lazy val maxNeighSize: Double = vars.map(x => math.log(x.size)).sum // Max neighbourhood size for propagation guided relax
  lazy val closeness: Option[ClosenessStore] = if(N*N*16 > (config.memLimit * 1000000)) None else Some(new ClosenessStore(N)) // Closeness store used for propagation guided relax

  private def wrapSearch(search: Branching): (CPIntSol) => Unit = {
    (sol: CPIntSol) => solver.search(search)
  }

  def instantiateCoupledOperators: Array[ALNSOperator] =(
    for(
      relaxKey <- config.relaxOperatorKeys;
      searchKey <- config.searchOperatorKeys
    )yield (relaxKey, searchKey)
    ).flatMap(pair => instantiateCoupledOperator(pair._1, pair._2))

  def instantiateRelaxOperators: Array[ALNSOperator] = {
    if (closeness.isDefined) config.relaxOperatorKeys.map(x => instantiateRelaxOperator(x, config.paramSelectionKey, config.paramMetricKey))
    else config.relaxOperatorKeys.filter(x => !x.equals(ALNSBuilder.RevPropGuided)).map(x => instantiateRelaxOperator(x, config.paramSelectionKey, config.paramMetricKey))
  }

  def instantiateSearchOperators: Array[ALNSOperator] = config.searchOperatorKeys.flatMap(instantiateSearchOperators(_))

  def instantiateOperatorStore(operators: Array[ALNSOperator]): AdaptiveStore[ALNSOperator] =
    instantiateAdaptiveStore[ALNSOperator](config.opSelectionKey, operators, config.opMetricKey)

  def instantiateMetric(): (ALNSElement, Int, SearchStatistics) => Double = instantiateMetric(config.opMetricKey)

  /**
    * Instantiates one or more coupled operators depending on the default parameters of the relaxation and search functions.
    * @param relaxKey the relaxation function key
    * @param searchKey the search function key
    * @return an array of coupled operator (one for each possible parameter combination)
    */
  private def instantiateCoupledOperator(relaxKey: String, searchKey: String): Array[ALNSOperator] =
    for {
      (relaxName, relaxFunction) <- instantiateRelaxFunctions(relaxKey)
      (searchName, searchFunction) <- instantiateSearchFunctions(searchKey)
    } yield new ALNSNoParamOperator(
      relaxName + "_" + searchName,
      ALNSBuilder.DefNoParamFailThreshold,
      (sol: CPIntSol) => {
        relaxFunction(sol)
        searchFunction(sol)
      }
    )

  private def instantiateRelaxFunctions(opKey: String): Array[(String, CPIntSol => Unit)] = opKey match{

    case ALNSBuilder.Random =>
      ALNSBuilder.DefRelaxParam
        .map(x => Math.round(N * x).toInt)
        .map(x => (
          opKey + "(" + x.toString + ")",
          (sol: CPIntSol) => RelaxationFunctions.randomRelax(solver, vars, sol, x))
        )

    case ALNSBuilder.KSuccessive =>
      ALNSBuilder.DefRelaxParam
        .map(x => Math.round(N * x).toInt)
        .map(x => (
          opKey + "(" + x.toString + ")",
          (sol: CPIntSol) => RelaxationFunctions.successiveRelax(solver, vars, sol, x))
        )

    case ALNSBuilder.PropGuided =>
      ALNSBuilder.DefRelaxParam
        .map(x => maxNeighSize * x)
        .map(x => (
          opKey + "(" + x.toString + ")",
          (sol: CPIntSol) => RelaxationFunctions.propagationGuidedRelax(solver, vars, sol, closeness, x))
        )

    case ALNSBuilder.RevPropGuided =>
      if(closeness.isDefined) {
        ALNSBuilder.DefRelaxParam
          .map(x => maxNeighSize * x)
          .map(x => (
            opKey + "(" + x.toString + ")",
            (sol: CPIntSol) => RelaxationFunctions.reversedPropagationGuidedRelax(solver, vars, sol, closeness.get, x))
          )
      }
      else Array[(String, CPIntSol => Unit)]()

    case ALNSBuilder.ValGuided =>
      for(
        k <- ALNSBuilder.DefRelaxParam.map(x => Math.round(N * x).toInt);
        scheme <- ALNSBuilder.DefValGuidedParam2
      )yield (
        opKey + "(" + k.toString + "," + scheme + ")",
        (sol: CPIntSol) => RelaxationFunctions.valueGuidedRelax(solver, vars, sol, k, scheme)
      )

    case ALNSBuilder.FullRelax => Array((opKey, _ => Unit))
  }

  private def instantiateSearchFunctions(opKey: String): Array[(String, CPIntSol => Unit)] = {
    val functions = ArrayBuffer[(String, CPIntSol => Unit)]()
    if(config.valHeuristic == ALNSBuilder.ValHeurisMin || config.valHeuristic == ALNSBuilder.ValHeurisBoth){
      functions += instantiateSearchFunction(opKey, valMax = false, valLearn = false)
      if(config.valLearn) functions += instantiateSearchFunction(opKey, valMax = false, valLearn = true)
    }
    if(config.valHeuristic == ALNSBuilder.ValHeurisMax || config.valHeuristic == ALNSBuilder.ValHeurisBoth){
      functions += instantiateSearchFunction(opKey, valMax = true, valLearn = false)
      if(config.valLearn) functions += instantiateSearchFunction(opKey, valMax = true, valLearn = true)
    }
    functions.toArray
  }

  private def instantiateSearchFunction(opKey: String, valMax: Boolean, valLearn: Boolean): (String, CPIntSol => Unit) = {
    val opName = opKey + (if(valLearn && opKey != ALNSBuilder.WeightDeg) "(valLearn" else "(") + (if(valMax) "Max)" else "Min)")
    (opName, wrapSearch(
      opKey match{
        case ALNSBuilder.ConfOrder => SearchFunctions.conflictOrdering(vars, valMax, valLearn)
        case ALNSBuilder.FirstFail => SearchFunctions.firstFail(vars, valMax, valLearn)
        case ALNSBuilder.LastConf => SearchFunctions.lastConflict(vars, valMax, valLearn)
        case ALNSBuilder.BinSplit => SearchFunctions.binarySplit(vars, valMax, valLearn)
        case ALNSBuilder.ExtOriented => SearchFunctions.extensionalOriented(vars, valMax, valLearn)
        case ALNSBuilder.WeightDeg => SearchFunctions.weightedDegree(vars, valMax, 0.99)
      }
    ))
  }

  /**
    * Instantiate a loose ANLS relax operator with the given parameters.
    * @param opKey The operator key
    * @param paramSelectKey The parameter selection key (only if the operator uses adaptive parameters)
    * @param paramMetricKey The parameter metric key (only if the operator uses adaptive parameters)
    * @return an ALNSOperator object
    */
  private def instantiateRelaxOperator(
                                        opKey: String,
                                        paramSelectKey: String = ALNSBuilder.DefParamSelectKey,
                                        paramMetricKey: String = ALNSBuilder.DefParamMetricKey
                              ): ALNSOperator = opKey match {

    case ALNSBuilder.Random => new ALNSSingleParamOperator[Int](
      ALNSBuilder.Random,
      ALNSBuilder.DefWithParamFailThreshold,
      RelaxationFunctions.randomRelax(solver, vars, _: CPIntSol, _: Int),
      instantiateAdaptiveStore(
        paramSelectKey,
        ALNSBuilder.DefRelaxParam
          .map(x => Math.round(N * x).toInt)
          .map(x => new ALNSParameter[Int](x, ALNSBuilder.DefNoParamFailThreshold)),
        paramMetricKey
      ),
      instantiateMetric(paramMetricKey)
    )

    case ALNSBuilder.KSuccessive => new ALNSSingleParamOperator[Int](
      ALNSBuilder.KSuccessive,
      ALNSBuilder.DefWithParamFailThreshold,
      RelaxationFunctions.successiveRelax(solver, vars, _: CPIntSol, _: Int),
      instantiateAdaptiveStore(
        paramSelectKey,
        ALNSBuilder.DefRelaxParam
          .map(x => Math.round(N * x).toInt)
          .map(x => new ALNSParameter[Int](x, ALNSBuilder.DefNoParamFailThreshold)),
        paramMetricKey
      ),
      instantiateMetric(paramMetricKey)
    )

    case ALNSBuilder.PropGuided => new ALNSSingleParamOperator[Double](
      ALNSBuilder.PropGuided,
      ALNSBuilder.DefWithParamFailThreshold,
      RelaxationFunctions.propagationGuidedRelax(solver, vars, _: CPIntSol, closeness, _: Double),
      instantiateAdaptiveStore(
        paramSelectKey,
        ALNSBuilder.DefRelaxParam
          .map(x => x * maxNeighSize)
          .map(x => new ALNSParameter[Double](x, ALNSBuilder.DefNoParamFailThreshold)),
        paramMetricKey),
      instantiateMetric(paramMetricKey)
    )

    case ALNSBuilder.RevPropGuided => new ALNSSingleParamOperator[Double](
      ALNSBuilder.RevPropGuided,
      ALNSBuilder.DefWithParamFailThreshold,
      RelaxationFunctions.reversedPropagationGuidedRelax(solver, vars, _: CPIntSol, closeness.get, _: Double),
      instantiateAdaptiveStore(
        paramSelectKey,
        ALNSBuilder.DefRelaxParam
          .map(x => x * maxNeighSize)
          .map(x => new ALNSParameter[Double](x, ALNSBuilder.DefNoParamFailThreshold)),
        paramMetricKey),
      instantiateMetric(paramMetricKey)
    )

    case ALNSBuilder.ValGuided => new ALNSTwoParamsOperator[Int, String](
      ALNSBuilder.ValGuided,
      ALNSBuilder.DefWithParamFailThreshold,
      RelaxationFunctions.valueGuidedRelax(solver, vars, _:CPIntSol, _: Int, _: String),
      instantiateAdaptiveStore(
        paramSelectKey,
        ALNSBuilder.DefRelaxParam
          .map(x => Math.round(N * x).toInt)
          .map(x => new ALNSParameter[Int](x, ALNSBuilder.DefNoParamFailThreshold)),
        paramMetricKey
      ),
      instantiateAdaptiveStore(
        paramSelectKey,
        ALNSBuilder.DefValGuidedParam2.map(x => new ALNSParameter[String](x, ALNSBuilder.DefNoParamFailThreshold)),
        paramMetricKey
      ),
      instantiateMetric(paramMetricKey)
    )

    case ALNSBuilder.FullRelax => new ALNSNoParamOperator(
      ALNSBuilder.FullRelax,
      ALNSBuilder.DefNoParamFailThreshold,
      _ => Unit
    )
  }

  /**
    * Instantiate loose ANLS search operators for the given keys.
    * @param opKey The operator key
    * @return an array of ALNSOperator objects
    */
  private def instantiateSearchOperators(opKey: String): Array[ALNSOperator] = instantiateSearchFunctions(opKey).map{
    case (name, function) => new ALNSNoParamOperator(name, ALNSBuilder.DefNoParamFailThreshold, function)
  }

  /**
    * Instantiate an adaptive store with the given parameters.
    * @param selectionKey the adaptive store key
    * @param elems the elements to put in the store
    * @param metricKey the performance metric used
    * @tparam T the type of the elements in the store
    * @return An AdaptiveStore
    */
  private def instantiateAdaptiveStore[T](
                                    selectionKey: String,
                                    elems: Array[T],
                                    metricKey: String
                                  ): AdaptiveStore[T] = selectionKey match{

    case ALNSBuilder.RWheel => new RouletteWheel[T](
      elems,
      metricKey match{
        case ALNSBuilder.LastImprov => 0.5
        case ALNSBuilder.AvgImprov => 1.0
        case ALNSBuilder.TTI => 1.0
      },
      metricKey match{
        case ALNSBuilder.LastImprov => false
        case ALNSBuilder.AvgImprov => false
        case ALNSBuilder.TTI => true
      }
    )

    case ALNSBuilder.Priority => new PriorityStore[T](
      elems,
      metricKey match{
        case ALNSBuilder.LastImprov => 0.5
        case ALNSBuilder.AvgImprov => 1.0
        case ALNSBuilder.TTI => 1.0
      },
      metricKey match{
        case ALNSBuilder.LastImprov => false
        case ALNSBuilder.AvgImprov => false
        case ALNSBuilder.TTI => true
      }
    )

    case ALNSBuilder.Random => new RandomStore[T](elems)
  }

  /**
    * Instantiates a metric function.
    * @param metricKey the key of the metric function
    * @return a metric function
    */
  private def instantiateMetric(metricKey: String): (ALNSElement, Int, SearchStatistics) => Double = metricKey match{
    case ALNSBuilder.LastImprov => Metrics.lastImprovement
    case ALNSBuilder.AvgImprov => Metrics.averageImprovement
    case ALNSBuilder.TTI => Metrics.timeToImprovement
  }
}
