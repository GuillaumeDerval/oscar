package oscar.modeling.solvers.cp.searches.alns.operators

import oscar.algo.search.SearchStatistics
import oscar.cp.searches.lns.CPIntSol
import oscar.cp.searches.lns.operators._
import oscar.cp.searches.lns.search.ALNSConfig
import oscar.cp.searches.lns.selection.{AdaptiveStore, Metrics, PriorityStore, RouletteWheel}
import oscar.modeling.models.cp.CPModel
import oscar.modeling.solvers.cp.Branchings.BranchingInstantiator
import oscar.modeling.vars.IntVar

/**
  * Companion object for the ALNSBuilder class. Contains the keys and default values for the ALNS elements
  * supported by the ALNSBuilder.
  */
object ALNSBuilder{

  /**
    * Available relax operators:
    */
  // Random relaxation:
  val Random = "Random"
  val Random_Param1 = Array(0.10, 0.25, 0.50, 0.75) //Percentage of the neighbourhood which is relaxed

  // K successive relaxation:
  val KSuccessive = "KSuccessive"
  val KSuccessive_Param1 = Array(0.10, 0.25, 0.50, 0.75) //Percentage of the neighbourhood which is relaxed

  // Propagation guided relaxation:
  val PropGuided = "PropGuided"
  val PropGuided_Param1 = Array(0.10, 0.25, 0.50, 0.75) //Percentage of the neighbourhood which is relaxed

  // Reversed propagation guided relaxation:
  val RevPropGuided  = "RevPropGuided"
  val RevPropGuided_Param1 = Array(0.10, 0.25, 0.50, 0.75) //Percentage of the neighbourhood which is relaxed

  //TODO: implement other relaxation functions

  /**
    * Available search operators:
    */
  // Conflict ordering search:
  val ConfOrder = "ConfOrder"
  val ConfOrderValLearn = "ConfOrderValLearn"

  // First fail search:
  val FirstFail = "FirstFail"
  val FirstFailValLearn = "FirstFailValLearn"

  // Last conflict search:
  val LastConf = "LastConf"
  val LastConfValLearn = "LastConfValLearn"

  // Binary split search:
  val BinSplit = "BinSplit"
  val BinSplitValLearn = "BinSplitValLearn"

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
class ALNSBuilder(model: CPModel, vars: Array[IntVar], maximizeObjective: Boolean, config: ALNSConfig){

  // lazy vals used for some operators:
  lazy val N: Int = vars.length // The number of vars
  lazy val maxNeighSize: Double = vars.map(x => math.log(x.size)).sum // Max neighbourhood size for propagation guided relax
  lazy val closeness = new ClosenessStore(N) // Closeness store used for propagation guided relax

  private def wrapSearch(search: BranchingInstantiator): (CPIntSol) => Unit = {
    (sol: CPIntSol) => model.cpSolver.search(search(model))
  }

  def instantiateCoupledOperators: Array[ALNSOperator] =
    (for(relaxKey <- config.relaxOperatorKeys; searchKey <- config.searchOperatorKeys) yield (relaxKey, searchKey))
    .flatMap(pair => instantiateCoupledOperator(pair._1, pair._2))

  def instantiateRelaxOperators: Array[ALNSOperator] =
    config.relaxOperatorKeys.map(x => instantiateLooseOperator(x, config.paramSelectionKey, config.paramMetricKey))

  def instantiateSearchOperators: Array[ALNSOperator] =
    config.searchOperatorKeys.map(x => instantiateLooseOperator(x, config.paramSelectionKey, config.paramMetricKey))

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
      (relaxParam, relaxFunction) <- instantiateRelaxFunctions(relaxKey)
      (searchParam, searchFunction) <- instantiateSearchFunctions(searchKey)
    } yield new ALNSNoParamOperator(
      relaxKey + "(" + relaxParam + ")_" + searchKey + "(" + searchParam + ")",
      ALNSBuilder.DefNoParamFailThreshold,
      (sol: CPIntSol) => {
        relaxFunction(sol)
        searchFunction(sol)
      }
    )

  private def instantiateRelaxFunctions(opKey: String): Array[(String, CPIntSol => Unit)] = opKey match{

    case ALNSBuilder.Random =>
      ALNSBuilder.Random_Param1
        .map(x => Math.round(N * x).toInt)
        .map(x => (x.toString, (sol: CPIntSol) => RelaxationFunctions.randomRelax(model, vars, sol, x)))

    case ALNSBuilder.KSuccessive =>
      ALNSBuilder.KSuccessive_Param1
        .map(x => Math.round(N * x).toInt)
        .map(x => (x.toString, (sol: CPIntSol) => RelaxationFunctions.successiveRelax(model, vars, sol, x)))

    case ALNSBuilder.PropGuided =>
      ALNSBuilder.PropGuided_Param1
        .map(x => maxNeighSize * x)
        .map(x => (
          x.toString,
          (sol: CPIntSol) => RelaxationFunctions.propagationGuidedRelax(model, vars, sol, closeness, x))
        )

    case ALNSBuilder.RevPropGuided =>
      ALNSBuilder.RevPropGuided_Param1
        .map(x => maxNeighSize * x)
        .map(x => (
          x.toString,
          (sol: CPIntSol) => RelaxationFunctions.reversedPropagationGuidedRelax(model, vars, sol, closeness, x))
        )
  }

  private def instantiateSearchFunctions(opKey: String): Array[(String, CPIntSol => Unit)] = opKey match{

    case ALNSBuilder.ConfOrder =>
      Array(("", wrapSearch(SearchFunctions.conflictOrdering(vars, maximizeObjective, valLearn = false))))

    case ALNSBuilder.ConfOrderValLearn =>
      Array(("", wrapSearch(SearchFunctions.conflictOrdering(vars, maximizeObjective, valLearn = true))))

    case ALNSBuilder.FirstFail =>
      Array(("", wrapSearch(SearchFunctions.firstFail(vars, maximizeObjective, valLearn = false))))

    case ALNSBuilder.FirstFailValLearn =>
      Array(("", wrapSearch(SearchFunctions.firstFail(vars, maximizeObjective, valLearn = true))))

    case ALNSBuilder.LastConf =>
      Array(("", wrapSearch(SearchFunctions.lastConflict(vars, maximizeObjective, valLearn = false))))

    case ALNSBuilder.LastConfValLearn =>
      Array(("", wrapSearch(SearchFunctions.lastConflict(vars, maximizeObjective, valLearn = true))))

    case ALNSBuilder.BinSplit =>
      Array(("", wrapSearch(SearchFunctions.binarySplit(vars, maximizeObjective, valLearn = false))))

    case ALNSBuilder.BinSplitValLearn =>
      Array(("", wrapSearch(SearchFunctions.binarySplit(vars, maximizeObjective, valLearn = true))))
  }

  /**
    * Instantiate a loose ANLS operator with the given parameters.
    * @param opKey The operator key
    * @param paramSelectKey The parameter selection key (only if the operator uses adaptive parameters)
    * @param paramMetricKey The parameter metric key (only if the operator uses adaptive parameters)
    * @return an ALNSOperator object
    */
  private def instantiateLooseOperator(
                                        opKey: String,
                                        paramSelectKey: String = ALNSBuilder.DefParamSelectKey,
                                        paramMetricKey: String = ALNSBuilder.DefParamMetricKey
                              ): ALNSOperator = opKey match{

    // Relaxation operators:
    case ALNSBuilder.Random => new ALNSSingleParamOperator[Int](
      ALNSBuilder.Random,
      ALNSBuilder.DefWithParamFailThreshold,
      RelaxationFunctions.randomRelax(model, vars, _:CPIntSol, _:Int),
      instantiateAdaptiveStore(
        paramSelectKey,
        ALNSBuilder.Random_Param1
          .map(x => Math.round(N * x).toInt)
          .map(x => new ALNSParameter[Int](x, ALNSBuilder.DefNoParamFailThreshold)),
        paramMetricKey
      ),
      instantiateMetric(paramMetricKey)
    )

    case ALNSBuilder.KSuccessive => new ALNSSingleParamOperator[Int](
      ALNSBuilder.KSuccessive,
      ALNSBuilder.DefWithParamFailThreshold,
      RelaxationFunctions.successiveRelax(model, vars, _:CPIntSol, _:Int),
      instantiateAdaptiveStore(
        paramSelectKey,
        ALNSBuilder.KSuccessive_Param1
          .map(x => Math.round(N * x).toInt)
          .map(x => new ALNSParameter[Int](x, ALNSBuilder.DefNoParamFailThreshold)),
        paramMetricKey
      ),
      instantiateMetric(paramMetricKey)
    )

    case ALNSBuilder.PropGuided => new ALNSSingleParamOperator[Double](
      ALNSBuilder.PropGuided,
      ALNSBuilder.DefWithParamFailThreshold,
      RelaxationFunctions.propagationGuidedRelax(model, vars,_:CPIntSol, closeness, _:Double),
      instantiateAdaptiveStore(
        paramSelectKey,
        ALNSBuilder.PropGuided_Param1
          .map(x => x * maxNeighSize)
          .map(x => new ALNSParameter[Double](x, ALNSBuilder.DefNoParamFailThreshold)),
        paramMetricKey),
      instantiateMetric(paramMetricKey)
    )

    case ALNSBuilder.RevPropGuided => new ALNSSingleParamOperator[Double](
      ALNSBuilder.RevPropGuided,
      ALNSBuilder.DefWithParamFailThreshold,
      RelaxationFunctions.reversedPropagationGuidedRelax(model, vars, _:CPIntSol, closeness, _:Double),
      instantiateAdaptiveStore(
        paramSelectKey,
        ALNSBuilder.RevPropGuided_Param1
          .map(x => x * maxNeighSize)
          .map(x => new ALNSParameter[Double](x, ALNSBuilder.DefNoParamFailThreshold)),
        paramMetricKey),
      instantiateMetric(paramMetricKey)
    )

    //Search operators:
    case ALNSBuilder.ConfOrder => new ALNSNoParamOperator(
      ALNSBuilder.ConfOrder,
      ALNSBuilder.DefNoParamFailThreshold,
      wrapSearch(SearchFunctions.conflictOrdering(vars, maximizeObjective, valLearn = false))
    )

    case ALNSBuilder.ConfOrderValLearn => new ALNSNoParamOperator(
      ALNSBuilder.ConfOrderValLearn,
      ALNSBuilder.DefNoParamFailThreshold,
      wrapSearch(SearchFunctions.conflictOrdering(vars, maximizeObjective, valLearn = true))
    )

    case ALNSBuilder.FirstFail => new ALNSNoParamOperator(
      ALNSBuilder.FirstFail,
      ALNSBuilder.DefNoParamFailThreshold,
      wrapSearch(SearchFunctions.firstFail(vars, maximizeObjective, valLearn = false))
    )

    case ALNSBuilder.FirstFailValLearn => new ALNSNoParamOperator(
      ALNSBuilder.FirstFailValLearn,
      ALNSBuilder.DefNoParamFailThreshold,
      wrapSearch(SearchFunctions.firstFail(vars, maximizeObjective, valLearn = true))
    )

    case ALNSBuilder.LastConf => new ALNSNoParamOperator(
      ALNSBuilder.LastConf,
      ALNSBuilder.DefNoParamFailThreshold,
      wrapSearch(SearchFunctions.lastConflict(vars, maximizeObjective, valLearn = false))
    )

    case ALNSBuilder.LastConfValLearn => new ALNSNoParamOperator(
      ALNSBuilder.LastConfValLearn,
      ALNSBuilder.DefNoParamFailThreshold,
      wrapSearch(SearchFunctions.lastConflict(vars, maximizeObjective, valLearn = true))
    )

    case ALNSBuilder.BinSplit => new ALNSNoParamOperator(
      ALNSBuilder.BinSplit,
      ALNSBuilder.DefNoParamFailThreshold,
      wrapSearch(SearchFunctions.binarySplit(vars, maximizeObjective, valLearn = false))
    )

    case ALNSBuilder.BinSplitValLearn => new ALNSNoParamOperator(
      ALNSBuilder.BinSplitValLearn,
      ALNSBuilder.DefNoParamFailThreshold,
      wrapSearch(SearchFunctions.binarySplit(vars, maximizeObjective, valLearn = true))
    )
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
        case ALNSBuilder.LastImprov => 0.2
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
        case ALNSBuilder.LastImprov => 0.2
        case ALNSBuilder.AvgImprov => 1.0
        case ALNSBuilder.TTI => 1.0
      },
      metricKey match{
        case ALNSBuilder.LastImprov => false
        case ALNSBuilder.AvgImprov => false
        case ALNSBuilder.TTI => true
      }
    )
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
