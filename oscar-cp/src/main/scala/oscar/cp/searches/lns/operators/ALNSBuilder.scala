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

  /**
    * Available relax operators:
    */

  val RandomRelax = "RandomRelax"
  val KSuccessiveRelax = "KSuccessiveRelax"

  // Propagation guided relaxation:
  val PropGuidedRelax = "PropGuidedRelax"

  // Reversed propagation guided relaxation:
  val RevPropGuidedRelax  = "RevPropGuidedRelax"

  // Value guided relaxation:
  val RandomValGroupsRelax = "RandomValGroupsRelax"
  val MinValGroupsRelax = "MinValGroupsRelax"
  val MaxValGroupsRelax = "MaxValGroupsRelax"
  val MinValRelax = "MinValRelax"
  val MaxValRelax = "MaxValRelax"
  val MinMaxValRelax = "MinMaxValRelax"

  // Circuit relaxation:
  val CircuitSeqRelax = "CircuitSeqRelax"
  val CircuitKoptRelax = "CircuitKoptRelax"

  // Scheduling relaxation
  val ValWindowRelax = "ValWindowRelax"
  val DefValWindowParam1 = Array(0.5, 0.75, 1.0)
  val DefValWindowParam2 = Array(0.1, 0.25, 0.5)

  // Full relaxation:
  val FullRelax = "FullRelax"

  // Default relaxation size (percentage)
  val DefRelaxParam = Array(0.02, 0.05, 0.1, 0.2, 0.4) //Percentage of the neighbourhood which is relaxed
//  val DefRelaxParam = (0.1 to 0.9 by 0.1).toArray //Percentage of the neighbourhood which is relaxed

  //TODO: implement other relaxation functions

  /**
    * Available search operators:
    */
  // Conflict ordering search:
  val ConfOrderSearch = "ConfOrderSearch"

  // First fail search:
  val FirstFailSearch = "FirstFail"

  // Last conflict search:
  val LastConfSearch = "LastConf"

  // Binary split search:
  val BinSplitSearch = "BinSplit"

  //Extentionnal Oriented search:
  val ExtOrientedSearch = "ExtOrientedSearch"

  //Max weighted degree
  val WeightDegSearch = "WeightDegSearch"
  val DefWeigDegreeParam2 = 0.99

  // Available value Heuristic functions:
  val DefValHeuris = Array(/*"Min", */"Max"/*, "Median", "Random"*/)

  //Default Backtracking:
  val DefNFailures = Array(10, 100, 1000, 10000, 0)

  //Default Discrepancy:
  val DefMaxDiscrepancy = Array(Int.MaxValue)

  //TODO: implement other search heuristics

  /**
    * Available adaptive stores:
    */
  val RWheel = "RWheel"
  val Priority = "Priority"
  val Random = "Random"

  /**
    * Available performance metrics:
    */
  val LastImprov = "LastImprov"
  val LastImprovRatio = "LastImprov"
  val AvgImprov = "AvgImprov"
  val AvgImprovRatio = "AvgImprovRatio"
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

  def instantiateCoupledOperators: Array[ALNSOperator] =(
    for(
      relaxKey <- config.relaxOperatorKeys;
      searchKey <- config.searchOperatorKeys
    )yield (relaxKey, searchKey)
    ).flatMap(pair => instantiateCoupledOperator(pair._1, pair._2)
  )

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
      nFailures <- ALNSBuilder.DefNFailures
      maxDiscrepancy <- ALNSBuilder.DefMaxDiscrepancy
    } yield new ALNSNoParamOperator(
      relaxName + "_" + searchName + "(" + (if(nFailures == 0) "NoFailLimit" else nFailures) + "," + (if(maxDiscrepancy == Int.MaxValue) "NoMaxDiscrepancy" else maxDiscrepancy) + ")",
      if(config.opDeactivation) ALNSBuilder.DefNoParamFailThreshold else 0,
      () => (
        (sol:CPIntSol) => {
          relaxFunction(sol)
          searchFunction(sol)
        },
        Some(nFailures),
        Some(maxDiscrepancy)
      )
    )

  def instantiateRelaxOperators: Array[ALNSOperator] = {
    if (closeness.isDefined) config.relaxOperatorKeys.map(x => instantiateRelaxOperator(x, config.paramSelectionKey, config.paramMetricKey))
    else config.relaxOperatorKeys.filter(x => !x.equals(ALNSBuilder.RevPropGuidedRelax)).map(x => instantiateRelaxOperator(x, config.paramSelectionKey, config.paramMetricKey))
  }

  def instantiateSearchOperators: Array[ALNSOperator] = config.searchOperatorKeys.flatMap(instantiateSearchOperators(_))

  def instantiateOperatorStore(operators: Array[ALNSOperator]): AdaptiveStore[ALNSOperator] =
    instantiateAdaptiveStore[ALNSOperator](config.opSelectionKey, operators, config.opMetricKey)

  def instantiateMetric(): (ALNSElement, Int, SearchStatistics) => Double = instantiateMetric(config.opMetricKey)



  private def instantiateRelaxFunctions(opKey: String): Array[(String, CPIntSol => Unit)] = opKey match{

    case ALNSBuilder.RandomRelax =>
      ALNSBuilder.DefRelaxParam
        .map(x => Math.round(N * x).toInt)
        .map(x => (
          opKey + "(" + x.toString + ")",
          (sol: CPIntSol) => RelaxationFunctions.randomRelax(solver, vars, sol, x))
        )

    case ALNSBuilder.KSuccessiveRelax =>
      ALNSBuilder.DefRelaxParam
        .map(x => Math.round(N * x).toInt)
        .map(x => (
          opKey + "(" + x.toString + ")",
          (sol: CPIntSol) => RelaxationFunctions.successiveRelax(solver, vars, sol, x))
        )

    case ALNSBuilder.PropGuidedRelax =>
      ALNSBuilder.DefRelaxParam
        .map(x => maxNeighSize * x)
        .map(x => (
          opKey + "(" + x.toString + ")",
          (sol: CPIntSol) => RelaxationFunctions.propagationGuidedRelax(solver, vars, sol, closeness, x))
        )

    case ALNSBuilder.RevPropGuidedRelax =>
      if(closeness.isDefined) {
        ALNSBuilder.DefRelaxParam
          .map(x => maxNeighSize * x)
          .map(x => (
            opKey + "(" + x.toString + ")",
            (sol: CPIntSol) => RelaxationFunctions.reversedPropagationGuidedRelax(solver, vars, sol, closeness.get, x))
          )
      }
      else Array[(String, CPIntSol => Unit)]()

    case ALNSBuilder.RandomValGroupsRelax =>
      ALNSBuilder.DefRelaxParam
        .map(x => Math.round(N * x).toInt)
        .map(x => (
          opKey + "(" + x.toString + ")",
          (sol: CPIntSol) => RelaxationFunctions.randomGroupsRelax(solver, vars, sol, x))
        )

    case ALNSBuilder.MinValGroupsRelax =>
      ALNSBuilder.DefRelaxParam
        .map(x => Math.round(N * x).toInt)
        .map(x => (
          opKey + "(" + x.toString + ")",
          (sol: CPIntSol) => RelaxationFunctions.minGroupsRelax(solver, vars, sol, x))
        )

    case ALNSBuilder.MaxValGroupsRelax =>
      ALNSBuilder.DefRelaxParam
        .map(x => Math.round(N * x).toInt)
        .map(x => (
          opKey + "(" + x.toString + ")",
          (sol: CPIntSol) => RelaxationFunctions.maxGroupsRelax(solver, vars, sol, x))
        )

    case ALNSBuilder.MinValRelax =>
      ALNSBuilder.DefRelaxParam
        .map(x => Math.round(N * x).toInt)
        .map(x => (
          opKey + "(" + x.toString + ")",
          (sol: CPIntSol) => RelaxationFunctions.minValRelax(solver, vars, sol, x))
        )

    case ALNSBuilder.MaxValRelax =>
      ALNSBuilder.DefRelaxParam
        .map(x => Math.round(N * x).toInt)
        .map(x => (
          opKey + "(" + x.toString + ")",
          (sol: CPIntSol) => RelaxationFunctions.maxValRelax(solver, vars, sol, x))
        )

    case ALNSBuilder.MinMaxValRelax =>
      ALNSBuilder.DefRelaxParam
        .map(x => Math.round(N * x).toInt)
        .map(x => (
          opKey + "(" + x.toString + ")",
          (sol: CPIntSol) => RelaxationFunctions.minMaxValRelax(solver, vars, sol, x))
        )

    case ALNSBuilder.CircuitSeqRelax =>
      ALNSBuilder.DefRelaxParam
        .map(x => Math.round(N * x).toInt)
        .map(x => (
          opKey + "(" + x.toString + ")",
          (sol: CPIntSol) => RelaxationFunctions.predRelaxSeqFixed(solver, vars, sol, x))
        )

    case ALNSBuilder.CircuitKoptRelax =>
      ALNSBuilder.DefRelaxParam
        .map(x => Math.round(N * x).toInt / 2)
        .map(x => (
          opKey + "(" + x.toString + ")",
          (sol: CPIntSol) => RelaxationFunctions.predRelaxKopt(solver, vars, sol, x))
        )

    case ALNSBuilder.ValWindowRelax =>
      for(
        lr <- ALNSBuilder.DefValWindowParam1;
        ur <- ALNSBuilder.DefValWindowParam2
      )yield (
        opKey + "(" + lr.toString + "," + ur.toString + ")",
        (sol: CPIntSol) => RelaxationFunctions.valWindowRelax(solver, vars, sol, lr, ur)
      )

    case ALNSBuilder.FullRelax => Array((opKey, _ => Unit))
  }

  private def instantiateSearchFunctions(opKey: String): Array[(String, CPIntSol => Unit)] = {
    val functions = ArrayBuffer[(String, CPIntSol => Unit)]()
    ALNSBuilder.DefValHeuris.foreach(heuristic =>{
      functions += instantiateSearchFunction(opKey, heuristic, valLearn = false)
      if(config.valLearn && opKey != ALNSBuilder.WeightDegSearch) functions += instantiateSearchFunction(opKey, heuristic, valLearn = true)
    })
    functions.toArray
  }

  private def instantiateSearchFunction(opKey: String, valHeuristic: String, valLearn: Boolean): (String, CPIntSol => Unit) = {
    val opName = opKey + (if(valLearn && opKey != ALNSBuilder.WeightDegSearch) "(valLearn" else "(") + valHeuristic + ")"
    (opName, _ => opKey match{
      case ALNSBuilder.ConfOrderSearch => SearchFunctions.conflictOrdering(vars, valHeuristic, valLearn)
      case ALNSBuilder.FirstFailSearch => SearchFunctions.firstFail(vars, valHeuristic, valLearn)
      case ALNSBuilder.LastConfSearch => SearchFunctions.lastConflict(vars, valHeuristic, valLearn)
      case ALNSBuilder.BinSplitSearch => SearchFunctions.binarySplit(vars, valHeuristic, valLearn)
      case ALNSBuilder.ExtOrientedSearch => SearchFunctions.extensionalOriented(vars, valHeuristic, valLearn)
      case ALNSBuilder.WeightDegSearch => SearchFunctions.weightedDegree(vars, valHeuristic, ALNSBuilder.DefWeigDegreeParam2)
    })
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

    case ALNSBuilder.RandomRelax => new ALNSSingleParamOperator[Int](
      ALNSBuilder.RandomRelax,
      ALNSBuilder.DefWithParamFailThreshold,
      (param: Int) => ((sol:CPIntSol) => RelaxationFunctions.randomRelax(solver, vars, sol: CPIntSol, param: Int), None, None),
      instantiateAdaptiveStore(
        paramSelectKey,
        ALNSBuilder.DefRelaxParam
          .map(x => Math.round(N * x).toInt)
          .map(x => new ALNSParameter[Int](x, if(config.opDeactivation) ALNSBuilder.DefNoParamFailThreshold else 0)),
        paramMetricKey
      ),
      instantiateMetric(paramMetricKey)
    )

    case ALNSBuilder.KSuccessiveRelax => new ALNSSingleParamOperator[Int](
      ALNSBuilder.KSuccessiveRelax,
      ALNSBuilder.DefWithParamFailThreshold,
      (param: Int) => ((sol:CPIntSol) => RelaxationFunctions.successiveRelax(solver, vars, sol: CPIntSol, param: Int), None, None),
      instantiateAdaptiveStore(
        paramSelectKey,
        ALNSBuilder.DefRelaxParam
          .map(x => Math.round(N * x).toInt)
          .map(x => new ALNSParameter[Int](x, if(config.opDeactivation) ALNSBuilder.DefNoParamFailThreshold else 0)),
        paramMetricKey
      ),
      instantiateMetric(paramMetricKey)
    )

    case ALNSBuilder.PropGuidedRelax => new ALNSSingleParamOperator[Double](
      ALNSBuilder.PropGuidedRelax,
      ALNSBuilder.DefWithParamFailThreshold,
      (param: Double) => ((sol:CPIntSol) => RelaxationFunctions.propagationGuidedRelax(solver, vars, sol: CPIntSol, closeness, param: Double), None, None),
      instantiateAdaptiveStore(
        paramSelectKey,
        ALNSBuilder.DefRelaxParam
          .map(x => x * maxNeighSize)
          .map(x => new ALNSParameter[Double](x, if(config.opDeactivation) ALNSBuilder.DefNoParamFailThreshold else 0)),
        paramMetricKey),
      instantiateMetric(paramMetricKey)
    )

    case ALNSBuilder.RevPropGuidedRelax => new ALNSSingleParamOperator[Double](
      ALNSBuilder.RevPropGuidedRelax,
      ALNSBuilder.DefWithParamFailThreshold,
      (param: Double) => ((sol:CPIntSol) => RelaxationFunctions.reversedPropagationGuidedRelax(solver, vars, sol: CPIntSol, closeness.get, param: Double), None, None),
      instantiateAdaptiveStore(
        paramSelectKey,
        ALNSBuilder.DefRelaxParam
          .map(x => x * maxNeighSize)
          .map(x => new ALNSParameter[Double](x, if(config.opDeactivation) ALNSBuilder.DefNoParamFailThreshold else 0)),
        paramMetricKey),
      instantiateMetric(paramMetricKey)
    )

    case ALNSBuilder.RandomValGroupsRelax => new ALNSSingleParamOperator[Int](
      ALNSBuilder.RandomValGroupsRelax,
      ALNSBuilder.DefWithParamFailThreshold,
      (param: Int) => ((sol:CPIntSol) => RelaxationFunctions.randomGroupsRelax(solver, vars, sol: CPIntSol, param: Int), None, None),
      instantiateAdaptiveStore(
        paramSelectKey,
        ALNSBuilder.DefRelaxParam
          .map(x => Math.round(N * x).toInt)
          .map(x => new ALNSParameter[Int](x, if(config.opDeactivation) ALNSBuilder.DefNoParamFailThreshold else 0)),
        paramMetricKey
      ),
      instantiateMetric(paramMetricKey)
    )

    case ALNSBuilder.MinValGroupsRelax => new ALNSSingleParamOperator[Int](
      ALNSBuilder.MinValGroupsRelax,
      ALNSBuilder.DefWithParamFailThreshold,
      (param: Int) => ((sol:CPIntSol) => RelaxationFunctions.minGroupsRelax(solver, vars, sol: CPIntSol, param: Int), None, None),
      instantiateAdaptiveStore(
        paramSelectKey,
        ALNSBuilder.DefRelaxParam
          .map(x => Math.round(N * x).toInt)
          .map(x => new ALNSParameter[Int](x, if(config.opDeactivation) ALNSBuilder.DefNoParamFailThreshold else 0)),
        paramMetricKey
      ),
      instantiateMetric(paramMetricKey)
    )

    case ALNSBuilder.MaxValGroupsRelax => new ALNSSingleParamOperator[Int](
      ALNSBuilder.MaxValGroupsRelax,
      ALNSBuilder.DefWithParamFailThreshold,
      (param: Int) => ((sol:CPIntSol) => RelaxationFunctions.maxGroupsRelax(solver, vars, sol: CPIntSol, param: Int), None, None),
      instantiateAdaptiveStore(
        paramSelectKey,
        ALNSBuilder.DefRelaxParam
          .map(x => Math.round(N * x).toInt)
          .map(x => new ALNSParameter[Int](x, if(config.opDeactivation) ALNSBuilder.DefNoParamFailThreshold else 0)),
        paramMetricKey
      ),
      instantiateMetric(paramMetricKey)
    )

    case ALNSBuilder.MinValRelax => new ALNSSingleParamOperator[Int](
      ALNSBuilder.MinValRelax,
      ALNSBuilder.DefWithParamFailThreshold,
      (param: Int) => ((sol:CPIntSol) => RelaxationFunctions.minValRelax(solver, vars, sol: CPIntSol, param: Int), None, None),
      instantiateAdaptiveStore(
        paramSelectKey,
        ALNSBuilder.DefRelaxParam
          .map(x => Math.round(N * x).toInt)
          .map(x => new ALNSParameter[Int](x, if(config.opDeactivation) ALNSBuilder.DefNoParamFailThreshold else 0)),
        paramMetricKey
      ),
      instantiateMetric(paramMetricKey)
    )

    case ALNSBuilder.MaxValRelax => new ALNSSingleParamOperator[Int](
      ALNSBuilder.MaxValRelax,
      ALNSBuilder.DefWithParamFailThreshold,
      (param: Int) => ((sol:CPIntSol) => RelaxationFunctions.maxValRelax(solver, vars, sol: CPIntSol, param: Int), None, None),
      instantiateAdaptiveStore(
        paramSelectKey,
        ALNSBuilder.DefRelaxParam
          .map(x => Math.round(N * x).toInt)
          .map(x => new ALNSParameter[Int](x, if(config.opDeactivation) ALNSBuilder.DefNoParamFailThreshold else 0)),
        paramMetricKey
      ),
      instantiateMetric(paramMetricKey)
    )

    case ALNSBuilder.MinMaxValRelax => new ALNSSingleParamOperator[Int](
      ALNSBuilder.MinMaxValRelax,
      ALNSBuilder.DefWithParamFailThreshold,
      (param: Int) => ((sol:CPIntSol) => RelaxationFunctions.minMaxValRelax(solver, vars, sol: CPIntSol, param: Int), None, None),
      instantiateAdaptiveStore(
        paramSelectKey,
        ALNSBuilder.DefRelaxParam
          .map(x => Math.round(N * x).toInt)
          .map(x => new ALNSParameter[Int](x, if(config.opDeactivation) ALNSBuilder.DefNoParamFailThreshold else 0)),
        paramMetricKey
      ),
      instantiateMetric(paramMetricKey)
    )

    case ALNSBuilder.CircuitSeqRelax => new ALNSSingleParamOperator[Int](
      ALNSBuilder.CircuitSeqRelax,
      ALNSBuilder.DefWithParamFailThreshold,
      (param: Int) => ((sol:CPIntSol) => RelaxationFunctions.predRelaxSeqFixed(solver, vars, sol: CPIntSol, param: Int), None, None),
      instantiateAdaptiveStore(
        paramSelectKey,
        ALNSBuilder.DefRelaxParam
          .map(x => Math.round(N * x).toInt)
          .map(x => new ALNSParameter[Int](x, if(config.opDeactivation) ALNSBuilder.DefNoParamFailThreshold else 0)),
        paramMetricKey
      ),
      instantiateMetric(paramMetricKey)
    )

    case ALNSBuilder.CircuitKoptRelax => new ALNSSingleParamOperator[Int](
      ALNSBuilder.CircuitKoptRelax,
      ALNSBuilder.DefWithParamFailThreshold,
      (param: Int) => ((sol:CPIntSol) => RelaxationFunctions.predRelaxKopt(solver, vars, sol: CPIntSol, param: Int), None, None),
      instantiateAdaptiveStore(
        paramSelectKey,
        ALNSBuilder.DefRelaxParam
          .map(x => Math.round(N * x).toInt / 2)
          .map(x => new ALNSParameter[Int](x, if(config.opDeactivation) ALNSBuilder.DefNoParamFailThreshold else 0)),
        paramMetricKey
      ),
      instantiateMetric(paramMetricKey)
    )

    case ALNSBuilder.ValWindowRelax => new ALNSTwoParamsOperator[Double, Double](
      ALNSBuilder.ValWindowRelax,
      ALNSBuilder.DefWithParamFailThreshold,
      (param1: Double, param2: Double) => ((sol:CPIntSol) => RelaxationFunctions.valWindowRelax(solver, vars, sol: CPIntSol, param1: Double, param2:Double), None, None),
      instantiateAdaptiveStore(
        paramSelectKey,
        ALNSBuilder.DefValWindowParam1.map(x => new ALNSParameter[Double](x, if(config.opDeactivation) ALNSBuilder.DefNoParamFailThreshold else 0)),
        paramMetricKey
      ),
      instantiateAdaptiveStore(
        paramSelectKey,
        ALNSBuilder.DefValWindowParam2.map(x => new ALNSParameter[Double](x, if(config.opDeactivation) ALNSBuilder.DefNoParamFailThreshold else 0)),
        paramMetricKey
      ),
      instantiateMetric(paramMetricKey)
    )

    case ALNSBuilder.FullRelax => new ALNSNoParamOperator(
      ALNSBuilder.FullRelax,
      if(config.opDeactivation) ALNSBuilder.DefNoParamFailThreshold else 0,
      () => (_ => Unit, None, None)
    )
  }

  /**
    * Instantiate loose ANLS search operators for the given keys.
    * @param opKey The operator key
    * @return an array of ALNSOperator objects
    */
  private def instantiateSearchOperators(
                                          opKey: String,
                                          paramSelectKey: String = ALNSBuilder.DefParamSelectKey,
                                          paramMetricKey: String = ALNSBuilder.DefParamMetricKey
                                        ): Array[ALNSOperator] =
    instantiateSearchFunctions(opKey).map{
      case (name, function) => new ALNSTwoParamsOperator[Int, Int](
        name,
        if(config.opDeactivation) ALNSBuilder.DefNoParamFailThreshold else 0,
        (param1: Int, param2: Int) => (function, Some(param1), Some(param2)),
        instantiateAdaptiveStore(
          paramSelectKey,
          ALNSBuilder.DefNFailures
            .map(x => new ALNSParameter[Int](x, if(config.opDeactivation) ALNSBuilder.DefNoParamFailThreshold else 0)),
          paramMetricKey
        ),
        instantiateAdaptiveStore(
          paramSelectKey,
          ALNSBuilder.DefMaxDiscrepancy
            .map(x => new ALNSParameter[Int](x, if(config.opDeactivation) ALNSBuilder.DefNoParamFailThreshold else 0)),
          paramMetricKey
        ),
        instantiateMetric(paramMetricKey)
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
        case ALNSBuilder.LastImprov => 0.5
        case ALNSBuilder.LastImprovRatio => 0.5
        case ALNSBuilder.AvgImprov => 1.0
        case ALNSBuilder.AvgImprovRatio => 1.0
        case ALNSBuilder.TTI => 1.0
      },
      metricKey match{
        case ALNSBuilder.LastImprov => false
        case ALNSBuilder.LastImprovRatio => false
        case ALNSBuilder.AvgImprov => false
        case ALNSBuilder.AvgImprovRatio => false
        case ALNSBuilder.TTI => true
      }
    )

    case ALNSBuilder.Priority => new PriorityStore[T](
      elems,
      metricKey match{
        case ALNSBuilder.LastImprov => 0.5
        case ALNSBuilder.LastImprovRatio => 0.5
        case ALNSBuilder.AvgImprov => 1.0
        case ALNSBuilder.AvgImprovRatio => 1.0
        case ALNSBuilder.TTI => 1.0
      },
      metricKey match{
        case ALNSBuilder.LastImprov => false
        case ALNSBuilder.LastImprovRatio => false
        case ALNSBuilder.AvgImprov => false
        case ALNSBuilder.AvgImprovRatio => false
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
    case ALNSBuilder.LastImprovRatio => Metrics.lastImprovementRatio
    case ALNSBuilder.AvgImprov => Metrics.averageImprovement
    case ALNSBuilder.AvgImprovRatio => Metrics.averageImprovementRatio
    case ALNSBuilder.TTI => Metrics.timeToImprovement
  }
}
