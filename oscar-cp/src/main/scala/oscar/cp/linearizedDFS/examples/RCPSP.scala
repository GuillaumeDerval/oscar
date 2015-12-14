package oscar.cp.linearizedDFS.examples

import oscar.cp.constraints.{CPObjective, CPObjectiveUnitMinimize}
import oscar.cp.scheduling.constraints.SweepMaxCumulative
import oscar.cp.scheduling.constraints.EnergeticReasoning
import oscar.cp.linearizedDFS.{DFSLinearizerSearch, DFSReplayer, Tracking}
import oscar.cp.linearizedDFS.branching.SetTimesBranchingToReplay
import oscar.cp._
import oscar.cp.scheduling.constraints.EnergeticChecker

/**
 * Created by saschavancauwelaert on 15/12/14.
 */
object RCPSP extends CPModel with App {


  // Model
  // -----------------------------------------------------------------------

  // (duration, consumption)
  val instance = Array((5, 1), (3, 1), (9, 3), (1, 2), (2, 2), (8, 1), (3, 2), (2, 2), (2, 1), (1, 1), (1, 2))
  val durationsData = instance.map(_._1)
  val demandsData = instance.map(_._2)
  val capa = CPIntVar(4)
  val nTasks = instance.size
  val Tasks = 0 until nTasks
  val resourceid = 1

  val horizon = durationsData.sum
  implicit val cp = CPSolver()

  val durations = Array.tabulate(nTasks)(t => CPIntVar(durationsData(t)))
  //THE DECISION VARIABLES MUST HAVE DIFFERENT NON-EMPTY NAMES
  val starts = Array.tabulate(nTasks)(t => CPIntVar(0 to horizon - durations(t).min,"starts"+t))
  val ends = Array.tabulate(nTasks)(t => starts(t) + durations(t))
  val demands = Array.tabulate(nTasks)(t => CPIntVar(demandsData))
  val resources = Array.tabulate(nTasks)(t => CPIntVar(resourceid))

  val taskIds = 0 until nTasks



  // Constraints
  // -----------------------------------------------------------------------

  val makespan = maximum(ends)

  // Consistency
  for (t <- taskIds)
    cp.add(ends(t) == starts(t) + durations(t))

  // Cumulative
  val tt = new SweepMaxCumulative(starts, durations, ends, demands, resources, capa, resourceid)
  val erch = new EnergeticChecker(starts, durations, ends, demands, resources, capa, resourceid)
  val erTracked = new {val decisionVariables = (starts ++ ends).toSeq } with EnergeticReasoning(starts, durations, ends, demands, resources, capa, resourceid) with Tracking

  erTracked.deactivate()
  add(tt)
  add(erch)

  val cpObj = new CPObjectiveUnitMinimize(makespan)
  val obj = new CPObjective(cp, cpObj)
  cp.optimize(obj)



  // search
  // -----------------------------------------------------------------------

  //val br = new BinaryStaticOrderBranching(starts)
  //val br = new BinaryFirstFailBranching(starts)
  val br = new SetTimesBranchingToReplay(starts,durations,ends)

  val linearizingSearch = new DFSLinearizerSearch(cp)

  /*
   * save a linearized DFS with chunks of decisions with a maximum size of (about) 10 in /tmp
   * if you want to keep the linearized DFS in RAM, use None instead of Some(dirURL)
   */
  val (nSolsTracked, nBacktracksTracked, nNodesTracked) = linearizingSearch.start(br, false, Some("/tmp/"),10)

  val replayer = new DFSReplayer(cp,starts ++ ends)

  // REPLAY WITHOUT
  // -----------------------------------------------------------------------
  cpObj.relax()

  //if to be replayed from RAM, get the decision list from the DFSLinearizerSearch object
  val (timeWithout, nSolsWithout, nBacktracksWithout, nNodesWithout) :  (Long,Int,Int,Int) = replayer.replay("/tmp/")
  assert(nSolsTracked == nSolsWithout)
  assert(nBacktracksTracked == nBacktracksWithout)
  assert(nNodesTracked + 1 == nNodesWithout)

  println((timeWithout, nSolsWithout, nBacktracksWithout, nNodesWithout))

  // REPLAY WITH
  // -----------------------------------------------------------------------
  cpObj.relax()

  erTracked.activate()
  add(erTracked)

  val (timeWith, nSolsWith, nBacktracksWith, nNodesWith) : (Long,Int,Int,Int)  = replayer.replay("/tmp/")
  assert(nSolsTracked == nSolsWith)
  assert(nBacktracksTracked >= nBacktracksWith)
  assert(nNodesTracked + 1 >= nNodesWith)

  println((timeWith, nSolsWith, nBacktracksWith, nNodesWith))

  println(erTracked.numUsefulCalls, erTracked.timeSpentOnUsefulCalls, erTracked.numUselessCalls, erTracked.timeSpentOnUselessCalls)


}
