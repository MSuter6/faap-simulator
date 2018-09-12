package experiments.various

import experiments.{DefaultFixedOrderRatioRunner, ExperimentNaming, FaapProblemInstancesConfiguration}
import io.solver.ZimplWriter
import simulator.allocation._

/** Experiment using the linear program relaxation (results not shown in thesis due to
  * very poor performance. */
object LpRoundingExps extends App with ExperimentNaming {
  override val expName = "lpRounding"

  private val pathHandler = getPathHandler(expType)

  val allocators = List(
    OptimalAllocator(SolverAllocatorConfig(pathHandler, Solver.SCIP, 500, ZimplWriter.lp())),
    GreedyBorderAllocator)
  DefaultFixedOrderRatioRunner(FaapProblemInstancesConfiguration(5 to 500 by 70, 10), allocators, pathHandler = pathHandler)
}

/**
  * Experiment which studies the linear programming relaxation with the solver compared to the heuristics)
  */
object SolverHeuristicsExps extends App with ExperimentNaming {
  override val expName = "lpRoundingWithHeuristics"

  private val pathHandler = getPathHandler(expType)

  val allocators = List(OptimalAllocator(SolverAllocatorConfig(pathHandler, Solver.SCIP, 10)),
    GreedyBorderAllocator
  )

  DefaultFixedOrderRatioRunner(FaapProblemInstancesConfiguration(5 to 50 by 10, 3), allocators, pathHandler = pathHandler)
}