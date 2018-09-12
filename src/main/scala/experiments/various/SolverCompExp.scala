package experiments.various

import experiments.{DefaultFixedOrderRatioRunner, ExperimentNaming, FaapProblemInstancesConfiguration}
import simulator.allocation.{OptimalAllocator, Solver, SolverAllocatorConfig}

/** Experiment: 4.3.4 Solver Comparison */
object SolverCompExp extends App with ExperimentNaming {
  override val expName = "solverComparison"

  val pathHandler = getPathHandler(expType)

  private val allocators = Seq(OptimalAllocator(SolverAllocatorConfig(pathHandler, Solver.Gurobi)), OptimalAllocator(SolverAllocatorConfig(pathHandler, Solver.SCIP)))

  DefaultFixedOrderRatioRunner(FaapProblemInstancesConfiguration(4 to 10 by 2), allocators, pathHandler = pathHandler)

}
