package experiments.scale

import experiments.ExperimentCommon._
import experiments.{DefaultExpRunner, DefaultFixedOrderRatioRunner, ExperimentNaming, FaapProblemInstancesConfiguration}
import io.visualization.BoxPlotPlotterPerfGuarnt
import io.visualization.DefaultPlotWriterConfigs._
import io.visualization.PlotUtils.YValueExtractors
import simulator.allocation._
import simulator.generation.FixedComponentFaapProblemGenerator.{FixAppGraphOrderFaapProblemGenerator, FixFogGraphOrderFaapProblemGenerator}

/**
  * Experiments comparing studying the impact of the graph order (Thesis 4.3.1.1).
  */

/** Experiment: Constant Ratio (large scale) */
object FixedRatioExperiment extends App with ExperimentNaming {
  override val expName = "constRatio"

  val pathHandler = getPathHandler(expType)

  val faapProblemInstancesConfiguration = FaapProblemInstancesConfiguration(4 to 504 by 50)
  val appToFogNodeRatio = 2

  val plotWriterConfig = DefaultPlotWriterConfig joining allLinePlots
  DefaultFixedOrderRatioRunner(faapProblemInstancesConfiguration, heuristics,
    appToFogNodeRatio, pathHandler, plotWriterConfig)
}

/** Experiment: Constant Ratio performance guarantee/ratio */
object ApproximationGuarantee extends App with ExperimentNaming {
  override val expName = "ApprxGuartn"

  val pathHandler = getPathHandler(expType)

  val faapProblemInstancesConfiguration = FaapProblemInstancesConfiguration(4 to 6 by 2, 5)
  val appToFogNodeRatio = 2

  val plotWriterConfig = DefaultPlotWriterConfig joining allLinePlots :+ (YValueExtractors.costExtractor, BoxPlotPlotterPerfGuarnt)
  DefaultFixedOrderRatioRunner(faapProblemInstancesConfiguration, defaultAllocators(pathHandler, Solver.SCIP),
    appToFogNodeRatio, pathHandler, plotWriterConfig)
}

/** Experiment: Constant Ratio (small scale) */
object FixedRatioSmallScaleExperiment extends App with ExperimentNaming {
  override val expName = "constRatioSmallScale"

  val pathHandler = getPathHandler(expType)

  val faapProblemInstancesConfiguration = FaapProblemInstancesConfiguration(4 to 12 by 2)
  val appToFogNodeRatio = 2

  val plotWriterConfig = DefaultPlotWriterConfig joining allLinePlots
  DefaultFixedOrderRatioRunner(faapProblemInstancesConfiguration, defaultAllocators(pathHandler, Solver.Gurobi),
    appToFogNodeRatio, pathHandler, plotWriterConfig)
}

/** Experiment: Impact of fog graph order */
object FixedAppGraphOrderExperiments extends App with ExperimentNaming {
  override val expName = "constAppGraphOrder"
  private val pathHandler = getPathHandler(expType)

  val faapProblemInstancesConfiguration = FaapProblemInstancesConfiguration(26 to 26 by 3)
  val allocators = defaultAllocators(pathHandler, Solver.Gurobi)

  private val faapProblemInstancesGen = FixAppGraphOrderFaapProblemGenerator(faapProblemInstancesConfiguration)

  DefaultExpRunner(faapProblemInstancesConfiguration, faapProblemInstancesGen, allocators, pathHandler)
}

/** Experiment: Impact of app graph order */
object FixedFogGraphOrderExperiments extends App with ExperimentNaming {
  override val expName = "constFogGraphOrder"
  private val pathHandler = getPathHandler(expType)

  val faapProblemInstancesConfiguration = FaapProblemInstancesConfiguration(26, 100)
  val allocators = defaultAllocators(pathHandler, Solver.Gurobi)

  private val faapProblemInstancesGen = FixFogGraphOrderFaapProblemGenerator(faapProblemInstancesConfiguration)

  DefaultExpRunner(faapProblemInstancesConfiguration, faapProblemInstancesGen, allocators, pathHandler)
}

/** Experiment: 4.3.5 Improved Heuristic */
object ConstRatioCombinedHeurExperiment extends App with ExperimentNaming {
  override val expName = "constRatioCombHeuristics"

  val pathHandler = getPathHandler(expType)

  val faapProblemInstancesConfiguration = FaapProblemInstancesConfiguration(4 to 204 by 50)
  val appToFogNodeRatio = 2

  val plotWriterConfig = DefaultPlotWriterConfig joining allLinePlots
  DefaultFixedOrderRatioRunner(faapProblemInstancesConfiguration, heuristics :+ CombinedAllocator,
    appToFogNodeRatio, pathHandler, plotWriterConfig)
}
