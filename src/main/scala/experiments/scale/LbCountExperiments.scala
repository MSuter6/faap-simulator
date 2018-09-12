package experiments.scale

import experiments.ExperimentCommon._
import experiments.{DefaultExpRunner, ExperimentNaming, FaapProblemInstancesConfiguration}
import io.visualization.DefaultPlotWriterConfigs._
import simulator.FaapProblemInstanceRepetitions
import simulator.allocation.Solver
import simulator.generation._

import scala.collection.immutable

/** Experiment: Scale experiments, Impact of location-bound node count */
object LbCountExperiments extends App with LbCountExperiment {
  override val expName = "lbCount"

  val nodeUsageConfig = () => GraphGenBase(fogNodeConstraintGen = () => {
    val weight = GlobalConfig.random.nextDouble() * 2
    weight
  })

  runLbCountExp(nodeUsageConfig, 0 to 10 by 2)
}

/**
  * Experiment runner for location-bound node experiments.
  */
trait LbCountExperiment extends ExperimentNaming {
  protected def runLbCountExp(nodeUsageConfig: () => GraphGenBase,
                              xAxisRange: Range,
                              appGraphOrder: Int = defaultAppGraphOrder,
                              fogGraphOrder: Int = defaultFogGraphOrder,
                              repetitions: Int = defaultRepetitionCount,
                              lbRatio: Double = defaultLocationBoundMappingRatio): Unit = {
    def nodeCapGenerator(faapProblemInstancesConfiguration: FaapProblemInstancesConfiguration): () => immutable.IndexedSeq[FaapProblemInstanceRepetitions] = {

      () =>
        for (scale <- faapProblemInstancesConfiguration.xAxisRange) yield {
          val lbRatio = scale.toDouble / 10

          val nodeGeneratorBase = nodeUsageConfig()

          val appGraphGenerator = defaultAppGraphType(SeriesParallelDecomposableGraphGenConfig(appGraphOrder, graphGenBase = nodeGeneratorBase))
          val fogGraphGenerator = defaultFogGraphType(CactusGraphGenConfig(fogGraphOrder, graphGenBase = nodeGeneratorBase))
          val locationBoundMappingGenerator = DefaultLocationBoundMappingGenerator(LocationBoundMappingConfig(lbRatio))

          val description = f"SPDG2Cac_cap$lbRatio%1.2f_a${appGraphOrder}_f$fogGraphOrder"
          val generator = FaapProblemGenerator(appGraphGenerator, fogGraphGenerator, locationBoundMappingGenerator, None, description = description, lbRatio.toString, scale)
          generator.generateProblemInstances(faapProblemInstancesConfiguration.repetitions)
        }
    }

    val faapProblemInstancesConfiguration = FaapProblemInstancesConfiguration(xAxisRange, repetitions)
    val graphGenerator = nodeCapGenerator(faapProblemInstancesConfiguration)

    val pathHandler = getPathHandler(expType)
    val allocators = defaultAllocators(pathHandler, Solver.Gurobi)

    DefaultExpRunner(faapProblemInstancesConfiguration,
      graphGenerator,
      allocators,
      pathHandler,
      allLinePlots joining allBoxPlots
    )
  }
}