package experiments.scale

import experiments.ExperimentCommon._
import experiments.{DefaultExpRunner, ExperimentNaming, FaapProblemInstancesConfiguration}
import io.visualization.DefaultPlotWriterConfigs._
import simulator.FaapProblemInstanceRepetitions
import simulator.allocation.Solver
import simulator.generation._

import scala.collection.immutable

/** Experiment: Scale experiments, Impact of app graph resource requirements */
object AppNodeCap extends App with NodeCapExperiments {
  override val expName = "appNodeCap"

  val nodeUsageConfig = (max: Double) => GraphGenBase(appNodeConstraintGen = () => {
    val weight = GlobalConfig.random.nextDouble() * max
    weight
  })

  runNodeCapExp(nodeUsageConfig, 1 to 12 by 2, lbRatio = 0.0)
}

/** Experiment: Scale experiments, Impact of fog graph capacity */
object FogNodeCap extends App with NodeCapExperiments {
  override val expName = "fogNodeCap"
  GlobalConfig.random.nextDouble()
  val nodeUsageConfig = (max: Double) => GraphGenBase(fogNodeConstraintGen = () => {
    val weight = GlobalConfig.random.nextDouble() * max
    weight
  })

  runNodeCapExp(nodeUsageConfig, 1 to 31 by 5, lbRatio = 0.0)
}

/**
  * Experiment runner of node resource experiments.
  */
trait NodeCapExperiments extends ExperimentNaming {
  protected def runNodeCapExp(nodeUsageConfig: Double => GraphGenBase,
                              xAxisRange: Range,
                              appGraphOrder: Int = defaultAppGraphOrder,
                              fogGraphOrder: Int = defaultFogGraphOrder,
                              repetitions: Int = defaultRepetitionCount,
                              lbRatio: Double = defaultLocationBoundMappingRatio): Unit = {
    def nodeCapGenerator(faapProblemInstancesConfiguration: FaapProblemInstancesConfiguration): () => immutable.IndexedSeq[FaapProblemInstanceRepetitions] = {

      () =>
        for (scale <- faapProblemInstancesConfiguration.xAxisRange) yield {
          val max = scale.toDouble / 10

          val nodeGeneratorBase = nodeUsageConfig(max)

          val appGraphGenerator = defaultAppGraphType(SeriesParallelDecomposableGraphGenConfig(appGraphOrder, graphGenBase = nodeGeneratorBase))
          val fogGraphGenerator = defaultFogGraphType(CactusGraphGenConfig(fogGraphOrder, graphGenBase = nodeGeneratorBase))
          val locationBoundMappingGenerator = DefaultLocationBoundMappingGenerator(LocationBoundMappingConfig(lbRatio))

          val description = f"SPDG2Cac_cap$max%1.2f_a${appGraphOrder}_f$fogGraphOrder"
          val generator = FaapProblemGenerator(appGraphGenerator, fogGraphGenerator, locationBoundMappingGenerator, None, description = description, max.toString, scale)
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