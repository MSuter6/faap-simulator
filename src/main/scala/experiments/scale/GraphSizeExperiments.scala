package experiments.scale
import experiments.ExperimentCommon._
import experiments.{DefaultExpRunner, ExperimentNaming, FaapProblemInstancesConfiguration}
import io.PathHandler
import io.visualization.DefaultPlotWriterConfigs._
import simulator.FaapProblemInstanceRepetitions
import simulator.allocation._
import simulator.generation._

/** Experiment: Scale experiments, Impact of fog graph density */
object FogEdgeCountExperimentsSolver extends App with ExperimentNaming {
  val expName = "fogDensity"
  val pathHandler = getPathHandler(expType)
  EdgeCountExperiments.runEdgeCountExperiment(expName, pathHandler = pathHandler, allocators = defaultAllocators(pathHandler, Solver.Gurobi))
}

/**
  * Experiment runner for graph density experiments.
  */
object EdgeCountExperiments {
  def runEdgeCountExperiment(expName: String,
                             scaleRange: Range = 0 to 10 by 2,
                             appGraphOrder: Int = defaultAppGraphOrder,
                             fogGraphOrder: Int = defaultFogGraphOrder,
                             pathHandler: PathHandler,
                             allocators: Seq[Allocator],
                             fogIsSparsenessGraph: Boolean = true,
                             repetitionCount: Int = defaultRepetitionCount): Unit = {

    def nodeCapGenerator(faapProblemInstancesConfiguration: FaapProblemInstancesConfiguration,
                         lbRatio: Double = 0.1): () => IndexedSeq[FaapProblemInstanceRepetitions] = {
      () =>
        for (scale <- faapProblemInstancesConfiguration.xAxisRange) yield {
          val percentage = scale.toDouble / 10

          val (appGraphGenerator: ApplicationGraphGenerator, fogGraphGenerator: FogGraphGenerator) = if (fogIsSparsenessGraph) {
            val appGraphGenerator = SpdgApplicationGraphGenerator(SeriesParallelDecomposableGraphGenConfig(appGraphOrder))
            val fogGraphGenerator = ConnectedSparsenessFogGraphGenerator(ConnectedDensityGraphGenConfig(fogGraphOrder, percentage))
            (appGraphGenerator, fogGraphGenerator)
          } else {
            val appGraphGenerator = ConnectedSparsenessApplicationGraphGenerator(ConnectedDensityGraphGenConfig(appGraphOrder, percentage))
            val fogGraphGenerator = CactusFogGraphGenerator(CactusGraphGenConfig(fogGraphOrder))
            (appGraphGenerator, fogGraphGenerator)
          }
          val locationBoundMappingGenerator = DefaultLocationBoundMappingGenerator(LocationBoundMappingConfig(lbRatio))

          val generator = FaapProblemGenerator(appGraphGenerator, fogGraphGenerator, locationBoundMappingGenerator, None, description = f"SPDG2Cac_e${percentage}_a${appGraphOrder}_f$fogGraphOrder", percentage.toString, scale)
          generator.generateProblemInstances(faapProblemInstancesConfiguration.repetitions)
        }
    }

    val faapProblemInstancesConfiguration = FaapProblemInstancesConfiguration(scaleRange, repetitionCount)
    val graphGenerator = nodeCapGenerator(faapProblemInstancesConfiguration)

    DefaultExpRunner(faapProblemInstancesConfiguration,
      graphGenerator,
      allocators,
      pathHandler,
      allLinePlots joining allBoxPlots
    )
  }
}
