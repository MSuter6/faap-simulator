package experiments.graphtypes

import experiments.ExperimentCommon._
import experiments.{DefaultExpRunner, ExperimentNaming, FaapProblemInstancesConfiguration}
import simulator.FaapProblemInstanceRepetitions
import simulator.generation._

/**
  * Experiments with map-reduce app graphs (not in thesis).
  */
object AppMapReduceToCactusExperiment extends App with ExperimentNaming {
  override def expName = "mapReduceToCactus"

  val pathHandler = getPathHandler(expType)
  val faapProblemInstancesConfiguration = FaapProblemInstancesConfiguration(7 to 13 by 2, 3)

  val allocators = defaultAllocators(pathHandler)

  private val faapProblemInstancesGen = MapReduceFaapProblemGenerator.mapReduceConstantRatioGenerator(faapProblemInstancesConfiguration)
  DefaultExpRunner(faapProblemInstancesConfiguration, faapProblemInstancesGen, allocators, pathHandler)
}

object MapReduceFaapProblemGenerator {
  def mapReduceConstantRatioGenerator(faapProblemInstancesConfiguration: FaapProblemInstancesConfiguration, nodeCountRatio: Int = 2): () => IndexedSeq[FaapProblemInstanceRepetitions] = {
    () =>
      for (fogGraphOrder <- faapProblemInstancesConfiguration.xAxisRange) yield {
        val appGraphOrder = fogGraphOrder * nodeCountRatio

        val appGraphGenerator = MapReduceApplicationGraphGenerator(MapReduceGenConfig(appGraphOrder))
        val fogGraphGenerator = CactusFogGraphGenerator(CactusGraphGenConfig(fogGraphOrder))
        val locationBoundMappingGenerator = SourceSinkLocationBoundMappingGenerator()

        val generator = FaapProblemGenerator(appGraphGenerator,
          fogGraphGenerator,
          locationBoundMappingGenerator,
          description = s"MapRed2Cac_fn$fogGraphOrder", id = fogGraphOrder.toString, order = fogGraphOrder)
        generator.generateProblemInstances(faapProblemInstancesConfiguration.repetitions)
      }
  }
}
