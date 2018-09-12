package experiments.graphtypes

import Utils.atLeast
import experiments.{DefaultExpRunner, ExperimentNaming, FaapProblemInstancesConfiguration}
import simulator.FaapProblemInstanceRepetitions
import simulator.allocation._
import simulator.generation._

/**
  * Experiment definitions with ABB Control application (4.3.2)
  */

/** Experiment: Optimally solvable instance  */
object ABBControlToCactusExperiments extends App with ExperimentNaming {
  override def expName = "abbControlAppToCactus"

  val pathHandler = getPathHandler(expType)
  val faapProblemInstancesConfiguration = FaapProblemInstancesConfiguration(2 to 27 by 5)
  import experiments.ExperimentCommon._

  val allocators = defaultAllocators(pathHandler)

  private val faapProblemInstancesGen = ABBControlToCactusExperimentsFaapProblemGenerator.abbControlConstantRatioGenerator(faapProblemInstancesConfiguration)
  DefaultExpRunner(faapProblemInstancesConfiguration, faapProblemInstancesGen, allocators, pathHandler)
}

/** Experiment: Large scale instances */
object ABBControlToCactusExperimentLargeScale extends App with ExperimentNaming {
  override def expName = "abbControlAppToCactusLargeScale"

  val pathHandler = getPathHandler(expType)
  val faapProblemInstancesConfiguration = FaapProblemInstancesConfiguration(2 to 500 by 71)

  val allocators = Seq(NodeConstraintsAllocator, GreedyBorderAllocator, GreedyCollocationAllocator)

  private val faapProblemInstancesGen = ABBControlToCactusExperimentsFaapProblemGenerator.abbControlConstantRatioGenerator(faapProblemInstancesConfiguration)
  DefaultExpRunner(faapProblemInstancesConfiguration, faapProblemInstancesGen, allocators, pathHandler)
}

/** Experiment: SubGraphILP, breaks down ABB app into subgraphs (not in thesis)*/
object ABBSubGraphILPExperiments extends App with ExperimentNaming {
  override def expName = "abbUCSubgraphILP"

  val pathHandler = getPathHandler(expType)
  val faapProblemInstancesConfiguration = FaapProblemInstancesConfiguration(2 to 27 by 5, 10)

  val allocators = Seq(NodeConstraintsAllocator, GreedyBorderAllocator, GreedyCollocationAllocator, SubgraphMIPAllocator(pathHandler))

  private val faapProblemInstancesGen = ABBControlToCactusExperimentsFaapProblemGenerator.abbControlConstantRatioGenerator(faapProblemInstancesConfiguration)
  DefaultExpRunner(faapProblemInstancesConfiguration, faapProblemInstancesGen, allocators, pathHandler)
}

/** Generator of FAAP instances with the ABB control app */
object ABBControlToCactusExperimentsFaapProblemGenerator {
  def abbControlConstantRatioGenerator(faapProblemInstancesConfiguration: FaapProblemInstancesConfiguration,
                                       massRatio: Int = 2): () => IndexedSeq[FaapProblemInstanceRepetitions] = {
    () =>
      for (n <- faapProblemInstancesConfiguration.xAxisRange) yield {
        val appGraphConfig = ABBControlApplicationGraphGenConfig(n, wcet = true)

        val graphGenBase = GraphGenBase(fogNodeConstraintGen = () => ABBControlApplicationGraphGenConfig.generateFogNodeWeight())
        val fogGraphOrder = {
          atLeast(math.ceil(appGraphConfig.expectedTotalAppNodeUsage / appGraphConfig.expectedAverageFogNodeCapacity * massRatio).toInt,
            2)
        }

        println(appGraphConfig.expectedAverageFogNodeCapacity)
        println(appGraphConfig.expectedTotalAppNodeUsage)
        println(fogGraphOrder)

        val fogGraphConfig = ABBControlAppFogGraphGenConfig(fogGraphOrder.toInt, n, graphGenBase = graphGenBase)

        val appGraphGenerator = ABBControlApplicationGraphGenerator(appGraphConfig)
        val fogGraphGenerator = ABBControlAppFogGraphGenerator(fogGraphConfig)
        val locationBoundMappingGenerator = ABBControlAppLocationBoundMappingGenerator()

        val generator = FaapProblemGenerator(appGraphGenerator,
          fogGraphGenerator,
          locationBoundMappingGenerator,
          description = s"ABBCtrl2Cac_n$n", id = n.toString + "_" + fogGraphOrder, order = n)
        generator.generateProblemInstances(faapProblemInstancesConfiguration.repetitions)
      }
  }
}
