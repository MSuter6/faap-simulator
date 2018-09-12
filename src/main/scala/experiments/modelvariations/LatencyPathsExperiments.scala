package experiments.modelvariations

import experiments.{DefaultExpRunner, ExperimentCommon, ExperimentNaming, FaapProblemInstancesConfiguration}
import simulator.{FaapProblemInstance, FaapProblemInstanceRepetitions}
import simulator.allocation.{OptimalAllocator, Solver, SolverAllocatorConfig}
import simulator.generation._

/**
  * Experiment with growing number of latency paths compared to no latency paths (Thesis 4.3.3).
  */
object LatencyPathsCountCompareExperiment extends App with ExperimentNaming {
  override val expName = "lpCountSolverComp"
  private val pathHandler = getPathHandler(expType)
  GlobalConfig.random.nextDouble()
  GlobalConfig.random.nextDouble()
  GlobalConfig.random.nextDouble()
  val faapProblemInstancesConfiguration = FaapProblemInstancesConfiguration(1 to 5 by 1)
  val allocators = Seq(OptimalAllocator(SolverAllocatorConfig(pathHandler, Solver.Gurobi)))

  private val faapProblemInstancesGen = SimpleLatencyPathFaapProblemGenerator.LatencyPathCountCompareFaapProblemGenerator(faapProblemInstancesConfiguration)

  DefaultExpRunner(faapProblemInstancesConfiguration, faapProblemInstancesGen, allocators, pathHandler)
}

/**
  * Experiment which studies the impact of the instance scale using 3 latency paths. (not in thesis)
  */
object LatencyPathsWithGrowingGraphOrder extends App with ExperimentNaming {
  override val expName = "lpCount"
  private val pathHandler = getPathHandler(expType)

  val faapProblemInstancesConfiguration = FaapProblemInstancesConfiguration(6 to 10 by 2, 1)
  val allocators = Seq(OptimalAllocator(SolverAllocatorConfig(pathHandler, Solver.Gurobi)))

  private val faapProblemInstancesGen = SimpleLatencyPathFaapProblemGenerator.FixAppGraphScaleSPDGToCactusFaapProblemGenerator(faapProblemInstancesConfiguration, lbRatio = 0.2, lpCount = 3)

  DefaultExpRunner(faapProblemInstancesConfiguration, faapProblemInstancesGen, allocators, pathHandler)
}

/**
  * Problem generator with latency paths.
  */
object SimpleLatencyPathFaapProblemGenerator {
  def LatencyPathCountCompareFaapProblemGenerator(faapProblemInstancesConfiguration: FaapProblemInstancesConfiguration, appGraphScale: Int = ExperimentCommon.defaultAppGraphOrder,
                                                  fogGraphScale: Int = 14, lbRatio: Double = ExperimentCommon.defaultLocationBoundMappingRatio): () => IndexedSeq[FaapProblemInstanceRepetitions] = {
    () => (for (lpCount <- faapProblemInstancesConfiguration.xAxisRange) yield {
      val appGraphGenerator = SpdgApplicationGraphGenerator(SeriesParallelDecomposableGraphGenConfig(appGraphScale))

      val nodeUsageConfig = GraphGenBase(fogNodeConstraintGen = () => GlobalConfig.random.nextDouble() * 0.666)
      val fogGraphGenerator = CactusFogGraphGenerator(CactusGraphGenConfig(fogGraphScale, graphGenBase = nodeUsageConfig))

      val locationBoundMappingGenerator = DefaultLocationBoundMappingGenerator(LocationBoundMappingConfig(lbRatio))
      val latencyPathGenerator = DefaultLatencyPathGenerator(LatencyPathConfiguration(lpCount))

      def generateProblemInstance(rep: Int) = {
        val appGraph = appGraphGenerator.generateApplicationGraph
        val fogGraph = fogGraphGenerator.generateFogGraph
        val locationBoundMapping = locationBoundMappingGenerator.generateLocationBoundMappings(appGraph, fogGraph)

        val instWithout = FaapProblemInstance(rep, appGraph, fogGraph, Some(locationBoundMapping), None, id = s"SPDG2Cac_nolp$lpCount")
        val instWith = FaapProblemInstance(rep, appGraph, fogGraph, Some(locationBoundMapping), Some(latencyPathGenerator.generateLatencyPaths(appGraph, fogGraph, locationBoundMapping)), id = s"SPDG2Cac_lp$lpCount")
        (instWithout, instWith)
      }

      val instanceRepetitions = for (rep <- 0 until faapProblemInstancesConfiguration.repetitions) yield generateProblemInstance(rep)

      val repsWithoutLps = FaapProblemInstanceRepetitions(instanceRepetitions.map(_._1), s"SPDG2Cac_nolp$lpCount", lpCount.toString + "_none", lpCount*2)
      val repsWithLps = FaapProblemInstanceRepetitions(instanceRepetitions.map(_._2), s"SPDG2Cac_lp$lpCount", lpCount.toString, lpCount*2+1)

      Seq(repsWithLps, repsWithoutLps)
    }).flatten
  }

  def FixAppGraphScaleSPDGToCactusFaapProblemGenerator(faapProblemInstancesConfiguration: FaapProblemInstancesConfiguration, appGraphScale: Int = 10, lbRatio: Double = 0.1, lpCount: Int = 2): () => IndexedSeq[FaapProblemInstanceRepetitions] = {
    () => for (fogGraphScale <- faapProblemInstancesConfiguration.xAxisRange) yield {
      val appGraphGenerator = SpdgApplicationGraphGenerator(SeriesParallelDecomposableGraphGenConfig(appGraphScale))
      val fogGraphGenerator = CactusFogGraphGenerator(CactusGraphGenConfig(fogGraphScale))
      val locationBoundMappingGenerator = DefaultLocationBoundMappingGenerator(LocationBoundMappingConfig(lbRatio))
      val latencyPathGrenerator = DefaultLatencyPathGenerator(LatencyPathConfiguration(lpCount))

      val generator = FaapProblemGenerator(appGraphGenerator, fogGraphGenerator, locationBoundMappingGenerator, Some(latencyPathGrenerator), description = s"SPDG2Cac_fn${fogGraphScale}_an${appGraphScale}_lb${lbRatio}_lp$lpCount", fogGraphScale.toString, fogGraphScale)
      generator.generateProblemInstances(faapProblemInstancesConfiguration.repetitions)
    }
  }
}