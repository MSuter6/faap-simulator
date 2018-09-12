package experiments

import Utils.time
import io.PathHandler
import io.visualization.DefaultPlotWriterConfigs.DefaultPlotWriterConfig
import io.visualization.PlotWriterConfig
import simulator.allocation.{AllocationException, Allocator}
import simulator.evaluation.{DefaultMappingEvaluator, MappingEvaluator}
import simulator.generation.FaapProblemGenerator
import simulator.{FaapProblemInstance, FaapProblemInstanceRepetitions}

/**
  * Basic class to run experiments. All configuration parameters are passed via the constructor and the experiment
  * is started using the run()-method. Prints informations to the experiments to the console.
  * @param faapProblemInstances Sequence of all problem instances for the experiment
  * @param faapProblemInstancesConfiguration Exp config
  * @param allocators The allocators to use
  * @param pathHandler The pathHandler (needed for solver artifacts)
  * @param mappingEvaluator The evaluation method to use
  * @param plotWriterConfig The desired plots to export (plots are *not* produced using run()-method; use [[io.ExperimentExporter]])
  */
case class ExperimentRunner(faapProblemInstances: Seq[FaapProblemInstanceRepetitions], faapProblemInstancesConfiguration: FaapProblemInstancesConfiguration, allocators: Seq[Allocator], pathHandler: PathHandler = PathHandler(), mappingEvaluator: MappingEvaluator = DefaultMappingEvaluator, plotWriterConfig: PlotWriterConfig = DefaultPlotWriterConfig){

  def printStats(): Unit = faapProblemInstances.foreach(printProblemInstanceStats)

  def printProblemInstanceStats(instance: FaapProblemInstanceRepetitions): Unit = {
    println(f"Problem instance stats: ${instance.description}, first repetition instance:")
    val firstInstance = instance.instances.head

    println(f"App nodes: ${firstInstance.appGraph.order}")
    println(f"App edges: ${firstInstance.appGraph.graphSize}")
    println(f"Fog nodes: ${firstInstance.fogGraph.order}")
    println(f"Fog edges: ${firstInstance.fogGraph.graphSize}")
    println(f"LB size: ${firstInstance.locationBoundMapping.get.size}")
  }

  def run(): ExperimentWrapper = {
    val experimentInstances = {
      for {faapProblemInstance <- faapProblemInstances; allocator <- allocators} yield ExperimentInstance(allocator, faapProblemInstance)
    }

    val results = runExperiment(experimentInstances, mappingEvaluator)

    val expConfig = ExperimentConfig(pathHandler.id, GlobalConfig.seed, allocators, mappingEvaluator, plotWriterConfig)

    ExperimentWrapper(expConfig, faapProblemInstancesConfiguration, null, results, pathHandler) // TODO: Do something about the faapProblemGenerator (removing seems to be the best option)
  }

  /**
    * Runs the whole experiment, applying each allocator to all experiment instances and evaluating the resulting mappings.
    */
  private def runExperiment(experimentInstances: Seq[ExperimentInstance], mappingEvaluator: MappingEvaluator): Seq[ExperimentIteration] = {
    experimentInstances.map(experimentInstance => {
      println("----------------------------------------")
      println(f"${Console.BOLD}FOM Problem Instance: ${experimentInstance.faapProblemInstanceRepetitions.description}, Allocation Policy: ${experimentInstance.allocator.policyName}${Console.RESET}")

      val repetitionResults: Seq[Option[ExperimentResult]] = experimentInstance.faapProblemInstanceRepetitions.instances.map(repetitionInstance => {
        val experimentResult = testAllocationStrategy(repetitionInstance, experimentInstance.allocator, mappingEvaluator)

        val resultString = if (experimentResult.isDefined) experimentResult.get.evaluationResult.mkConsoleString else "Failed to find solution"
        println(f"Policy evaluation result: ${experimentResult.flatMap(_.timestamps.map(_.shortString)).getOrElse("")}, $resultString")

        if (experimentResult.isDefined) if (experimentResult.get.isValidResult) experimentResult else None else None
      })
      ExperimentIteration(experimentInstance, repetitionResults)
    })
  }

  private def testAllocationStrategy(repetitionInstance: FaapProblemInstance, allocator: Allocator, mappingEvaluator: MappingEvaluator): Option[ExperimentResult] = {
    try {
      val (mapping, allocationTime) = time {
        allocator.runAllocation(repetitionInstance)
      }

      val (evaluationResult, evaluationTime) = time {
        mappingEvaluator.evaluateMapping(repetitionInstance, mapping)
      }

      Some(ExperimentResult(mapping, evaluationResult, Some(ExperimentTimestamps(allocationTime, evaluationTime))))
    } catch {
      case e1: Exception =>
        Console.err.println("exception" + e1)
        None
    }
  }
}
