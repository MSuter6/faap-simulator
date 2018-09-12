package experiments

import io.visualization.DefaultPlotWriterConfigs._
import io.visualization.PlotWriterConfig
import io.{ExperimentExporter, PathHandler}
import simulator.FaapProblemInstanceRepetitions
import simulator.allocation.Solver.Solver
import simulator.allocation._
import simulator.generation._


/**
  * Helper class to run experiments.
  * Makes use of the [[experiments.ExperimentRunner]]-class to run experiments and exports the experiment.
  * @param faapProblemInstancesConfiguration Config
  * @param instancesGenerator Generates the FaapProblemInstances
  * @param allocators The allocators to use
  */
case class DefaultExpRunner(faapProblemInstancesConfiguration: FaapProblemInstancesConfiguration,
                            instancesGenerator: () => Seq[FaapProblemInstanceRepetitions],
                            allocators: Seq[Allocator],
                            pathHandler: PathHandler = PathHandler(),
                            plotWriterConfig: PlotWriterConfig = DefaultPlotWriterConfig) {

  private val instanceRepetitions = instancesGenerator()
  val experimentRunner = ExperimentRunner(instanceRepetitions, faapProblemInstancesConfiguration, allocators, pathHandler, plotWriterConfig = plotWriterConfig)

  experimentRunner.printStats()

  private val expWrapper = experimentRunner.run()

  ExperimentExporter(pathHandler).writeExperiment(expWrapper)
}

/**
  * Basic experimant runner for many experiments which utilize a constant ratio between app and fog graph order.
  */
case class DefaultFixedOrderRatioRunner(faapProblemInstancesConfiguration: FaapProblemInstancesConfiguration,
                                        allocators: Seq[Allocator],
                                        appToFogNodeRatio: Double = 2,
                                        pathHandler: PathHandler = PathHandler(),
                                        plotWriterConfig: PlotWriterConfig = DefaultPlotWriterConfig) {
  DefaultExpRunner(faapProblemInstancesConfiguration,
    FaapProblemGenerator.defaultFaapProblemGenerator(faapProblemInstancesConfiguration, appToFogNodeRatio),
    allocators,
    pathHandler,
    plotWriterConfig
  )
}
