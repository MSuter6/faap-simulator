package experiments

import io.imports.ExperimentImporter
import io.visualization.{BoxPlotPlotterPerfGuarnt, PlotWriterConfig}
import io.visualization.DefaultPlotWriterConfigs._
import io.{ExperimentExporter, PathHandler}
import simulator.allocation._
import simulator.generation.SPDGToCactusGraphGenerator


/**
  * This file contains various methods to use data from old experiments such as a complete rerun of an experiment,
  * rerunning an experiment with new allocators or problem instances, or simple rerun the plot generation.
  * The objects can be adapted easily to new requirements (different experiment, different allocator etc.)
  */

/**
  * Imports an experiment, reexports all artifacts from this experiment
  */
object ExperimentReevaluator extends App {
  val pathHandler = PathHandler.getLatestExperiment(scale.expType)

  val experiment = ExperimentImporter.importExperiment(pathHandler)

  private val reevaluationPathHandler = pathHandler.reevaluation

  val newExp = experiment.withPathHandler(reevaluationPathHandler)
  ExperimentExporter(reevaluationPathHandler).writeExperiment(newExp)
}

/**
  * Rerun an experiment with a new allocator
  */
object ExperimentWithNewAllocatorsReevaluator extends App {
  val pathHandler = PathHandler.getLatestExperiment(scale.expType)
  val additionalAllocators = Seq(CombinedAllocator)

  val experiment = ExperimentImporter.importExperiment(pathHandler)

  val expWithNewAllocator = ExperimentRunner(experiment.experimentIterations.map(_.experimentInstance.faapProblemInstanceRepetitions).distinct,
    experiment.faapProblemInstancesConfiguration,
    additionalAllocators,
    pathHandler,
    experiment.experimentConfig.evalMethod,
    experiment.experimentConfig.plotWriterConfig
  ).run()

  val combinedExperiment = (experiment join expWithNewAllocator).withPathHandler(PathHandler.expTypeWithName(pathHandler.expType, pathHandler.id, "combinedHeurist"))

  val exporter = ExperimentExporter(combinedExperiment.pathHandler)

  exporter.writeExperimentIterations(expWithNewAllocator, withPng = true)

  exporter.writeExperimentFinalResult(combinedExperiment)
}

/**
  * Reruns an experiment with new problem instances
  */
object LastExperimentWithNewProblemInstancesReevaluator extends App {
  val pathHandler = PathHandler.getLatestExperiment(scale.expType)

  val experiment = ExperimentImporter.importExperiment(pathHandler)
  val exporter = ExperimentExporter(experiment.pathHandler)

  val instances = SPDGToCactusGraphGenerator.SPDGToCactusFaapProblemGenerator(FaapProblemInstancesConfiguration(26 to 26 by 2), appToFogNodeRatio = 2)

  val expWithNewProblemInstances = ExperimentRunner(instances(),
    experiment.faapProblemInstancesConfiguration,
    experiment.experimentConfig.allocators,
    pathHandler,
    experiment.experimentConfig.evalMethod,
    experiment.experimentConfig.plotWriterConfig
  ).run()
  val combinedExperiment = experiment join expWithNewProblemInstances

  exporter.writeExperimentIterations(expWithNewProblemInstances, withPng = false)
  exporter.writeFaapProblemInstances(expWithNewProblemInstances.experimentIterations, withPng = false)
  exporter.writeExperimentFinalResult(combinedExperiment)
}

/**
  * Exports the results of an experiment.
  */
object LastExperimentWriteSolutionReevaluator extends App {
  val pathHandler = PathHandler.getLatestExperiment(scale.expType)

  val experiment = ExperimentImporter.importExperiment(pathHandler)

  val combinedExperiment = experiment using (evalTimeBoxPlotter joining costPerfRatioPlot)

  private val reevaluationPathHandler = pathHandler.reevaluation
  val newExp = combinedExperiment.withPathHandler(reevaluationPathHandler)

  ExperimentExporter(reevaluationPathHandler).writeExperimentIterations(experiment, withPng = true)
}