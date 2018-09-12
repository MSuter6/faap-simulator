package io

import experiments.{ExperimentRunner, FaapProblemInstancesConfiguration}
import io.imports.ExperimentImporter
import io.visualization.PlotWriterConfig
import org.scalatest.FunSuite
import simulator.allocation.{OptimalAllocator, Solver, SolverAllocatorConfig}
import simulator.generation.SPDGToCactusGraphGenerator

class ExperimentImporterTest extends FunSuite {

  test("testImportExperiment") {
    val faapConfig = FaapProblemInstancesConfiguration(6, 2)
    val pathHandler = PathHandler.getTest("importer")

    pathHandler.getExp.clear()

    val allocs = Seq(OptimalAllocator(SolverAllocatorConfig(pathHandler, Solver.SCIP)))

    val faapProblemInstances = SPDGToCactusGraphGenerator.SPDGToCactusFaapProblemGenerator(faapConfig, 1.5)

    val experimentRunner = ExperimentRunner(faapProblemInstances(), faapConfig, allocs, pathHandler, plotWriterConfig = PlotWriterConfig(Seq()))

    val expWrapper = experimentRunner.run()

    ExperimentExporter(pathHandler).writeExperiment(expWrapper)

    val imported = ExperimentImporter.importExperiment(pathHandler)

    val importResult = ExperimentRunner(imported.experimentIterations.map(_.experimentInstance.faapProblemInstanceRepetitions).distinct, faapConfig, imported.experimentConfig.allocators, PathHandler(imported.experimentConfig.id), imported.experimentConfig.evalMethod).run()
    assert(importResult.experimentIterations.map(_.experimentResults.map(r => (r.get.mapping.toSeq.sortBy(_._1.id), r.get.evaluationResult)).mkString).mkString == expWrapper.experimentIterations.map(_.experimentResults.map(r => (r.get.mapping.toSeq.sortBy(_._1.id), r.get.evaluationResult)).mkString).mkString)

    assert(imported.experimentIterations.map(_.experimentInstance.faapProblemInstanceRepetitions.instances.mkString).mkString == expWrapper.experimentIterations.map(_.experimentInstance.faapProblemInstanceRepetitions.instances.mkString).mkString)

    assert(imported.experimentIterations.map(_.experimentResults.map(r => (r.get.mapping.toSeq.sortBy(_._1.id), r.get.evaluationResult)).mkString).mkString == expWrapper.experimentIterations.map(_.experimentResults.map(r => (r.get.mapping.toSeq.sortBy(_._1.id), r.get.evaluationResult)).mkString).mkString)

  }
}
