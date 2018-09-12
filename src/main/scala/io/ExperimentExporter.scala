package io

import java.nio.file.Path

import GlobalConfig.NL
import GraphDefinition.GraphNode
import Utils.Serializer
import better.files.Dsl.SymbolicOperations
import better.files.File
import experiments._
import io.dot.{DotApplicationGraphGenerator, DotFogGraphGenerator, MappingGraphGenerator}
import simulator.{FaapProblemInstance, FaapProblemInstanceRepetitions}

import scala.sys.process._

/**
  * Provides functionality to export all artifacts produced with an experiment.
  * This includes all objects stored in the [[experiments.ExperimentWrapper]] class.
  * @param pathHandler Where to write the files.
  */
case class ExperimentExporter(pathHandler: PathHandler) {
  def runDotPngCommand(graphPath: Path, pngOutPath: Path): Unit = {
    Seq("cmd", "/C", "dot", "-Tpng", graphPath.toString, "-o", pngOutPath.toString) ! ProcessLogger(_ => ())
  }

  /**
    * Exports the whole experiment
    * @param experimentWrapper The experiment to export.
    * @param withGraphPlots If visualizations of the app graph and the fog graph (.png files using graphviz) should be produced.
    */
  def writeExperiment(experimentWrapper: ExperimentWrapper, withGraphPlots: Boolean = true): Unit = {
    writeExperimentConfig(experimentWrapper)

    writeExperimentFinalResult(experimentWrapper)

    writeFaapProblemInstances(experimentWrapper.experimentIterations, withGraphPlots)

    writeExperimentIterations(experimentWrapper, withGraphPlots)
  }

  def writeExperimentIterations(experimentWrapper: ExperimentWrapper, withPng: Boolean): Unit = {
    experimentWrapper.experimentIterations.foreach(writeExperimentIteration(_, experimentWrapper.pathHandler, withPng))
  }

  def writeExperimentConfig(experimentWrapper: ExperimentWrapper): Unit = {
    val configFile = pathHandler.getConfig()

    val config =
      s"""Experiment Id, ${experimentWrapper.experimentConfig.id}
         |Random seed, ${experimentWrapper.experimentConfig.seed}
         |Allocators, ${experimentWrapper.experimentConfig.allocators.map(_.getClass).mkString(";")}
         |FaapProblemInstancesConfig, ${experimentWrapper.faapProblemInstancesConfiguration}
         |Iteration size, ${experimentWrapper.experimentIterations.size}
         |Evaluation method, ${experimentWrapper.experimentConfig.evalMethod}""".stripMargin

    configFile << config

    Serializer.serialize(experimentWrapper.experimentConfig, pathHandler.getConfig("-ser.conf").pathAsString)
  }

  def writeFaapProblemInstances(experimentIterations: Seq[ExperimentIteration], withPng: Boolean): Unit = {
    experimentIterations.map(_.experimentInstance.faapProblemInstanceRepetitions).distinct.foreach(iter => {
      iter.instances.foreach(repetitionInstance => {
        writeFaapProblemInstance(iter, repetitionInstance, withPng)
      })
    })
  }

  def writeIterationConfig(experimentIteration: ExperimentIteration, iterPath: File): Unit = {
    val configFile = pathHandler.getIterConfig(experimentIteration.experimentInstance)

    val allocator = experimentIteration.experimentInstance.allocator
    val config =
      s"""Iteration Configuration Id (${experimentIteration.experimentInstance.id}):
         |   Mapping policy:\t\t       ${allocator.policyName} - ${allocator.policyDescription}""".stripMargin

    configFile << config
  }

  def writeRepetitionConfig(experimentInstance: ExperimentInstance, faapProblemInstance: FaapProblemInstance): Unit = {
    val configFile = pathHandler.getRepConfig(experimentInstance, faapProblemInstance)

    val appGraph = faapProblemInstance.appGraph
    val fogGraph = faapProblemInstance.fogGraph
    val config =
      s"""Repetition number (${faapProblemInstance.repetition}):
         |   App graph stats:\t\t      Node count ${appGraph.nodes.size}, Edge count: ${appGraph.edges.size}
         |   Fog graph stats:\t  Node count ${fogGraph.nodes.size}, Edge count: ${fogGraph.edges.size}""".stripMargin

    configFile << config
  }

  def writeExperimentIteration(experimentIteration: ExperimentIteration, pathHandler: PathHandler, withPng: Boolean): Unit = {
    val iterPath = pathHandler.getIterPath(experimentIteration.experimentInstance)

    writeIterationConfig(experimentIteration, iterPath)

    experimentIteration.experimentInstance.faapProblemInstanceRepetitions.instances.foreach(repetitionInstance => {
      writeRepetitionConfig(experimentIteration.experimentInstance, repetitionInstance)

      val repetition = repetitionInstance.repetition
      if (experimentIteration.experimentResults(repetition).isDefined) {
        writeExperimentResults(experimentIteration, repetition, withPng)
      }
    })
  }

  def writeFaapProblemInstance(faapProblemInstanceRepetitions: FaapProblemInstanceRepetitions, faapProblemInstance: FaapProblemInstance, withPng: Boolean): Unit = {
    writeAppGraph(faapProblemInstanceRepetitions, faapProblemInstance, withPng)
    writeFogGraph(faapProblemInstanceRepetitions, faapProblemInstance, withPng)
    writeLocationBoundMapping(faapProblemInstanceRepetitions, faapProblemInstance)
    writeLatencyPaths(faapProblemInstanceRepetitions, faapProblemInstance)
  }

  def writeExperimentResults(experimentIteration: ExperimentIteration, repetition: Int, withPng: Boolean = false): Unit = {
    val experimentInstance = experimentIteration.experimentInstance
    val faapProblemInstance = experimentInstance.faapProblemInstanceRepetitions.instances(repetition)
    val experimentResult = experimentIteration.experimentResults(repetition).get

    writeMapping(experimentInstance, faapProblemInstance, experimentResult, withPng)
    writeEvaluationResults(experimentInstance, faapProblemInstance, experimentResult)
    writeTimestamps(experimentInstance, faapProblemInstance, experimentResult)
  }

  def writeAppGraph(faapProblemInstanceRepetitions: FaapProblemInstanceRepetitions, faapProblemInstance: FaapProblemInstance, withPng: Boolean): Unit = {
    val appGraphPath = pathHandler.getAppGraph(faapProblemInstanceRepetitions, faapProblemInstance.repetition)
    val appGraphPngPath = pathHandler.getAppGraph(faapProblemInstanceRepetitions, faapProblemInstance.repetition, ".png")
    val appGraphNodeWeightsPath = pathHandler.getAppGraphNodeWeights(faapProblemInstanceRepetitions, faapProblemInstance.repetition)

    val appGraph = faapProblemInstance.appGraph

    appGraphPath < DotApplicationGraphGenerator.generateDotRepresentation(appGraph)

    writeGraphPng(appGraph.nodes.toOuter, appGraphPath, appGraphPngPath, withPng)

    writeNodeWeights(appGraph.nodes.toOuter, appGraphNodeWeightsPath)
  }

  def writeFogGraph(faapProblemInstanceRepetitions: FaapProblemInstanceRepetitions, faapProblemInstance: FaapProblemInstance, withPng: Boolean): Unit = {
    val fogGraphPath = pathHandler.getFogGraph(faapProblemInstanceRepetitions, faapProblemInstance.repetition)
    val fogGraphPngPath = pathHandler.getFogGraph(faapProblemInstanceRepetitions, faapProblemInstance.repetition, ".png")
    val fogGraphNodeWeightsPath = pathHandler.getFogGraphNodeWeights(faapProblemInstanceRepetitions, faapProblemInstance.repetition)

    val fogGraph = faapProblemInstance.fogGraph

    writeNodeWeights(fogGraph.nodes.toOuter, fogGraphNodeWeightsPath)
    fogGraphPath < DotFogGraphGenerator.generateDotRepresentation(fogGraph)

    writeGraphPng(fogGraph.nodes.toOuter, fogGraphPath, fogGraphPngPath, withPng)

  }

  private def writeGraphPng[V <: GraphNode](nodes: Set[V], appGraphPath: File, appGraphPngPath: File, withPng: Boolean): Unit = {
    if (withPng && nodes.size <= GlobalConfig.graphNodeCountDrawingThreshold) {
      // Don't generate pic for too large graphs
      runDotPngCommand(appGraphPath.path, appGraphPngPath.path)
    }
  }

  private def writeNodeWeights[V <: GraphNode](nodes: Set[V], graphNodeWeightsPath: File): Unit = {
    graphNodeWeightsPath < nodes.map(node => f"${
      node.toString
    } -> ${
      node.resourceConstraint.toDouble
    }").mkString(NL)
  }

  def writeLocationBoundMapping(faapProblemInstanceRepetitions: FaapProblemInstanceRepetitions, faapProblemInstance: FaapProblemInstance): Unit = {
    faapProblemInstance.locationBoundMapping.foreach(mapping => {
      val locationBoundMappingFile = pathHandler.getLocationBoundMapping(faapProblemInstanceRepetitions, faapProblemInstance.repetition)

      locationBoundMappingFile < mapping.mkString(NL)
    })
  }

  def writeLatencyPaths(faapProblemInstanceRepetitions: FaapProblemInstanceRepetitions, faapProblemInstance: FaapProblemInstance): Unit = {
    faapProblemInstance.latencyPaths.foreach(paths => {
      val latencyPathFile = pathHandler.getLatencyPath(faapProblemInstanceRepetitions, faapProblemInstance.repetition)
      paths.paths.foreach(path => {

        latencyPathFile << path.path.mkString(" -> ") + ", " + path.deadline
      })
    })
  }

  def writeMapping(experimentInstance: ExperimentInstance, faapProblemInstance: FaapProblemInstance, experimentResult: ExperimentResult, withPng: Boolean): Unit = {
    val mapping = experimentResult.mapping

    val mappingFile = pathHandler.getMappingResult(experimentInstance, faapProblemInstance)
    val mappingDotFile = pathHandler.getMappingResult(experimentInstance, faapProblemInstance, ".dot")
    val mappingPngFile = pathHandler.getMappingResult(experimentInstance, faapProblemInstance, ".png")

    mappingFile < mapping.mkString(NL)

    mappingDotFile < MappingGraphGenerator.generateDotRepresentation(faapProblemInstance.fogGraph, faapProblemInstance.appGraph, mapping)
    writeGraphPng(faapProblemInstance.fogGraph.nodes.toOuter, mappingDotFile.path, mappingPngFile.path, withPng)
  }

  def writeEvaluationResults(experimentInstance: ExperimentInstance, faapProblemInstance: FaapProblemInstance, experimentResult: ExperimentResult): Unit = {
    val evalutationFile = pathHandler.getEvaluationResult(experimentInstance, faapProblemInstance)

    evalutationFile < experimentResult.evaluationResult.mkString
  }

  def writeTimestamps(experimentInstance: ExperimentInstance, faapProblemInstance: FaapProblemInstance, experimentResult: ExperimentResult): Unit = {
    experimentResult.timestamps.foreach(timestamps => {
      val evalutationFile = pathHandler.getTimestamp(experimentInstance, faapProblemInstance)

      evalutationFile < timestamps.mkString
    })
  }

  def writeExperimentFinalResult(experimentWrapper: ExperimentWrapper): Unit = {
    experimentWrapper.experimentConfig.plotWriterConfig.plots.foreach {
      case (extractor, plotWriter) =>
        val (plotTempPath, plotPngPath) = experimentWrapper.pathHandler.getFinalResultPath(s"_${
          extractor.sanitizedName
        }_${plotWriter.aggregationTypeName.getOrElse("")}.png")
        plotWriter.writeExperiment(experimentWrapper, plotTempPath, plotPngPath, extractor, experimentWrapper.pathHandler.id)
    }
  }
}


