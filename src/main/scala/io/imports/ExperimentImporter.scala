package io.imports

import GraphDefinition.{AppNode, FogNode}
import Utils.Serializer
import better.files.File
import experiments._
import io.PathHandler
import io.dot.DotParser
import scalax.collection.Graph
import scalax.collection.edge.{WDiEdge, WUnDiEdge}
import simulator.Mapping.Mapping
import simulator._
import simulator.evaluation.{ConstraintsCheckResult, DefaultMappingEvaluationResult, MappingEvaluationResult}
import simulator.generation.FaapProblemGenerator

/**
  * Import artifacts from previous experiments. An experiment contains all objects stored in the [[experiments.ExperimentWrapper]]
  */
object ExperimentImporter {

  def importExperiment(pathHandler: PathHandler): ExperimentWrapper = {
    val repetitions = importFaapProblemInstances(pathHandler.getInstancesPath)

    val expConfig = importExperimentConfiguration(pathHandler)

    val iterations: Seq[ExperimentIteration] = importExperimentIterations(pathHandler, expConfig, repetitions)

    val problemGenerator = importFaapProblemGenerator(pathHandler.getExp)

    ExperimentWrapper(expConfig, FaapProblemInstancesConfiguration(0, 0), problemGenerator, iterations, pathHandler)
  }

  def importExperimentIterations(pathHandler: PathHandler, expConfig: ExperimentConfig, repetitions: Seq[FaapProblemInstanceRepetitions]): Seq[ExperimentIteration] = {
    val iterations = expConfig.allocators.flatMap(alloc => {

      repetitions.map(iterRepetitions => {
        println(iterRepetitions.description)

        val instance = ExperimentInstance(alloc, iterRepetitions)

        val results = iterRepetitions.instances.zipWithIndex.map { case (faapInstance, _) =>
          importExperimentResults(pathHandler.getResultsPath(instance, faapInstance), faapInstance.appGraph, faapInstance.fogGraph)
        }

        ExperimentIteration(instance, results)
      })
    })
    iterations
  }

  def importExperimentConfiguration(pathHandler: PathHandler): ExperimentConfig = {
    Serializer.deserialize[ExperimentConfig](pathHandler.getConfig("-ser.conf").pathAsString)
  }


  def importFaapProblemInstances(instancesPath: File): Seq[FaapProblemInstanceRepetitions] = {
    val list = instancesPath.list.filter(_.isDirectory).toList

    list.map(instanceDir => {
      val (order, xAxis, instName) = instanceDir.name.split("-") match {
        case Array(orderString, xAxisString, instNameString) => (orderString.toInt, xAxisString, instNameString)
        case d => throw ImporterException(f"Failed to read problem instance dir: ${d.mkString}")
      }

      val repetitions = instanceDir.list.filter(_.name.startsWith("r")).toSeq.sortBy(_.name.drop(1).toInt).map(repetition => {
        importFaapProblemInstance(repetition, instName)
      }).toList
      println(instanceDir.pathAsString)

      FaapProblemInstanceRepetitions(repetitions.toIndexedSeq, instName, xAxis, order)
    })
  }

  def importNodeWeights(file: File): Map[String, BigDecimal] = {
    val iterator = file.lineIterator

    Map(iterator.map(line => {
      line.split(" ") match {
        case Array(appN, _, fogN) => (appN, Utils.importBigDecimalToPrecision(fogN))
      }
    }).toSeq: _*)
  }

  private def importFaapProblemInstance(repetition: File, description: String): FaapProblemInstance = {
    val appNodeWeights = importNodeWeights(PathHandler.getAppGraphNodeWeights(repetition))
    val fogNodeWeights = importNodeWeights(PathHandler.getFogGraphNodeWeights(repetition))

    val appGraph = importAppGraph(PathHandler.getAppGraph(repetition), appNodeWeights)
    val fogGraph = importFogGraph(PathHandler.getFogGraph(repetition), fogNodeWeights)
    val lbMapping = importMapping(PathHandler.getLocationBoundMapping(repetition), appGraph, fogGraph)
    val lp = importLatencyPaths(PathHandler.getLatencyPath(repetition), appGraph)
    FaapProblemInstance(repetition.name.drop(1).toInt, appGraph, fogGraph, Some(lbMapping), lp, description)
  }

  def importExperimentResults(file: File, appGraph: Graph[AppNode, WDiEdge], fogGraph: Graph[FogNode, WUnDiEdge]): Option[ExperimentResult] = {
    if (!PathHandler.getMappingResult(file).exists) {
      None
    } else {
      val mapping = importMapping(PathHandler.getMappingResult(file), appGraph, fogGraph)
      val evaluation = importEvaluation(PathHandler.getEvaluationResult(file))
      val timestamps = importTimestamp(PathHandler.getTimestamp(file))

      Some(ExperimentResult(mapping, evaluation, timestamps))
    }
  }

  def importEvaluation(file: File): MappingEvaluationResult = {
    val iterator = file.lineIterator
    val cost = readConfigLine(iterator.next(), ':', "eval result").toDouble

    val validMapping = readConfigLine(iterator.next(), ':', "eval result").toBoolean
    val nodeConstr = readConfigLine(iterator.next(), ':', "eval result").toBoolean
    val edgeconstr = readConfigLine(iterator.next(), ':', "eval result").toBoolean
    val pathConstr = readConfigLine(iterator.next(), ':', "eval result").toBoolean
    val lbConstr = readConfigLine(iterator.next(), ':', "eval result").toBoolean

    DefaultMappingEvaluationResult(cost, ConstraintsCheckResult(validMapping, nodeConstr, edgeconstr, pathConstr, lbConstr))
  }

  def readConfigLine(line: String, delim: Char = ',', msg: String): String = {
    line.split(delim) match {
      case Array(_, ids) => ids.trim
      case l => throw ImporterException(f"Failed to read $msg file (line ${l.mkString})")
    }
  }

  def importTimestamp(timestampsFile: File): Option[ExperimentTimestamps] = {
    if (!timestampsFile.exists) {
      None
    } else {
      val iterator = timestampsFile.lineIterator

      val allocTime = readConfigLine(iterator.next(), ':', "timestamps result").toDouble
      val evalTime = readConfigLine(iterator.next(), ':', "timestamps result").toDouble

      Some(ExperimentTimestamps.fromSeconds(allocTime, evalTime))
    }
  }

  def importFaapProblemGenerator(generatorPath: File): FaapProblemGenerator = {
    Utils.warning("Faap Problem Generator can not yet be imported")
    FaapProblemGenerator(null, null, null, null, null, null, 0)
  }

  def importAppGraph(appGraphPath: File, appNodeWeights: Map[String, BigDecimal]): Graph[AppNode, WDiEdge] = DotParser.parseAppGraph(appGraphPath.lineIterator, appNodeWeights)

  def importFogGraph(fogGraphPath: File, fogNodeWeights: Map[String, BigDecimal]): Graph[FogNode, WUnDiEdge] = DotParser.parseFogGraph(fogGraphPath.lineIterator, fogNodeWeights)

  def importMapping(file: File, appGraph: Graph[AppNode, WDiEdge], fogGraph: Graph[FogNode, WUnDiEdge]): Mapping = {
    val iterator = file.lineIterator

    val appNodeStrings = appGraph.nodes.map(n => n.toString() -> n).toMap
    val fogNodeStrings = fogGraph.nodes.map(n => n.toString() -> n).toMap

    Mapping(iterator.map(line => {
      val (appNodeString, fogNodeString) = line.split(" ") match {
        case Array(appN, _, fogN) => (appN, fogN)
      }

      val appNode = appNodeStrings.get(appNodeString)
      val fogNode = fogNodeStrings.get(fogNodeString)

      appNode.getOrElse(throw ImporterException(s"App node $appNodeString in lb mapping can't be imported as it does not exist (path: ${file.pathAsString}"))
      fogNode.getOrElse(throw ImporterException(s"Fog node $fogNodeString in lb mapping can't be imported as it does not exist (path: ${file.pathAsString}"))

      appNode.get.toOuter -> fogNode.get.toOuter
    }).toSeq: _*)
  }

  def importLatencyPaths(file: File, appGraph: Graph[AppNode, WDiEdge]): Option[LatencyPaths] = {
    if (!file.exists) {
      None
    } else {
      val iterator = file.lineIterator

      Some(LatencyPaths(iterator.map(line => {
        val (nodes, deadline) = line.split(",") match {
          case Array(n, deadlineString) => (n.split("->"), deadlineString.trim.toDouble)
        }

        LatencyPath(nodes.map(nodeString => {
          val appNode = appGraph.nodes.find(_.toString() == nodeString.trim)

          appNode.getOrElse(throw ImporterException(s"App node $nodeString in latency path can't be imported as it does not exist (path: ${file.pathAsString}")).toOuter
        }), deadline)
      }).toSeq: _*))
    }
  }
}

/**
  * Returned if the import of an experiment fails
  */
case class ImporterException(private val message: String = "",
                             private val cause: Throwable = None.orNull) extends Exception(message, cause)

