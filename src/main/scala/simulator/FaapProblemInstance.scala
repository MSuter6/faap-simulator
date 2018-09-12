package simulator

import GraphDefinition.{AppNode, FogNode}
import Utils.check
import scalax.collection.Graph
import scalax.collection.edge.{WDiEdge, WUnDiEdge}
import simulator.Mapping.Mapping
import simulator.evaluation.DefaultMappingEvaluator

/**
  * Represents an FAAP instance Q
  * @param repetition ID of experiment repetition
  * @param appGraph Application graph
  * @param fogGraph Fog Graph
  * @param locationBoundMapping Location-bound mapping
  * @param latencyPaths Latency Path
  * @param id ID of instance
  */
case class FaapProblemInstance(repetition: Int,
                               appGraph: Graph[GraphDefinition.AppNode, WDiEdge],
                               fogGraph: Graph[GraphDefinition.FogNode, WUnDiEdge],
                               locationBoundMapping: Option[Mapping] = None,
                               latencyPaths: Option[LatencyPaths] = None,
                               id: String = hashCode().toString
                              ) {

  require(if (locationBoundMapping.isDefined) !DefaultMappingEvaluator.checkNodeConstraints(locationBoundMapping.get) else true,
    s"location-bound mapping violates fog node resource constraints ${locationBoundMapping.get}")

  require(!DefaultMappingEvaluator.checkIfLatencyPathsAreInvalid(appGraph, latencyPaths),
    s"Latency paths are not valid (app nodes in path is not list of adjacent nodes)")

  check(appGraph.nodes.nonEmpty, "App graph is empty")
  check(appGraph.edges.nonEmpty, "No app graph edges")
  check(fogGraph.nodes.nonEmpty, "Fog graph is empty")
}

object FaapProblemInstance {
  implicit def ordering[A <: FaapProblemInstance]: Ordering[A] = Ordering.by(inst => -(inst.fogGraph.order + inst.appGraph.order))
}

/**
  * Contains all [[FaapProblemInstance]] repetitions of an experiment run of a single configuration
  * @param instances Sequence of instances
  * @param description Description of the configuration
  * @param xAxis X axis label for this configuration
  * @param order The order of this configuration; decides the order for the plots (most of the time same as xAxis)
  */
case class FaapProblemInstanceRepetitions(instances: IndexedSeq[FaapProblemInstance], description: String, xAxis: String, order: Int)

object FaapProblemInstanceRepetitions {
  implicit def ordering[A <: FaapProblemInstanceRepetitions]: Ordering[A] = Ordering.by(inst => inst.instances.head)
}

/**
  * Represents a mapping from app nodes to fog nodes
  */
object Mapping {
  type Mapping = Map[AppNode, FogNode]

  def apply(entries: (AppNode, FogNode)*): Mapping = entries.toMap

  def empty: Mapping = Map.empty
}

/**
  * Represents a latency path
  * @param path A path on a application graph (sequence of [[AppNode]])
  * @param deadline The latency limit of the path
  */
case class LatencyPath(path: Seq[AppNode], deadline: Double)

/**
  * @param paths A set of [[LatencyPath]]s
  */
case class LatencyPaths(paths: LatencyPath*)
