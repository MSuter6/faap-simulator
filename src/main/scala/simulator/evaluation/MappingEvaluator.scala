package simulator.evaluation

import GraphDefinition.{AppNode, FogNode}
import scalax.collection.Graph
import scalax.collection.edge.{WDiEdge, WUnDiEdge}
import simulator.Mapping.Mapping
import simulator.{FaapProblemInstance, LatencyPaths}

import scala.util.Try

trait MappingEvaluator {
  def evaluationMethod: String

  def evaluateMapping(faapProblemInstance: FaapProblemInstance, mapping: Mapping): MappingEvaluationResult
}

/**
  * Evaluates a mapping according to the basic cost functions and constraints
  */
@SerialVersionUID(3143295282096733874l)
object DefaultMappingEvaluator extends MappingEvaluator with Serializable {
  override def evaluationMethod: String = "Default evaluation method: Computes cost (cumulated bandwidth usage) for mapping and checks for constraint violations (node, edge and latency path)"

  override def evaluateMapping(faapProblemInstance: FaapProblemInstance, mapping: Mapping): MappingEvaluationResult = {
    val constraintResults = checkConstraintViolations(faapProblemInstance, mapping)
    val cost = Try(computeCost(faapProblemInstance.appGraph, faapProblemInstance.fogGraph, mapping))

    DefaultMappingEvaluationResult(cost.getOrElse(Double.NaN), constraintResults)
  }

  /**
    * Computation of communication cost c, total transferred data assuming shortest path traversal (Thesis Section 3.1.2)
    * @return Cost for this problem instance and mapping.
    */
  def computeCost(appGraph: Graph[AppNode, WDiEdge], fogGraph: Graph[FogNode, WUnDiEdge], mapping: Mapping): Double = {
    val outerEdges = appGraph.edges.toList

    var sum = 0.0
    // Fully parallelized without memoization found to be the most effective method to compute cost.
    outerEdges.foreach(edge => {
      val fogNode1 = mapping(edge._1)
      val fogNode2 = mapping(edge._2)

      val edgeWeight = edge.weight
      val maybePath = fogGraph.nodes.get(fogNode1).shortestPathTo(fogGraph.get(fogNode2))
      sum += maybePath.get.edges.size.toDouble * edgeWeight
    })
    val edgeCosts = outerEdges.map(edge => {
      val fogNode1 = mapping(edge._1)
      val fogNode2 = mapping(edge._2)

      val edgeWeight = edge.weight
      val maybePath = fogGraph.nodes.get(fogNode1).shortestPathTo(fogGraph.get(fogNode2))
      maybePath.get.edges.size.toDouble * edgeWeight
    }).sum
    edgeCosts
  }

  /**
    * Checks all constraints for violations for this problem instance and mapping
    * @return [[simulator.evaluation.ConstraintsCheckResult]]
    */
  def checkConstraintViolations(faapProblemInstance: FaapProblemInstance, mapping: Mapping): ConstraintsCheckResult = {
    val appGraph = faapProblemInstance.appGraph
    val fogGraph = faapProblemInstance.fogGraph
    val locationBoundMapping = faapProblemInstance.locationBoundMapping
    val latencyPaths = faapProblemInstance.latencyPaths

    val validMapping = checkIfNotAllAppNodesAreMapped(mapping, appGraph, fogGraph)
    val nodeConstrRes = checkNodeConstraints(mapping)
    val edgeConstrRes = checkEdgeConstraints(appGraph, fogGraph, mapping)
    val pathConstrRes = checkPathConstraints(appGraph, fogGraph, mapping, latencyPaths)
    val locationBoundMappingRes = checkLocationBoundMappingConstraints(mapping, locationBoundMapping)

    ConstraintsCheckResult(validMapping, nodeConstrRes, edgeConstrRes, pathConstrRes, locationBoundMappingRes)
  }

  /**
    * @return True if Node capacity constraints violated
    */
  def checkNodeConstraints(mapping: Mapping): Boolean = {
    mapping.groupBy(_._2).exists(grouped => grouped._2.map(_._1.resourceUsage).sum > grouped._1.cpuConstraint)
  }

  /**
    * @return True if the latency limit is violated for the latency paths.
    */
  def checkPathConstraints(appGraph: Graph[AppNode, WDiEdge], fogGraph: Graph[FogNode, WUnDiEdge], mapping: Mapping, latencyPaths: Option[LatencyPaths]): Boolean = {
    if (latencyPaths.isEmpty) return false

    !latencyPaths.get.paths.forall(path => {
      val pathLatency = path.path.sliding(2).map(pathTuple => { // Each pair of app nodes should be directly connected.
        if (pathTuple.size > 1) {
          val n1 = pathTuple.head
          val n2 = pathTuple(1)
          fogGraph.find(mapping(n1)).get.shortestPathTo(fogGraph.find(mapping(n2)).get).get.edges.size
        } else {
          0
        }
      }).sum
      pathLatency <= path.deadline
    })
  }

  /**
    * Edge capacities *not implemented* (might be implemented for the model variation with the "Constrained Network links".
    */
  def checkEdgeConstraints(appGraph: Graph[AppNode, WDiEdge], fogGraph: Graph[FogNode, WUnDiEdge], mapping: Mapping): Boolean = {
    false
  }

  /**
    * @return True if not all location bound nodes are really mapped to their specified fog node
    */
  def checkLocationBoundMappingConstraints(mapping: Mapping, locationBoundMapping: Option[Mapping]): Boolean = {
    if (locationBoundMapping.isEmpty) return false

    val tuples = mapping.toSet.intersect(locationBoundMapping.get.toSet)
    tuples != locationBoundMapping.get.toSet
  }

  /**
    * @return True if a mapping does not contain all app nodes or contains invalid fog nodes
    */
  def checkIfNotAllAppNodesAreMapped(mapping: Mapping, appGraph: Graph[AppNode, WDiEdge], fogGraph: Graph[FogNode, WUnDiEdge]): Boolean = {
    !appGraph.nodes.forall(appNode => // Check that for every app node
      mapping.exists(mapping => appNode.equals(mapping._1) && // A mapping exists
        fogGraph.nodes.exists(_.equals(mapping._2)))) && // and the mapped fog node is in the network
      mapping.values.size == mapping.keys.size //
  }

  /**
    * @return True if latency paths are not valid (not a valid path and app graph or on fog graph)
    */
  def checkIfLatencyPathsAreInvalid(appGraph: Graph[AppNode, WDiEdge], latencyPaths: Option[LatencyPaths]): Boolean = {
    if (latencyPaths.isEmpty) return false

    !latencyPaths.get.paths.forall(path => {
      path.path.sliding(2).forall(pathTuple => { // Each pair of app nodes should be directly connected.
        val n1 = appGraph.find(pathTuple.head)
        val n2 = appGraph.find(pathTuple(1))
        if (n1.isEmpty || n2.isEmpty) false
        else n1.get.findOutgoingTo(n2.get).isDefined
      })
    })
  }
}
