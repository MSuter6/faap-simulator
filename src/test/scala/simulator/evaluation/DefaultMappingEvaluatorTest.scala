package simulator.evaluation

import GraphDefinition.{AppNode, FogNode}
import scalax.collection.Graph
import scalax.collection.edge.{WDiEdge, WUnDiEdge}
import simulator.allocation.GreedyBorderAllocator
import simulator.generation.GraphGenBase
import simulator.{FaapProblemInstance, LatencyPath, LatencyPaths, Mapping}

class DefaultMappingEvaluatorTest extends org.scalatest.FunSuite {
  implicit val graphConfigBase: GraphGenBase = GraphGenBase()

  test("Cost should be 0 for empty mapping") {
    val emptyAppGraph = Graph[AppNode, WDiEdge]()
    val emptyFogGraph = Graph[FogNode, WUnDiEdge]()

    val emptyMapping = Mapping.empty

    val cost = DefaultMappingEvaluator.computeCost(emptyAppGraph, emptyFogGraph, emptyMapping)
    assert(cost == 0)
  }

  /** Simple Graph
    *
    *   Application Graph
    *      3     4
    *   1 --> 2 --> 3
    *
    *   Fog Graph
    *
    *   1 -- 2 -- 3 -- 4
    *
    */
  val appNode1: AppNode = AppNode.generateRandomly
  val appNode2: AppNode = AppNode.generateRandomly
  val appNode3: AppNode = AppNode.generateRandomly

  val appGraph: Graph[AppNode, WDiEdge] = Graph[AppNode, WDiEdge](
    WDiEdge(appNode1, appNode2)(3),
    WDiEdge(appNode2, appNode3)(4)
  )

  val fogNode1: FogNode = FogNode.generateRandomly
  val fogNode2: FogNode = FogNode.generateRandomly
  val fogNode3: FogNode = FogNode.generateRandomly
  val fogNode4: FogNode = FogNode.generateRandomly

  val fogGraph: Graph[FogNode, WUnDiEdge] = Graph[FogNode, WUnDiEdge](
    WUnDiEdge(fogNode1, fogNode2)(1320),
    WUnDiEdge(fogNode2, fogNode3)(100),
    WUnDiEdge(fogNode3, fogNode4)(1)
  )

  test("Simple mapping should compute cost correctly"){
    val mapping = Mapping((appNode1, fogNode1), (appNode2, fogNode4), (appNode3, fogNode4))

    val cost = DefaultMappingEvaluator.computeCost(appGraph, fogGraph, mapping)
    assert(cost == 9)
  }

  test("Simple different mapping should compute cost correctly"){
    val mapping = Mapping((appNode1, fogNode1), (appNode2, fogNode2), (appNode3, fogNode4))

    val cost = DefaultMappingEvaluator.computeCost(appGraph, fogGraph, mapping)
    assert(cost == 11)
  }

  test("Simple different mapping 2 should compute cost correctly"){
    val mapping = Mapping((appNode1, fogNode1), (appNode2, fogNode3), (appNode3, fogNode4))

    val cost = DefaultMappingEvaluator.computeCost(appGraph, fogGraph, mapping)
    assert(cost == 10)
  }

  test("Simple mapping on same node should have no cost"){
    val mapping = Mapping((appNode1, fogNode1), (appNode2, fogNode1), (appNode3, fogNode1))

    val cost = DefaultMappingEvaluator.computeCost(appGraph, fogGraph, mapping)
    assert(cost == 0)
  }

  test("GreedyBorderAllocator allocation should work"){
    val locationBoundMapping = Mapping((appNode1, fogNode1), (appNode3, fogNode4))

    val faapProblemInstance = FaapProblemInstance(0, appGraph, fogGraph, Some(locationBoundMapping))

    val (mapping, allocationTime) = Utils.time {
      GreedyBorderAllocator.runAllocation(faapProblemInstance)
    }

    println(appGraph.edges)
    println(fogGraph.edges)
    println(mapping)
    println(f"Time to compute allocation: ${allocationTime / 1000000000.0} s")
    val mappingEvaluator = DefaultMappingEvaluator

    val evaluationResult = mappingEvaluator.evaluateMapping(faapProblemInstance, mapping)

    println(evaluationResult.mkString)
  }

  test("Should confirm latency path validity"){
    val latencyPaths = LatencyPaths(LatencyPath(Seq(appNode1, appNode2, appNode3), 3))

    assert(!DefaultMappingEvaluator.checkIfLatencyPathsAreInvalid(appGraph, Some(latencyPaths)))
  }

  test("Should fail with invalid latency path with invalid path"){
    val latencyPaths = LatencyPaths(LatencyPath(Seq(appNode1, appNode3), 3))

    assert(DefaultMappingEvaluator.checkIfLatencyPathsAreInvalid(appGraph, Some(latencyPaths)))
  }

  test("Should fail with invalid latency path with wrong AppNode"){
    val latencyPaths = LatencyPaths(LatencyPath(Seq(appNode1, AppNode.generateRandomly), 3))

    assert(DefaultMappingEvaluator.checkIfLatencyPathsAreInvalid(appGraph, Some(latencyPaths)))
  }

  test("Latency path constraint should not be violated"){
    val mapping = Mapping((appNode1, fogNode1), (appNode2, fogNode2), (appNode3, fogNode3))

    val latencyPaths = LatencyPaths(LatencyPath(Seq(appNode1, appNode2, appNode3), 2))

    assert(!DefaultMappingEvaluator.checkPathConstraints(appGraph, fogGraph, mapping, Some(latencyPaths)))
  }

  test("Latency path constraint should be violated"){
    val mapping = Mapping((appNode1, fogNode1), (appNode2, fogNode2), (appNode3, fogNode4))

    val latencyPaths = LatencyPaths(LatencyPath(Seq(appNode1, appNode2, appNode3), 2))

    assert(DefaultMappingEvaluator.checkPathConstraints(appGraph, fogGraph, mapping, Some(latencyPaths)))
  }

  test("Latency path constraint should be violated with multiple paths (including one with too short deadline)"){
    val mapping = Mapping((appNode1, fogNode1), (appNode2, fogNode2), (appNode3, fogNode4))

    val latencyPaths = LatencyPaths(LatencyPath(Seq(appNode1, appNode2, appNode3), 3), LatencyPath(Seq(appNode2, appNode3), 1))

    assert(DefaultMappingEvaluator.checkPathConstraints(appGraph, fogGraph, mapping, Some(latencyPaths)))
  }
}
