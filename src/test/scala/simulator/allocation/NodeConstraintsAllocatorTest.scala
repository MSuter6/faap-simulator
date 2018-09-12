package simulator.allocation

import GraphDefinition.{AppNode, FogNode}
import org.scalatest.FunSuite
import scalax.collection.Graph
import scalax.collection.edge.{WDiEdge, WUnDiEdge}
import simulator.Mapping.Mapping
import simulator.evaluation.DefaultMappingEvaluator
import simulator.{FaapProblemInstance, Mapping}


class NodeConstraintsAllocatorTest extends FunSuite {

  private def assertNoConstraintsViolated(faapProblemInstance: FaapProblemInstance, mapping: Mapping): Unit = {
    assert(DefaultMappingEvaluator.evaluateMapping(faapProblemInstance, mapping).constraintsViolated.noConstraintsViolated)
  }

  test("NodeConstraintsAllocator should compute correct allocation") {
    import VerySimpleFaapProblemInstance._

    val faapProblemInstance = verySimpleFaapProblemInstance

    val mapping = NodeConstraintsAllocator.runAllocation(faapProblemInstance)

    assert(mapping == optimalMapping)
    assertNoConstraintsViolated(faapProblemInstance, mapping)
  }

  object VerySimpleFaapProblemInstance {

    val appNode1 = AppNode(1, 0.5)
    val appNode2 = AppNode(2, 0.1)
    val appNode3 = AppNode(3, 0.49)

    val appGraph: Graph[AppNode, WDiEdge] = Graph[AppNode, WDiEdge](
      WDiEdge(appNode1, appNode2)(3),
      WDiEdge(appNode2, appNode3)(4)
    )

    val fogNode1 = FogNode(1, 0.6)
    val fogNode2 = FogNode(2, 0.54)
    val fogNode3 = FogNode(3, 0.55)
    val fogNode4 = FogNode(4, 0.51)
    val fogGraph: Graph[FogNode, WUnDiEdge] = Graph[FogNode, WUnDiEdge](
      WUnDiEdge(fogNode1, fogNode2)(1),
      WUnDiEdge(fogNode2, fogNode3)(1),
      WUnDiEdge(fogNode3, fogNode4)(1)
    )

    /** Simple Graph
      *
      * Application Graph
      *   3     4
      * 1 --> 2 --> 3
      *
      * Fog Graph
      *
      * 1 -- 2 -- 3 -- 4
      *
      */
    val verySimpleFaapProblemInstance: FaapProblemInstance = FaapProblemInstance(0, appGraph, fogGraph)

    val optimalMapping = Mapping(appNode1 -> fogNode1, appNode2 -> fogNode1, appNode3 -> fogNode3)
  }

}
