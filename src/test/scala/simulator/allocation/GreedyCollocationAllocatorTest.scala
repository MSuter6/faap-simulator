package simulator.allocation

import org.scalatest.FunSuite
import simulator.Mapping.Mapping
import simulator.TestGraphs.SimpleFaapLocationBoundProblemInstance2._
import simulator.evaluation.DefaultMappingEvaluator
import simulator.{FaapProblemInstance, Mapping}


class GreedyCollocationAllocatorTest extends FunSuite {

  private def assertNoConstraintsViolated(faapProblemInstance: FaapProblemInstance, mapping: Mapping): Unit = {
    assert(DefaultMappingEvaluator.evaluateMapping(faapProblemInstance, mapping).constraintsViolated.noConstraintsViolated)
  }

  test("GreedyBorderCollocationAllocator should compute correct allocation") {
    val faapProblemInstance = simpleFaapLocationBoundProblemInstance

    val mapping = GreedyCollocationAllocator.runAllocation(faapProblemInstance)

    assert(mapping == optimalMapping)
    assertNoConstraintsViolated(faapProblemInstance, mapping)
  }

  test("GreedyBorderCollocationAllocator should compute correct allocation other location bound config") {
    val lbMapping = Some(Mapping((appNode1, fogNode1), (appNode2, fogNode5)))
    val faapProblemInstance = FaapProblemInstance(0, appGraph, fogGraph, lbMapping)

    val mapping = GreedyCollocationAllocator.runAllocation(faapProblemInstance)

    assert(mapping == Mapping((appNode1, fogNode1), (appNode3, fogNode1), (appNode4, fogNode2), (appNode2, fogNode5)))
    assertNoConstraintsViolated(faapProblemInstance, mapping)
  }
}
