package simulator.allocation

import org.scalatest.FunSuite
import simulator.Mapping.Mapping
import simulator.{FaapProblemInstance, Mapping}
import simulator.TestGraphs.VerySimpleFaapProblemInstance._
import simulator.evaluation.DefaultMappingEvaluator


class GreedyBorderAllocatorTest extends FunSuite {

  private def assertNoConstraintsViolated(faapProblemInstance: FaapProblemInstance, mapping: Mapping): Unit = {
    assert(DefaultMappingEvaluator.evaluateMapping(verySimpleFaapProblemInstance, mapping).constraintsViolated.noConstraintsViolated)
  }

  test("GreedyBorderAllocator should without location-bound nodes and not violate any constraints") {
    val mapping = GreedyBorderAllocator.runAllocation(verySimpleFaapProblemInstance)

    println(mapping)
    assertNoConstraintsViolated(verySimpleFaapProblemInstance, mapping)
  }

  test("GreedyBorderAllocator should compute correct allocation") {
    val faapProblemInstance = verySimpleFaapLocationBoundProblemInstance

    val mapping = GreedyBorderAllocator.runAllocation(faapProblemInstance)

    assert(mapping == Mapping((appNode1, fogNode1), (appNode2, fogNode4), (appNode3, fogNode4)))
    assertNoConstraintsViolated(faapProblemInstance, mapping)
  }

  test("GreedyBorderAllocator should compute correct allocation with all nodes location bound") {
    val lbMapping = Some(Mapping((appNode1, fogNode1), (appNode3, fogNode4), (appNode2, fogNode2)))
    val faapProblemInstance = FaapProblemInstance(0, appGraph, fogGraph, lbMapping)

    val mapping = GreedyBorderAllocator.runAllocation(faapProblemInstance)

    assert(mapping == Mapping((appNode1, fogNode1), (appNode2, fogNode2), (appNode3, fogNode4)))
    assertNoConstraintsViolated(faapProblemInstance, mapping)
  }

  test("GreedyBorderAllocator should fail with invalid location-bound mapping (node constraint violated)") {
    val lbMapping = Some(Mapping((appNode1, fogNode1), (appNode2, fogNode1), (appNode3, fogNode1)))
    assertThrows[IllegalArgumentException] {
      FaapProblemInstance(0, appGraph, fogGraph, lbMapping)
    }
  }
}
