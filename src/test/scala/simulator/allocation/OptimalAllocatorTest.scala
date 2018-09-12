package simulator.allocation

import io.PathHandler
import io.solver.LpSolverExecutor
import org.scalatest.FunSuite
import simulator.evaluation.DefaultMappingEvaluator

class OptimalAllocatorTest extends FunSuite {

  import simulator.TestGraphs.VerySimpleFaapProblemInstance._

  test("Optimal Allocator should work without location-bound nodes") {
    assume(LpSolverExecutor.isScipInstalled, "Test could not be conducted, SCIP not installed")

    val pathHandler = PathHandler.getTest("optwithoudlb")
    pathHandler.getExp.clear()

    OptimalAllocator(SolverAllocatorConfig(pathHandler, Solver.SCIP)).runAllocation(verySimpleFaapProblemInstance)
  }

  test("Optimal Gurobi Allocator should compute mapping correctly") {
    assume(LpSolverExecutor.isGurobiInstalled, "Test could not be conducted, Gurobi not installed")
    import simulator.TestGraphs.SimpleFaapLocationBoundProblemInstance._


    val pathHandler = PathHandler.getTest("optsimple")
    pathHandler.getExp.clear()

    val mapping = OptimalAllocator(SolverAllocatorConfig(pathHandler, Solver.Gurobi)).runAllocation(simpleFaapLocationBoundProblemInstance)

    val expectedMapping = optimalMapping


    assert(mapping.toSet == expectedMapping.toSet)
  }

  test("Optimal SCIP Allocator should compute mapping correctly") {
    assume(LpSolverExecutor.isScipInstalled, "Test could not be conducted, SCIP not installed")

    import simulator.TestGraphs.SimpleFaapLocationBoundProblemInstance._

    val pathHandler = PathHandler.getTest("optsimplescip")
    pathHandler.getExp.clear()

    val mapping = OptimalAllocator(SolverAllocatorConfig(pathHandler, Solver.SCIP)).runAllocation(simpleFaapLocationBoundProblemInstance)

    val expectedMapping = optimalMapping

    println(DefaultMappingEvaluator.evaluateMapping(simpleFaapLocationBoundProblemInstance, mapping))

    println(mapping)
    println(expectedMapping)
    assert(mapping.toSet == expectedMapping.toSet)
  }

  test("Optimal SCIP Allocator should not find mapping with latency paths") {
    assume(LpSolverExecutor.isScipInstalled, "Test could not be conducted, SCIP not installed")

    import simulator.TestGraphs.SimpleInfeasibleFaapLocationBoundLatencyPathProblemInstance._

    val pathHandler = PathHandler.getTest("optlatencypathinfeasible")
    pathHandler.getExp.clear()

    assertThrows[LpSolverFailedToFindSolutionException] {
      OptimalAllocator(SolverAllocatorConfig(pathHandler, Solver.SCIP)).runAllocation(simpleFaapLocationBoundProblemInstance)
    }
  }

  test("Optimal SCIP Allocator should find optimal mapping with latency paths") {
    assume(LpSolverExecutor.isScipInstalled, "Test could not be conducted, SCIP not installed")

    import simulator.TestGraphs.SimpleFeasibleFaapLocationBoundLatencyPathProblemInstance._

    val pathHandler = PathHandler.getTest("optlatencypathfeasible")
    pathHandler.getExp.clear()

    val mapping = OptimalAllocator(SolverAllocatorConfig(pathHandler, Solver.SCIP)).runAllocation(simpleFaapLocationBoundProblemInstance)

    val expectedMapping = optimalMapping

    println(DefaultMappingEvaluator.evaluateMapping(simpleFaapLocationBoundProblemInstance, mapping))

    println(mapping)
    println(expectedMapping)
    assert(mapping.toSet == expectedMapping.toSet)
  }
}
