package simulator.allocation

import GraphDefinition.GraphNode
import better.files.File
import experiments.ExperimentCommon.defaultSolverTimeLimit
import io.PathHandler
import io.solver.{LpSolutionParser, LpSolverExecutor, ZimplWriter}
import simulator.FaapProblemInstance
import simulator.Mapping.Mapping
import simulator.allocation.Solver.Solver

import scala.language.implicitConversions


/**
  * Configuration of the Solver Allocator
  * @param pathHandler Path handler to write the solver results to
  * @param solver Which [[simulator.allocation.Solver]] to use
  * @param timeLimit Time limit for solver (i.e. abort solving process after timeLimit seconds)
  * @param formulation Which ZimplWrite to use (e.g. use relaxation variant instead of normal formulation)
  */
case class SolverAllocatorConfig(pathHandler: PathHandler,
                                 solver: Solver,
                                 timeLimit: Int = defaultSolverTimeLimit,
                                 formulation: ZimplWriter = ZimplWriter())

/**
  * Enum which represents a Solver (either SCIP or Gurobi) and allows to run the solver
  */
object Solver extends Enumeration {
  type Solver = Value
  val SCIP, Gurobi = Value

  class SolverExecutor(solver: Value) {
    def run(lpFile: File, lpSolutionFile: File, timeLimit: Int): Unit = solver match {
      case SCIP => LpSolverExecutor.runScip(lpFile, lpSolutionFile, timeLimit)
      case Gurobi => LpSolverExecutor.runGurobi(lpFile, lpSolutionFile, timeLimit)
    }
  }

  implicit def value2Exec(solver: Value): SolverExecutor = new SolverExecutor(solver)
}

/**
  * Compute the optimal solution to a FAAP-Instance using an ILP solver. (Thesis 3.3.1)
  * @param optimalAllocatorConfig Configuration
  */
@SerialVersionUID(-7937808335353616419l)
case class OptimalAllocator(optimalAllocatorConfig: SolverAllocatorConfig) extends Allocator {
  import optimalAllocatorConfig._

  override val policyName: String = f"${optimalAllocatorConfig.formulation.name} $solver"
  override val policyDescription: String = "This allocator uses an optimal allocation strategy by formulating the faap problem as ILP and using an exact solver"

  override def runAllocation(faapProblemInstance: FaapProblemInstance): Mapping = {
    val appGraphOrder = faapProblemInstance.appGraph.order
    val fogGraphOrder = faapProblemInstance.fogGraph.order
    if (fogGraphOrder >= GlobalConfig.zimplLimit || appGraphOrder >= GlobalConfig.zimplLimit) {
      throw new LpSolverFailedToFindSolutionException(f"Instance scale was to large for zimpl to run (app Order: $appGraphOrder, fog Order: $fogGraphOrder, limit: ${GlobalConfig.zimplLimit})")
    }
    val zimplFile = pathHandler.getZimplFile(faapProblemInstance, optimalAllocatorConfig)
    val lpFile = pathHandler.getLpFile(faapProblemInstance, optimalAllocatorConfig)
    val lpSolutionFile = pathHandler.getLpSolutionFile(faapProblemInstance, optimalAllocatorConfig)

    if (!lpFile.exists){
      val wasSuccess = generateAndWriteZimpleFile(faapProblemInstance, zimplFile)

      if (!wasSuccess){
        throw new LpSolverFailedToFindSolutionException("Zimpl was not able to run for the instance")
      }
    }
    if (!lpSolutionFile.exists){
      solver.run(lpFile, lpSolutionFile, timeLimit)
    }

    println(s"Parsing solution from ${lpSolutionFile.pathAsString}")

    LpSolutionParser.parseLpSolution(lpSolutionFile, faapProblemInstance, optimalAllocatorConfig)
  }

  /**
    * Generates the ZIMPL files to the specified path based on the faapProblemInstance
    * @return True if the file generation succeeded.
    */
  private def generateAndWriteZimpleFile(faapProblemInstance: FaapProblemInstance, zimplFile: File) = {
    val costSeq: Seq[(GraphNode, GraphNode, Double)] = GraphDefinition.getShortestPathCosts(faapProblemInstance.fogGraph)

    val zimplWriter = optimalAllocatorConfig.formulation

    zimplWriter.appNodes(faapProblemInstance.appGraph.nodes.toOuter)
    zimplWriter.fogNodes(faapProblemInstance.fogGraph.nodes.toOuter)

    zimplWriter.appEdges(faapProblemInstance.appGraph.edges.toOuter)
    zimplWriter.fogEdges(costSeq.map(c => (c._1, c._2)))

    zimplWriter.computationalCost(faapProblemInstance.appGraph.nodes.toOuter)
    zimplWriter.dataBandwidth(faapProblemInstance.appGraph.edges.toOuter.toSeq)

    zimplWriter.cpuCapacity(faapProblemInstance.fogGraph.nodes.toOuter)

    zimplWriter.closestPathCost(costSeq)

    if (faapProblemInstance.latencyPaths.isDefined) {
      zimplWriter.latencyPaths(faapProblemInstance.latencyPaths.get)
    }

    if (faapProblemInstance.locationBoundMapping.isDefined) {
      zimplWriter.locationBound(faapProblemInstance.locationBoundMapping.get)
    }

    zimplWriter.appendCostAndConstraints

    zimplWriter.writeToFile(zimplFile)

    LpSolverExecutor.runZimpl(zimplFile)
  }
}
