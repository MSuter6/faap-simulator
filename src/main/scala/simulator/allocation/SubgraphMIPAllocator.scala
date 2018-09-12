package simulator.allocation

import GraphDefinition.AppNode
import io.PathHandler
import scalax.collection.Graph
import scalax.collection.edge.WDiEdge
import simulator.FaapProblemInstance
import simulator.Mapping.Mapping

/**
  * Allocator tailored for the ABB Control Application.
  * Breaks down problem into subproblems dividing it into the preprocessing blocks and solving them individually
  * with the optimal solver. (This approach leads to an insufficient performance)
  */
case class SubgraphMIPAllocator(pathHandler: PathHandler) extends Allocator {
  override val policyName = "Subgraph ILP Allocator"
  override val policyDescription: String = "Specialised for ABB Control app: Divide app graph into subgraphs (preprocessing blocks)" +
    "and map those optimally one by one"

  override def runAllocation(faapProblemInstance: FaapProblemInstance): Mapping = {
    Utils.check(faapProblemInstance.latencyPaths.isEmpty, s"$policyName doesn't support latency path constraints")

    val config = SolverAllocatorConfig(pathHandler, Solver.SCIP)
    val optimalAllocator = OptimalAllocator(config)
    var currentMapping = faapProblemInstance.locationBoundMapping.get

    def getProblemInstanceForSubgraph(nextSubgraph: Graph[AppNode, WDiEdge]): FaapProblemInstance = {
      FaapProblemInstance(nextSubgraph.hashCode(), nextSubgraph, faapProblemInstance.fogGraph, Some(currentMapping), id = faapProblemInstance.id + faapProblemInstance.repetition)
    }

    def getNextSubgraph: Option[Graph[AppNode, WDiEdge]] = {
      val currentSensor = faapProblemInstance.appGraph.nodes.find(n => {
        n.description.get.contains("sensor") && !currentMapping.isDefinedAt(n.diSuccessors.head.toOuter)
      })
      if (currentSensor.isDefined) {
        val sensor = currentSensor.get
        val scale = currentSensor.get.diSuccessors.head
        val smooth = scale.diSuccessors.head
        val feat = smooth.diSuccessors.head

        val alreadyMappedNodes = currentMapping.map { case (a, _) =>
          faapProblemInstance.appGraph.get(a).toOuter
        }

        Some(Graph.from(alreadyMappedNodes, sensor.edges.map(_.toOuter) ++ scale.edges.map(_.toOuter)
          ++ smooth.edges.map(_.toOuter) ++ feat.edges.map(_.toOuter))
          .asInstanceOf[Graph[AppNode, WDiEdge]])
      } else {
        None
      }
    }

    var nextSubgraph = getNextSubgraph
    while (nextSubgraph.isDefined) {

      val subProblemInstance = getProblemInstanceForSubgraph(nextSubgraph.get)
      val mapping = optimalAllocator.runAllocation(subProblemInstance)

      currentMapping = currentMapping ++ mapping
      nextSubgraph = getNextSubgraph
    }

    val globalInstance = FaapProblemInstance(5000, faapProblemInstance.appGraph, faapProblemInstance.fogGraph,
      Some(currentMapping), id = faapProblemInstance.id + faapProblemInstance.appGraph.order.toString)
    optimalAllocator.runAllocation(globalInstance)
  }
}