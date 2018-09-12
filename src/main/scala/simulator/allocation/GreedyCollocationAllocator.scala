package simulator.allocation

import GraphDefinition.AppNode
import scalax.collection.edge.WDiEdge
import simulator.Mapping.Mapping
import simulator.{FaapProblemInstance, Mapping}

/**
  * Implementation of the Greedy Collocation Allocator (GCA) (Thesis 3.3.2.2)
  */
@SerialVersionUID(7489295270139115659l)
object GreedyCollocationAllocator extends Allocator {
  override val policyName = "Collocation Heuristics"
  override val policyDescription = "Heuristics to minimize cost while respecting node and location-bound constraints"

  case class GreedyCollocationAllocatorState(faapProblemInstance: FaapProblemInstance) extends GreedyAllocatorState {
    def getUnmappedNodes: Set[AppNode] = faapProblemInstance.appGraph.nodes.toOuter.diff(mapping.keys.toSet)

    val sortedEdges: Seq[WDiEdge[AppNode]] = faapProblemInstance.appGraph.edges.toOuter.toSeq.sortBy(_.weight)

    def nodeAlreadyMapped(end: AppNode): Boolean = {
      mapping.keys.toSet.contains(end)
    }

    override var mapping: Mapping = {
      val mapping = faapProblemInstance.locationBoundMapping.getOrElse(Mapping.empty)
      mapping.foreach { case (appNode, fogNode) =>
        occupyResourcesOn(faapProblemInstance.fogGraph.get(fogNode), appNode.resourceUsage)
      }
      mapping
    }

    def findSuitableFogNode(edge: WDiEdge[AppNode]): Seq[(AppNode, Option[faapProblemInstance.fogGraph.NodeT])] = {
      val n1 = edge._1
      val n2 = edge._2
      val mapping1Exists = mapping.isDefinedAt(n1)
      val mapping2Exists = mapping.isDefinedAt(n2)

      if (!mapping1Exists && !mapping2Exists) { // Both edge nodes have not yet been mapped -> try to collocate
        val neededResources = n1.resourceUsage + n2.resourceUsage
        val fogNode = findSuitableFogNode(neededResources)

        Seq(n1 -> fogNode, n2 -> fogNode)
      } else if (mapping1Exists && !mapping2Exists) { // Only node 1 is mapped -> map node 2
        Seq(n2 -> Some(getClosesestAvailableFogNode(faapProblemInstance.appGraph.get(n2).resourceUsage, mapping(n1))))
      } else if (mapping2Exists && !mapping1Exists) { // Only node 2 is mapped -> map node 1
        Seq(n1 -> Some(getClosesestAvailableFogNode(faapProblemInstance.appGraph.get(n1).resourceUsage, mapping(n2))))
      } else {
        Seq()
      }
    }

    def findSuitableFogNode(node: AppNode): Option[faapProblemInstance.fogGraph.NodeT] = {
      findSuitableFogNode(node.resourceUsage)
    }

    private def findSuitableFogNode(neededResources: BigDecimal) = {
      val suitableFogNodes = fogNodeCpuRemainingResources.filter(_._2 >= neededResources)

      if (suitableFogNodes.isEmpty) {
        None
      } else {
        Some(suitableFogNodes.maxBy(_._2)._1)
      }
    }
  }

  override def runAllocation(faapProblemInstance: FaapProblemInstance): Mapping = {
    Utils.check(faapProblemInstance.latencyPaths.isEmpty, s"$policyName doesn't support latency path constraints")

    val allocState = GreedyCollocationAllocatorState(faapProblemInstance)

    for (edge <- allocState.sortedEdges) {
      // try to collocate edge on the fog node with most capacity left
      val fogNodes = allocState.findSuitableFogNode(edge)
      // add to mapping
      fogNodes.foreach {
        case (appNode: AppNode, Some(fogNode)) => allocState.addMapping(appNode, fogNode)
        case _ => // Do nothing if no suitable node is found..
      }
    }

    val unmappedNodes = allocState.getUnmappedNodes
    // forall unmapped app nodes (should only be isolated nodes)
    unmappedNodes.foreach(unmappedNode => {
      // map app node to fog node with most capacity left if possible
      val fogNode = allocState.findSuitableFogNode(unmappedNode)
      if (fogNode.isDefined) {
        fogNode.foreach(allocState.addMapping(unmappedNode, _))
      } else {
        // else: heuristics failed
        throw new HeuristicsFailedToFindSolutionException(s"Not enough capacity left to map node: ${unmappedNode.toFullString}")
      }
    })

    allocState.mapping
  }
}
