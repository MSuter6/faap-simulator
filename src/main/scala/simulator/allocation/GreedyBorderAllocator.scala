package simulator.allocation

import GraphDefinition.AppNode
import simulator.Mapping.Mapping
import simulator.{FaapProblemInstance, Mapping}

import scala.collection.mutable

/**
  * Implementation of the Greedy Boder Allocator (GBA) (Thesis 3.3.2.3)
  */
@SerialVersionUID(5948925250741370531l)
object GreedyBorderAllocator extends Allocator {
  override val policyName = "Greedy Border Heuristics"
  override val policyDescription = "Heuristics traversing app graph greedily by descending link weight"

  case class AllocState(faapProblemInstance: FaapProblemInstance) extends GreedyAllocatorState {

    var mapping: Mapping = faapProblemInstance.locationBoundMapping match {
      case Some(lb: Mapping) => lb
      case None =>
        val (appNode, fogNode) = getRandomMappingDisjointToCurrent(Mapping.empty)
        Mapping((appNode, fogNode.toOuter))
    }

    mapping.foreach{ case (appNode, fogNode) =>
      occupyResourcesOn(faapProblemInstance.fogGraph.get(fogNode), appNode.resourceUsage)
    }

    val EdgeOrdering: Ordering[(AppNode, faapProblemInstance.appGraph.EdgeT)] = Ordering.by((x: (AppNode, faapProblemInstance.appGraph.EdgeT)) => x._2.weight).reverse

    private var currentNodes = mutable.PriorityQueue.empty[(GraphDefinition.AppNode, faapProblemInstance.appGraph.EdgeT)](EdgeOrdering)

    mapping.keys.foreach(node => {
      faapProblemInstance.appGraph.get(node).edges.foreach(edge => currentNodes += ((node, edge)))
    })

    def nodeAlreadyMapped(end: AppNode): Boolean = {
      val isAlreadyMapped = mapping.keys.toSet.contains(end)
      if (isAlreadyMapped && currentNodes.nonEmpty) {
        currentNodes.dequeue
      }

      isAlreadyMapped
    }

    def addBorderNode(end: faapProblemInstance.appGraph.NodeT): Unit = {
      val nextEdges = end.edges.map(edge => (end.toOuter, edge))
      currentNodes = currentNodes ++ nextEdges
    }

    def getNextMappedFogNode = mapping(currentNodes.dequeue._1)

    def getRandomMappingDisjointToCurrent(mapping: Mapping): (AppNode, faapProblemInstance.fogGraph.NodeT) = {
      val randomOuterAppNodeFromOtherPartition = faapProblemInstance.appGraph.nodes.toOuter.diff(mapping.keys.toSet).head

      val mappedFogNode = fogNodeCpuRemainingResources.find(randomOuterAppNodeFromOtherPartition.resourceUsage <= _._2)
      if (mappedFogNode.isEmpty){
        throw new AllocationException(s"No node with enough capacity left (needed capacity: $randomOuterAppNodeFromOtherPartition")
      }

      (randomOuterAppNodeFromOtherPartition, mappedFogNode.get._1)
    }

    def getNextAdjacentNode: faapProblemInstance.appGraph.NodeT = {
      if (currentNodes.isEmpty) {
        // Disconnected and disconnected part has no location bound node, take random from other partition
        val (appNode, fogNode) = getRandomMappingDisjointToCurrent(mapping)

        val innerAppNode = faapProblemInstance.appGraph.get(appNode)

        addBorderNode(innerAppNode)
        addMapping(appNode, fogNode)

        innerAppNode
      } else {
        currentNodes.head._2.find(_.toOuter != currentNodes.head._1).getOrElse({
          currentNodes.head._2._1
        })
      }
    }
  }

  override def runAllocation(faapProblemInstance: FaapProblemInstance): Mapping = {
    Utils.check(faapProblemInstance.latencyPaths.isEmpty, s"$policyName doesn't support latency path constraints")

    val allocState = AllocState(faapProblemInstance)

    while (!allocState.allNodesMapped) {
      val end = allocState.getNextAdjacentNode

      if (!allocState.nodeAlreadyMapped(end)) { // TODO: can't this be made easier?
        val nextMappedFogNode = allocState.getClosesestAvailableFogNode(end.resourceUsage, allocState.getNextMappedFogNode)

        allocState.addMapping(end.toOuter, nextMappedFogNode)

        allocState.addBorderNode(end)
      }
    }
    allocState.getMapping
  }
}
