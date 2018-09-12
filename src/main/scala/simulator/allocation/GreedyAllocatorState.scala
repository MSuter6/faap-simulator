package simulator.allocation

import GraphDefinition.{AppNode, FogNode}
import scalax.collection.GraphTraversal.BreadthFirst
import simulator.FaapProblemInstance
import simulator.Mapping.Mapping

import scala.collection.mutable

/**
  * Holds the state of the greedy heuristics with elements such as the remaining capacities of the fog node, the current mapping etc.
  */
trait GreedyAllocatorState {
  implicit val ordering = Ordering[Double].reverse

  val faapProblemInstance: FaapProblemInstance

  val appNodes: Set[AppNode] = faapProblemInstance.appGraph.nodes.toOuter

  protected var mapping: Mapping

  val fogNodeCpuRemainingResources: mutable.Map[faapProblemInstance.fogGraph.NodeT, BigDecimal] = collection.mutable.Map(faapProblemInstance.fogGraph.nodes.toSeq.sortBy(_.cpuConstraint).map(fogNode => (fogNode, fogNode.cpuConstraint)): _*)

  def getMapping: Mapping = mapping

  def occupyResourcesOn(fogNode: faapProblemInstance.fogGraph.NodeT, resourceUsage: BigDecimal): Unit = {
    assert(fogNodeCpuRemainingResources(fogNode) >= resourceUsage, s"Fog node has not enough free resources: ${fogNodeCpuRemainingResources(fogNode)} needed $resourceUsage}")
    fogNodeCpuRemainingResources.put(fogNode, fogNodeCpuRemainingResources(fogNode) - resourceUsage)
  }

  def addMapping(appNode: AppNode, fogNode: faapProblemInstance.fogGraph.NodeT): Unit = {
    occupyResourcesOn(fogNode, appNode.resourceUsage)

    mapping += (appNode -> fogNode.toOuter)
  }

  def allNodesMapped: Boolean = mapping.keys.toSet == appNodes


  def getClosesestAvailableFogNode(neededResources: BigDecimal, fogNode: FogNode): faapProblemInstance.fogGraph.NodeT = {
    val startOuterFogNode = faapProblemInstance.fogGraph.get(fogNode)

    if (fogNodeCpuRemainingResources(startOuterFogNode) >= neededResources) { // Check if there is enough space on same fog node
      startOuterFogNode // Closest is same fog node

    } else { // If not, find closest successor
      val randomOrder = faapProblemInstance.fogGraph.nodes.map(_ -> GlobalConfig.random.nextInt()).toMap
      val randomOrdering = faapProblemInstance.fogGraph.NodeOrdering((n1, n2) => randomOrder(n1) - randomOrder(n2))
      val adjacentNode = startOuterFogNode.withKind(BreadthFirst).withOrdering(randomOrdering).findSuccessor(fogNodeCpuRemainingResources(_) >= neededResources).getOrElse(
        // TODO: Somewhat strange -> check if really closest fog node is chosen (which respects cpu constraints)
        throw new AllocationException(s"No node with enough capacity left (needed capacity: $neededResources")
      )
      adjacentNode
    }
  }
}
