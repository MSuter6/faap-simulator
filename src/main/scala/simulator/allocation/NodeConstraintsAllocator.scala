package simulator.allocation

import simulator.Mapping.Mapping
import simulator.{FaapProblemInstance, Mapping}

/**
  * Implementation of the Node Constraints Allocator (NCA) (Thesis 3.3.2.1)
  */
@SerialVersionUID(-1737213758814364652l)
object NodeConstraintsAllocator extends Allocator {
  override val policyName = "Node constraints policy"
  override val policyDescription = "Cycle through nodes and assign app to fog node if node constraint not violated"

  override def runAllocation(faapProblemInstance: FaapProblemInstance): Mapping = {
    Utils.check(faapProblemInstance.latencyPaths.isEmpty, s"$policyName doesn't support latency path constraints")

    val fogNodeRemainingResources = collection.mutable.ListBuffer(faapProblemInstance.fogGraph.nodes.toSeq.sortBy(-_.cpuConstraint).map(fogNode => (fogNode.toOuter, fogNode.cpuConstraint)): _*)
    val appNodeList = faapProblemInstance.appGraph.nodes.toList.sortBy(-_.resourceUsage)

    faapProblemInstance.locationBoundMapping.foreach(_.foreach{ case (appNode, fogNode) =>
      val index = fogNodeRemainingResources.indexWhere(_._1 == fogNode)

      fogNodeRemainingResources.update(index, (fogNode, fogNodeRemainingResources(index)._2 - appNode.resourceUsage))
    })


    val mappingList = for (node <- appNodeList) yield {
      val mappedFogNode = faapProblemInstance.locationBoundMapping.getOrElse(Mapping.empty).getOrElse(node.value, {
        val index = fogNodeRemainingResources.indexWhere(_._2 >= node.resourceUsage)
        if (index == -1) {
          throw new AllocationException(s"No node with enough capacity left (needed capacity: ${node.resourceUsage}")
        }
        val fogNode = fogNodeRemainingResources(index)
        fogNodeRemainingResources.update(index, (fogNode._1, fogNode._2 - node.resourceUsage))

        fogNode._1
      })

      node.toOuter -> mappedFogNode // Just cycle through node list without respect to constraints
    }

    mappingList.toMap
  }
}