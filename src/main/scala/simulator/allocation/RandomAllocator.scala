package simulator.allocation

import simulator.Mapping.Mapping
import simulator.{FaapProblemInstance, Mapping}

/**
  * Completely random allocator which only respects location-bound node constraints.
  */
object RandomAllocator extends Allocator {
  override val policyName = "Random policy"
  override val policyDescription = "Cycle through nodes and assign app to fog node sequentially (not respecting constraints)"

  override def runAllocation(faapProblemInstance: FaapProblemInstance): Mapping = {
    val fogNodeList = faapProblemInstance.fogGraph.nodes.toSeq
    val appNodeList = faapProblemInstance.appGraph.nodes.toSeq

    val nodeMap = appNodeList.map(node => {
      val mappedFogNode = faapProblemInstance.locationBoundMapping.getOrElse(Mapping.empty).getOrElse(node.value, {
        Utils.sampleFromSeq(fogNodeList).value
      })

      node.toOuter -> mappedFogNode // Just cycle through node list without respect to constraints
    }).toMap

    nodeMap
  }
}