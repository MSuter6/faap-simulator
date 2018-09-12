package simulator.allocation

import simulator.FaapProblemInstance
import simulator.Mapping.Mapping
import simulator.evaluation.DefaultMappingEvaluator

import scala.util.Try


case class MultiTryAllocator(alloc: Allocator, tries: Int = 10) {
  def runAllocation(faapProblemInstance: FaapProblemInstance): Option[Mapping] = {
    val mappingCosts = (for (_ <- 0 until tries) yield {
      val mapping = Try(alloc.runAllocation(faapProblemInstance))

      if (mapping.isFailure) {
        None
      } else {
        val result = DefaultMappingEvaluator.evaluateMapping(faapProblemInstance, mapping.get)

        Some((mapping.get, result.cost.value))
      }
    }).flatten
    if (mappingCosts.isEmpty) None
    else Some(mappingCosts.minBy(_._2)._1)
  }
}

@SerialVersionUID(4323756788414874586l)
object CombinedAllocator extends Allocator {
  override val policyName = "Combined Heuristics"
  override val policyDescription = "Combine all three heuristics and let the GBA and GCA run multiple times Heuristics "

  override def runAllocation(faapProblemInstance: FaapProblemInstance): Mapping = {
    val gca = MultiTryAllocator(GreedyCollocationAllocator).runAllocation(faapProblemInstance)
    val gba = MultiTryAllocator(GreedyBorderAllocator).runAllocation(faapProblemInstance)
    val nca = Try(NodeConstraintsAllocator.runAllocation(faapProblemInstance)).toOption
    Seq(gca, gba, nca).flatten.map(m => (m, DefaultMappingEvaluator.evaluateMapping(faapProblemInstance, m).cost.value)).minBy(_._2)._1
  }
}

object GreedyCollocationMultiTryAllocator extends Allocator {
  override val policyName = "Collocation MultiTry Heuristics"
  override val policyDescription = "Heuristics run multiple times to minimise cost while respecting node and location-bound constraints"

  override def runAllocation(faapProblemInstance: FaapProblemInstance): Mapping = {
    val mapping = MultiTryAllocator(GreedyCollocationAllocator).runAllocation(faapProblemInstance)
    if (mapping.isEmpty) {
      throw new AllocationException(s"Multitry failed")
    }
    mapping.get
  }
}

object GreedyBorderMultiTryAllocator extends Allocator {
  override val policyName = "Border MultiTry Heuristics"
  override val policyDescription = "Heuristics run multiple times to minimise cost while respecting node and location-bound constraints"

  override def runAllocation(faapProblemInstance: FaapProblemInstance): Mapping = {
    val mapping = MultiTryAllocator(GreedyBorderAllocator).runAllocation(faapProblemInstance)
    if (mapping.isEmpty) {
      throw new AllocationException(s"Multitry failed")
    }
    mapping.get
  }
}
