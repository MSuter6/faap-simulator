package simulator.allocation

import simulator.FaapProblemInstance
import simulator.Mapping.Mapping

trait Allocator extends Serializable {
  val policyName: String
  val policyDescription: String

  def runAllocation(faapProblemInstance: FaapProblemInstance): Mapping
}
