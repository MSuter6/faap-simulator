package simulator.evaluation

import GlobalConfig._
import Utils.boolColor
import experiments.{DefaultPlotItem, PlotItem}

trait MappingEvaluationResult {
  val cost: PlotItem
  val constraintsViolated: ConstraintsCheckResult

  def mkString: String = {
    f"Cost: ${cost.value}$NL$constraintsViolated"
  }

  def mkConsoleString: String = {
    f"Cost: ${cost.value}$NL${constraintsViolated.consoleString}"
  }
}

object DefaultMappingEvaluationResult {
  def apply(cost: Double, constraintsViolated: ConstraintsCheckResult): MappingEvaluationResult = new DefaultMappingEvaluationResult(DefaultPlotItem(cost), constraintsViolated)
}

/**
  * Represents the result of the mapping evaluation of the basic FAAP model. Is returned by the [[simulator.evaluation.DefaultMappingEvaluator]]
  * @param cost Cost as [[experiments.PlotItem]]
  * @param constraintsViolated Infomation about constraints check [[simulator.evaluation.ConstraintsCheckResult]]
  */
case class DefaultMappingEvaluationResult(cost: PlotItem, constraintsViolated: ConstraintsCheckResult) extends MappingEvaluationResult

/**
  *
  * @param validMapping True if mapping is invalid
  * @param nodeConstraints True if node capacity constraints are violated by mapping
  * @param edgeConstraints True if edge capacity constraints are violated by mapping (always false as basic model does not support edge constraints)
  * @param pathConstraints True if latency path constraints are violated by mapping
  * @param locationBoundMappingConstraints True if loction-bound mapping constraints are violated by mapping
  */
case class ConstraintsCheckResult(validMapping: Boolean, nodeConstraints: Boolean, edgeConstraints: Boolean, pathConstraints: Boolean, locationBoundMappingConstraints: Boolean) {
  override def toString: String = {
    f"Mapping is invalid (not all app nodes mapped on fog nodes): $validMapping$NL" +
      f"Node constraints violated: $nodeConstraints$NL" +
      f"Edge constraints violated: $edgeConstraints$NL" +
      f"Path constraints violated: $pathConstraints$NL" +
      f"Location-bound mapping constraints violated: $locationBoundMappingConstraints$NL"
  }

  def consoleString: String = {
    f"Mapping is invalid (not all app nodes mapped on fog nodes): ${boolColor(validMapping)}$NL" +
      f"Node constraints violated: ${boolColor(nodeConstraints)}$NL" +
      f"Edge constraints violated: ${boolColor(edgeConstraints)}$NL" +
      f"Path constraints violated: ${boolColor(pathConstraints)}$NL" +
      f"Location-bound mapping constraints violated: ${boolColor(locationBoundMappingConstraints)}$NL"
  }

  def noConstraintsViolated: Boolean = !validMapping && !nodeConstraints && !edgeConstraints && !pathConstraints && !locationBoundMappingConstraints
}