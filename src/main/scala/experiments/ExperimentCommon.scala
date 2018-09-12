package experiments

import GlobalConfig.random
import io.PathHandler
import io.visualization.PlotWriterConfig
import simulator.FaapProblemInstanceRepetitions
import simulator.Mapping.Mapping
import simulator.allocation.Solver.Solver
import simulator.allocation._
import simulator.evaluation.{MappingEvaluationResult, MappingEvaluator}
import simulator.generation.{CactusFogGraphGenerator, DoubleGen, SpdgApplicationGraphGenerator}

/**
  * This object contains all default values for the experiments.
  */
object ExperimentCommon {
  val defaultSolverTimeLimit: Int = 10000

  val defaultRepetitionCount = 100

  def defaultAllocators(pathHandler: PathHandler, solver: Solver = Solver.Gurobi): Seq[Allocator] = Seq(OptimalAllocator(SolverAllocatorConfig(pathHandler, solver))) ++ heuristics

  val heuristics = Seq(NodeConstraintsAllocator, GreedyBorderAllocator, GreedyCollocationAllocator)

  val defaultAppGraphOrder = 14
  val defaultFogGraphOrder = 7
  val defaultFogNodeCapacity: DoubleGen = () => random.nextDouble
  val defaultAppNodeResourceUsage: DoubleGen = () => random.nextDouble / 3
  val defaultAppLinkCapacity: DoubleGen = () => random.nextDouble / 2
  val defaultFogLinkCapacity: DoubleGen = () => 1

  val defaultLocationBoundMappingRatio = 0.1

  object DefaultFogGraphGenerator {
    val defaultCycleTreeRatio = 0.6
    val defaultTreeCountRatio = 0.2
    val defaultCycleCountRatio = 0.2
  }

  val defaultFogGraphType = CactusFogGraphGenerator

  object DefaultAppGraphGenerator {
    val defaultRangeSplitter: Int => (Int, Int) = (n: Int) => (math.floor(n.toDouble / 2).toInt, math.ceil(n.toDouble / 2).toInt)
    val defaultParallelSerialRatio = 0.5
  }

  val defaultAppGraphType = SpdgApplicationGraphGenerator
}

case class ExperimentConfig(id: String, seed: Int, allocators: Seq[Allocator], evalMethod: MappingEvaluator, plotWriterConfig: PlotWriterConfig) {
  def using(newConfig: PlotWriterConfig): ExperimentConfig = {
    ExperimentConfig(id, seed, allocators, evalMethod, newConfig)
  }

  def joining(newAllocators: Seq[Allocator]): ExperimentConfig = {
    ExperimentConfig(id, seed, (allocators ++ newAllocators).distinct, evalMethod, plotWriterConfig)
  }
}

/**
  * Represents a plot entry with a value and a toString method which prints the value up to a 6 decimal digits.
  */
trait PlotItem {
  val value: Double

  override def toString = f"$value%1.6f"
}

case class DefaultPlotItem(value: Double) extends PlotItem

/**
  * Time in naneseconds
  */
case class Timestamp(time: Double) extends PlotItem {

  def toSeconds: Double = time / 1000000000.0

  /**
    * Returns the value of the timestamp in seconds
    */
  override val value: Double = toSeconds
}

object Timestamp {
  def fromSeconds(timeInSeconds: Double) = Timestamp(timeInSeconds * 1000000000.0)
}

/**
  * Contains various timestamps of an experiment
  *
  * @param allocationComputeTime Time to compute allocation, also called runtime
  * @param evaluationTime        Time to compute evaluation.
  */
case class ExperimentTimestamps(allocationComputeTime: Timestamp, evaluationTime: Timestamp) {
  def mkString: String = {
    s"""Time to compute allocation: ${allocationComputeTime.toSeconds}
       |Time to compute evaluation: ${evaluationTime.toSeconds}
     """.stripMargin
  }

  def shortString: String = {
    f"allocation time: ${allocationComputeTime.toSeconds}, evaluation time: ${evaluationTime.toSeconds}"
  }
}

object ExperimentTimestamps {
  def apply(allocationComputeTime: Double, evaluationTime: Double): ExperimentTimestamps = new ExperimentTimestamps(Timestamp(allocationComputeTime), Timestamp(evaluationTime))

  def fromSeconds(allocationComputeTime: Double, evaluationTime: Double): ExperimentTimestamps = {
    new ExperimentTimestamps(Timestamp.fromSeconds(allocationComputeTime), Timestamp.fromSeconds(evaluationTime))
  }
}

/**
  * Represents a result from an experiment
  *
  * @param mapping          The resulting mapping from app to fog nodes
  * @param evaluationResult The results from the constraint checks
  * @param timestamps       Various timestamps
  */
case class ExperimentResult(mapping: Mapping,
                            evaluationResult: MappingEvaluationResult,
                            timestamps: Option[ExperimentTimestamps] = None) {
  def isValidResult: Boolean = evaluationResult.constraintsViolated.noConstraintsViolated
}

/**
  * An experiment instance including the allocator
  *
  * @param allocator                      To solve the instances of this repetitions (same config)
  * @param faapProblemInstanceRepetitions Different instances with same config
  */
case class ExperimentInstance(allocator: Allocator,
                              faapProblemInstanceRepetitions: FaapProblemInstanceRepetitions) {
  val id = f"${faapProblemInstanceRepetitions.description}_${allocator.policyName}"
}

/**
  * Contains the instance and the results of the allocation
  */
case class ExperimentIteration(experimentInstance: ExperimentInstance, experimentResults: Seq[Option[ExperimentResult]]) {
  def this(experimentInstance: ExperimentInstance, experimentResult: Option[ExperimentResult]) {
    this(experimentInstance, Seq(experimentResult))
  }
}

/**
  * Basic experiment configuration
  *
  * @param xAxisRange  Range of variable (shown on xAxis on plots)
  * @param repetitions Number of repetitions.
  */
case class FaapProblemInstancesConfiguration(xAxisRange: Range, repetitions: Int = ExperimentCommon.defaultRepetitionCount)

object FaapProblemInstancesConfiguration {
  def apply(scale: Int, repetitions: Int): FaapProblemInstancesConfiguration = new FaapProblemInstancesConfiguration(scale to scale, repetitions)
}

/**
  * Helper trait for experiment naming
  */
trait ExperimentNaming {
  /**
    * @return Specific name/id of this experiment
    */
  def expName: String

  /**
    * @param expType Experiment type/class (e.g. scale)
    * @return Path handler of this experiment.
    */
  def getPathHandler(expType: String): PathHandler = PathHandler.expTypeWithName(expType, expName)
}