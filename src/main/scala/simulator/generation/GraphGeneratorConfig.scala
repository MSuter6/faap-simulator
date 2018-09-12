package simulator.generation

import experiments.ExperimentCommon.DefaultAppGraphGenerator._
import experiments.ExperimentCommon.DefaultFogGraphGenerator._
import experiments.ExperimentCommon._
import scalax.collection.generator.NodeDegreeRange
import simulator.generation.ABBControlApplicationGraphGenConfig.WeightClass
import simulator.generation.ABBControlApplicationGraphGenConfig.WeightClass.WeightClass

/**
  * This file includes the case classes for all graph generator configurations.
  * For further descriptions of the
  */

trait GraphConfig {}

case class GraphGenBase(appNodeConstraintGen: DoubleGen = defaultAppNodeResourceUsage,
                        fogNodeConstraintGen: DoubleGen = defaultFogNodeCapacity,
                        appLinkWeightGen: DoubleGen = defaultAppLinkCapacity,
                        fogLinkWeightGen: DoubleGen = defaultFogLinkCapacity)

trait ScaleGraphGenConfig extends GraphConfig{
  def scale: Int
  def graphGenBase: GraphGenBase
}

case class ApplicationGraphGenConfig(scale: Int, graphGenBase: GraphGenBase = GraphGenBase()) extends ScaleGraphGenConfig
case class FogGraphGenConfig(scale: Int, graphGenBase: GraphGenBase = GraphGenBase()) extends ScaleGraphGenConfig

case class ConnectedGraphGenConfig(scale: Int, nodeDegreeRange: NodeDegreeRange = NodeDegreeRange(2, 8), graphGenBase: GraphGenBase = GraphGenBase()) extends GraphConfig

case class ConnectedDensityGraphGenConfig(scale: Int, density: Double, graphGenBase: GraphGenBase = GraphGenBase()) extends GraphConfig {
  assert(density >= 0 && density <= 1, "Sparseness should lie between 0 and 1 (connected to fully connected)")
}

case class MapReduceGenConfig(scale: Int,
                              graphGenBase: GraphGenBase = GraphGenBase()) extends GraphConfig {
  assert(scale >= 4, "A MapReduce graph must have at least 4 nodes (start-, map-, reduce- and end-node)")
}

case class DAGGraphGenConfig(minPerRank: Int = 2,
                             maxPerRank: Int = 5,
                             minRanks: Int = 2,
                             maxRanks: Int = 6,
                             edgePercentage: Double = 0.15,
                             graphGenBase: GraphGenBase = GraphGenBase()) extends GraphConfig

case class CactusGraphGenConfig(scale: Int,
                                cycleTreeRatio: Double = defaultCycleTreeRatio,
                                treeCountRatio: Double = defaultTreeCountRatio,
                                cycleCountRatio: Double = defaultCycleCountRatio,
                                graphGenBase: GraphGenBase = GraphGenBase()) extends GraphConfig

case class ABBControlAppFogGraphGenConfig(scale: Int, lbNodesCount: Int,
                                          cycleTreeRatio: Double = defaultCycleTreeRatio,
                                          treeCountRatio: Double = defaultTreeCountRatio,
                                          cycleCountRatio: Double = defaultCycleCountRatio,
                                          cycleSizeVariance: Double = 0.2,
                                          treeSizeVariance: Double = 0.2,
                                          graphGenBase: GraphGenBase = GraphGenBase())
case class SeriesParallelDecomposableGraphGenConfig(scale: Int,
                                                    rangeSplitter: Int => (Int, Int) = defaultRangeSplitter,
                                                    parallelSerialRatio: Double = defaultParallelSerialRatio,
                                                    graphGenBase: GraphGenBase = GraphGenBase()) extends GraphConfig

/**
  * Functionality for the computation of graph orders and node weights for the ABB Control App
  * @param n Number of sensor-actuator blocks (graph order = n * 7 + 2)
  * @param wcet If true: worst case weights are used, else: avg weights
  */
case class ABBControlApplicationGraphGenConfig(n: Int, wcet: Boolean) extends GraphConfig {
  /** Number of nodes in app graph */
  def expectedAppGraphOrder: Int = n * 7 + 3

  /** Number of edges in app graph */
  def expectedGraphSize: Int = n * 10 + 2

  def maximalNeededNodeCapacity: Double = ABBControlApplicationGraphGen.Weights(wcet).maxNodeWeight

  def expectedAverageFogNodeCapacity: Double = {
    val avgWeight = {
      def rangeAvg(range: (Int, Int)) = (range._1.toDouble + range._2) / 2
      rangeAvg(WeightClass.smallRange) * WeightClass.smallP +
        rangeAvg(WeightClass.mediumRange) * WeightClass.mediumP +
        rangeAvg(WeightClass.largeRange) * WeightClass.largeP
    }
    avgWeight - (avgWeight * WeightClass.utilizationFactor)
  }

  def expectedTotalAppNodeUsage: Double = {
    val weights = ABBControlApplicationGraphGen.Weights(wcet)
    weights.blockNodeWeights.sum * n + (weights.aggr1 + weights.aggr2 + weights.globalCtrl)
  }
}

object ABBControlApplicationGraphGenConfig {

  /**
    * Enum which hold the three different weight classes (small, medium and large) and provides functionality to sample
    * weights form those classes.
    */
  object WeightClass extends Enumeration {
    type WeightClass = Value
    val Small, Medium, Large = Value

    val smallP = 0.6
    val mediumP = 0.2
    val largeP = 0.2
    assert((smallP+mediumP+largeP).toInt == 1)

    val smallRange: (Int, Int) = (50,200)
    val mediumRange: (Int, Int) = (500,2000)
    val largeRange: (Int, Int) = (4000,10000)

    val utilizationFactor = 0.5

    def sampleClass(): ABBControlApplicationGraphGenConfig.WeightClass.Value = {
      val p = GlobalConfig.random.nextDouble()
      if (p < smallP) {
        Small
      } else if (p < mediumP) {
        Medium
      } else {
        Large
      }
    }

    def generateSmallWeight: Int = Utils.sampleFromRange(smallRange._1, smallRange._2)

    def generateMediumWeight: Int = Utils.sampleFromRange(mediumRange._1, mediumRange._2)

    def generateLargeWeight: Int = Utils.sampleFromRange(largeRange._1, largeRange._2)

    def generateFogNodeWeight(weightClassOpt: Option[WeightClass]): Double = {
      val weight = if (weightClassOpt.isEmpty) {
        generateFogNodeWeight(Some(sampleClass()))
      } else {
        val weightClass = weightClassOpt.get
        weightClass match {
          case Small => generateSmallWeight
          case Medium => generateMediumWeight
          case Large => generateLargeWeight
        }
      }
      weight - (weight * utilizationFactor)
    }
  }

  def generateFogNodeWeight(weightClass: Option[WeightClass]): Double = WeightClass.generateFogNodeWeight(weightClass)

  def generateFogNodeWeight(): Double = generateFogNodeWeight(None)
}
