package simulator.generation

import GlobalConfig.random
import GraphDefinition.AppNode
import scalax.collection.Graph
import scalax.collection.edge.{WDiEdge, WUnDiEdge}
import simulator.Mapping.Mapping
import simulator.{LatencyPath, LatencyPaths}

import scala.collection.mutable.ListBuffer

trait LatencyPathGenerator {
  def generateLatencyPaths(appGraph: Graph[GraphDefinition.AppNode, WDiEdge], fogGraph: Graph[GraphDefinition.FogNode, WUnDiEdge], locationBoundMapping: Mapping): LatencyPaths
}

/**
  * Generates latency paths according to the scheme from Section 4.2.1.3
  */
case class DefaultLatencyPathGenerator(latencyPathConfig: LatencyPathConfiguration) extends LatencyPathGenerator {
  override def generateLatencyPaths(appGraph: Graph[GraphDefinition.AppNode, WDiEdge], fogGraph: Graph[GraphDefinition.FogNode, WUnDiEdge], locationBoundMapping: Mapping): LatencyPaths = {

    import latencyPathConfig.maxLength

    assert(!appGraph.isCyclic, "Latency Path Generator can't work with cyclic app graphs.")
//    assert(appGraph.edges.size >= latencyPathConfig.count)

    val appNodes = appGraph.nodes.filter(_.outNeighbors.nonEmpty).toSeq
    val paths = ListBuffer[List[AppNode]]()

    while(paths.distinct.size < latencyPathConfig.count) {
      var current = Utils.sampleFromSeq(appNodes)
      val nodeSeq = ListBuffer(current.toOuter)
      while (current.outNeighbors.nonEmpty && nodeSeq.size <= maxLength) {
        current = Utils.sampleFromSet(current.outNeighbors)
        nodeSeq += current.toOuter
      }

      paths += nodeSeq.toList
    }

    LatencyPaths(paths.toList.distinct.map(nodeSeq => LatencyPath(nodeSeq, latencyPathConfig.latencyGenerator(nodeSeq.size))):_*)
  }
}

/**
  * @param count Number of latency paths for a [[simulator.FaapProblemInstance]]
  * @param maxLength The maximal length of the latency path
  * @param latencyGenerator The generator of the latency limit (dependent on the respective path length)
  */
case class LatencyPathConfiguration(count: Int, maxLength: Int = Int.MaxValue, latencyGenerator: Int => Int = LatencyPathConfiguration.defaultLatencyGenerator) {
}


object LatencyPathConfiguration {
  /**
    * defaultLatencyGenerator returns a randomly sampled value from N(pathlength, 3).
    */
  val defaultLatencyGenerator: (Int) => Int = (pathLength: Int) => Utils.drawGaussian(random, pathLength, 3)
}