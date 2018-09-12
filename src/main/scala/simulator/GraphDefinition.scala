import scalax.collection.Graph
import scalax.collection.edge.WUnDiEdge
import simulator.generation.GraphGenBase

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * Basic data structures of the graph such as fog and app nodes
  */
package object GraphDefinition {

  trait GraphNode {
    def id: Int

    def getIdString: String

    val resourceConstraint: BigDecimal
  }

  case class FogNode(id: Int, cpuConstraint: BigDecimal) extends GraphNode {
    cpuConstraint.setScale(GlobalConfig.doublePrecision, BigDecimal.RoundingMode.HALF_UP)
    private val charId = 'f'

    def toFullString: String = {
      f"$charId${id}_$cpuConstraint%1.2f"
    }

    var description: Option[String] = None
    def setDescription(description: String): FogNode = {
      this.description = Some(description)
      this
    }

    override def toString: String = f"$charId$id"

    override def getIdString: String = f"$charId$id"

    override val resourceConstraint: BigDecimal = cpuConstraint
  }

  case class AppNode(id: Int, resourceUsage: BigDecimal) extends GraphNode {
    resourceUsage.setScale(GlobalConfig.doublePrecision, BigDecimal.RoundingMode.HALF_UP)

    private val charId = 'a'
    def toFullString: String = description.getOrElse(f"$charId${id}_$resourceUsage%1.2f")

    var description: Option[String] = None
    def setDescription(description: String): AppNode = {
      this.description = Some(description)
      this
    }

    override def toString = f"$charId$id"

    override def getIdString: String = f"$charId$id"

    override val resourceConstraint: BigDecimal = resourceUsage
  }

  /**
    * Companion object which allows generate new app nodes and keeps track of the node id's
    */
  object AppNode {
    private var current = 0

    def next(weight: Double): AppNode = {
      AppNode(nextId, BigDecimal.valueOf(weight))
    }

    def generateRandomly(implicit graphConfig: GraphGenBase): AppNode = {
      val weight = graphConfig.appNodeConstraintGen()
      AppNode(nextId, BigDecimal.valueOf(weight))
    }

    private def nextId = {
      current += 1
      current
    }

    def reset(): Unit = current = 0
  }

  /**
    * Companion object which allows generate new fog nodes and keeps track of the node id's
    */
  object FogNode {
    private var current = 0

    def generateRandomly(implicit graphConfig: GraphGenBase): FogNode = {
      val weight = graphConfig.fogNodeConstraintGen()
      FogNode(nextId, BigDecimal.valueOf(weight))
    }

    private def nextId = {
      current += 1
      current
    }

    def next(weight: Double): FogNode = {
      FogNode(nextId, BigDecimal.valueOf(weight))
    }

    def reset(): Unit = current = 0
  }

  /**
    * Computes all shortest pahts between all fog node pairs of the fog graph, cubic time complexity.
    * @param fogGraph Fog graph to use.
    * @return List with all fog node pairs and the number of hops for the shortest path on the fogGraph.
    */
  def getShortestPathCosts(fogGraph: Graph[FogNode, WUnDiEdge]): ListBuffer[(GraphNode, GraphNode, Double)] = {
    val nodeSeq = fogGraph.nodes.toSeq.sortBy(t => t.id).toIndexedSeq
    var costSeq = mutable.ListBuffer.empty[(GraphNode, GraphNode, Double)]

    for {i <- nodeSeq.indices} {
      for (m <- i until nodeSeq.size) {
        val n1 = nodeSeq(i)
        val n2 = nodeSeq(m)
        lazy val pathLength = n1.shortestPathTo(n2).get.edges.size.toDouble
        costSeq += ((n1, n2, pathLength))
      }
    }
    costSeq
  }
}
