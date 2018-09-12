
package simulator.generation

import GlobalConfig.random
import GraphDefinition.{AppNode, FogNode}
import experiments.ExperimentCommon._
import scalax.collection.Graph
import scalax.collection.edge.{WDiEdge, WUnDiEdge}
import simulator.Mapping.Mapping

/**
  * @param percentage How many of all app nodes are location-bound
  */
case class LocationBoundMappingConfig(percentage: Double = defaultLocationBoundMappingRatio)

/**
  * Generates location-bound nodes randomly by choosing random app nodes and random fog nodes with enough capacity
  * to map them on (Thesis Section 4.1.1.4)
  */
trait LocationBoundMappingGenerator {
  def generateLocationBoundMappings(appGraph: Graph[AppNode, WDiEdge], fogGraph: Graph[FogNode, WUnDiEdge]): Mapping

  protected def generateMappingForFixedNodes(fogGraph: Graph[FogNode, WUnDiEdge], locationBoundNodes: collection.Set[AppNode]): Mapping = {
    val fogNodeMap = collection.mutable.Map(fogGraph.nodes.map(fogNode => (fogNode, fogNode.cpuConstraint)).toList: _*)

    val mapping: Mapping = locationBoundNodes.map(a => {
      val suitableFogNodes = fogNodeMap.filter(_._2 >= a.resourceUsage)
      if (suitableFogNodes.isEmpty) {
        throw new GeneratorException("Configuration doesn't allow for location-bound nodes (capacity exceeded for all nodes)")
      }
      val randIdx = random.nextInt(suitableFogNodes.size)
      val fogNode = suitableFogNodes.toList(randIdx)
      fogNodeMap(fogNode._1) -= a.resourceUsage
      a -> fogNode._1.toOuter
    }).toMap
    mapping
  }
}

case class DefaultLocationBoundMappingGenerator(config: LocationBoundMappingConfig) extends LocationBoundMappingGenerator {
  /**
    * @return Mapping of location-bound nodes
    */
  override def generateLocationBoundMappings(appGraph: Graph[AppNode, WDiEdge], fogGraph: Graph[FogNode, WUnDiEdge]): Mapping = {
    if (config.percentage < 0 || config.percentage > 1) {
      throw new IllegalArgumentException(f"Invalid location-bound node-percentage given: ${config.percentage}, should be in [0,1]")
    }

    val nodeCount = math.floor(appGraph.nodes.size * config.percentage).toInt

    val appNodes = random.shuffle(appGraph.nodes).take(nodeCount).map(_.value)

    val mapping: Mapping = generateMappingForFixedNodes(fogGraph, appNodes)

    mapping
  }
}

case class SourceSinkLocationBoundMappingGenerator() extends LocationBoundMappingGenerator {
  override def generateLocationBoundMappings(appGraph: Graph[AppNode, WDiEdge], fogGraph: Graph[FogNode, WUnDiEdge]): Mapping = {
    // Check if start node exists
    val sourceNodes = appGraph.nodes.filter(_.incoming.isEmpty)
    val sinkNodes = appGraph.nodes.filter(_.outgoing.isEmpty)

    val locationBoundNodes = (sourceNodes ++ sinkNodes).map(_.toOuter)

    val mapping: Mapping = generateMappingForFixedNodes(fogGraph, locationBoundNodes)

    mapping
  }
}

/**
  * Generates the location-bound mapping for the ABB Control App (Thesis Section 4.1.2.2).
  * Uses the node description to identify location-bound nodes.
  */
case class ABBControlAppLocationBoundMappingGenerator() extends LocationBoundMappingGenerator {
  override def generateLocationBoundMappings(appGraph: Graph[AppNode, WDiEdge], fogGraph: Graph[FogNode, WUnDiEdge]): Mapping = {

    val outerAppNodes = appGraph.nodes.toOuter
    val sensors = outerAppNodes.filter(_.description.get.contains("sensor"))
    val sensActNodes = sensors.map(s => {
      val index = s.description.get.drop(6).toInt
      val actor = outerAppNodes.find(_.description.get.contains(f"act$index")).getOrElse({
        throw new GeneratorException("Invalid ABB Control app, there should be a actuator for every sensor")
      })
      (s, actor)
    })

    val lbFogNodes = fogGraph.nodes.toOuter.filter(f => f.description.isDefined && f.description.get == "s")

    assert(sensActNodes.size == lbFogNodes.size)

    (lbFogNodes, sensActNodes).zipped.flatMap((lb, sensAct) => Seq(sensAct._1 -> lb, sensAct._2 -> lb)).toMap
  }
}