package simulator.generation

import GraphDefinition.FogNode
import scalax.collection.Graph
import scalax.collection.edge.WUnDiEdge

/**
  * Trait for the generation of fog graphs of different classes
  */
trait FogGraphGenerator extends GraphGenerator {
  def generateFogGraph: Graph[FogNode, WUnDiEdge]
}

case class RandomFogGraphGenerator(config: FogGraphGenConfig) extends FogGraphGenerator {
  implicit val graphConfig: GraphGenBase = config.graphGenBase

  def generateFogGraph: Graph[FogNode, WUnDiEdge] = {
    Graph.fill(config.scale)(WUnDiEdge(FogNode.generateRandomly, FogNode.generateRandomly)(GlobalConfig.random.nextDouble()))
  }
}

case class ConnectedFogGraphGenerator(config: ConnectedGraphGenConfig) extends FogGraphGenerator {
  implicit val graphConfig: GraphGenBase = config.graphGenBase

  def generateFogGraph: Graph[FogNode, WUnDiEdge] = {
    GraphGenerator.generateConnectedGraph(config, FogNode.generateRandomly, WUnDiEdge)
  }
}

case class CactusFogGraphGenerator(config: CactusGraphGenConfig) extends FogGraphGenerator {
  implicit val graphConfig: GraphGenBase = config.graphGenBase

  def generateFogGraph: Graph[FogNode, WUnDiEdge] = {
    GraphGenerator.generateCactusGraph[FogNode, WUnDiEdge, Graph](config, FogNode.generateRandomly, WUnDiEdge)
  }
}

case class ABBControlAppFogGraphGenerator(config: ABBControlAppFogGraphGenConfig) extends FogGraphGenerator {
  implicit val graphConfig: GraphGenBase = config.graphGenBase

  def generateFogGraph: Graph[FogNode, WUnDiEdge] = {
    import config.lbNodesCount

    val cactusConfig = CactusGraphGenConfig(config.scale, config.cycleTreeRatio, config.treeCountRatio, config.cycleCountRatio, config.graphGenBase)
    val baseCactusGraph = GraphGenerator.generateCactusGraph[FogNode, WUnDiEdge, Graph](cactusConfig, FogNode.generateRandomly, WUnDiEdge)

    val weights = ABBControlApplicationGraphGen.Weights(true)
    val lbFogNodes = Array.fill(lbNodesCount)(FogNode.next(weights.sensor + weights.actuator).setDescription("s")).toList
    val connectingFogNodes = Array.fill(lbNodesCount)(Utils.sampleFromSet(baseCactusGraph.nodes.toOuter)).toList

    val newEdges = (lbFogNodes, connectingFogNodes).zipped.map( (f1, f2) => WUnDiEdge(f1, f2)(graphConfig.fogLinkWeightGen()))

    baseCactusGraph ++ newEdges
  }
}

case class ConnectedSparsenessFogGraphGenerator(config: ConnectedDensityGraphGenConfig) extends FogGraphGenerator {
  implicit val graphConfig: GraphGenBase = config.graphGenBase

  def generateFogGraph: Graph[FogNode, WUnDiEdge] = {
    GraphGenerator.generateConnectedDensityGraph[FogNode, WUnDiEdge](config, FogNode.generateRandomly, WUnDiEdge)
  }
}

case class StaticFogGraphGenerator(fogGraph: Graph[FogNode, WUnDiEdge]) extends FogGraphGenerator {
  override def generateFogGraph: Graph[FogNode, WUnDiEdge] = fogGraph
}

