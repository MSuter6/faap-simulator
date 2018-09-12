package simulator.generation

import GraphDefinition.AppNode
import scalax.collection.Graph
import scalax.collection.edge.WDiEdge

/**
  * Trait which provides an interface to generate application graph of different classes and configurations.
  * An application graph has nodes of the AppNode class and weighted, directed edges [[scalax.collection.edge.WDiEdge]].
  */
trait ApplicationGraphGenerator extends GraphGenerator {
  def generateApplicationGraph: Graph[AppNode, WDiEdge]
}

case class RandomApplicationGraphGenerator(config: ApplicationGraphGenConfig) extends ApplicationGraphGenerator {
  implicit val graphConfig: GraphGenBase = config.graphGenBase

  def generateApplicationGraph: Graph[AppNode, WDiEdge] = {
    Graph.fill(config.scale)(WDiEdge(AppNode.generateRandomly, AppNode.generateRandomly)(GlobalConfig.random.nextDouble()))
  }
}

case class ConnectedApplicationGraphGenerator(config: ConnectedGraphGenConfig) extends ApplicationGraphGenerator {
  implicit val graphConfig: GraphGenBase = config.graphGenBase

  def generateApplicationGraph: Graph[AppNode, WDiEdge] = {
    GraphGenerator.generateConnectedGraph(config, AppNode.generateRandomly, WDiEdge)
  }
}

case class DAGApplicationGraphGenerator(config: DAGGraphGenConfig) extends ApplicationGraphGenerator {
  implicit val graphConfig: GraphGenBase = config.graphGenBase

  def generateApplicationGraph: Graph[AppNode, WDiEdge] = {
    val dag = GraphGenerator.generateDirectedConnectedAcyclicGraph(config, AppNode.generateRandomly, WDiEdge)
    assert(!dag.isCyclic,  "Generated DAG should not be cyclic")
//    assert(dag.isConnected,  "Generated DAG should be connected")
    dag
  }
}

case class CactusApplicationGraphGenerator(config: CactusGraphGenConfig) extends ApplicationGraphGenerator {
  implicit val graphConfig: GraphGenBase = config.graphGenBase

  def generateApplicationGraph: Graph[AppNode, WDiEdge] = {
    GraphGenerator.generateCactusGraph[AppNode, WDiEdge, Graph](config, AppNode.generateRandomly, WDiEdge)
  }
}

case class SpdgApplicationGraphGenerator(config: SeriesParallelDecomposableGraphGenConfig) extends ApplicationGraphGenerator {
  implicit val graphConfig: GraphGenBase = config.graphGenBase

  def generateApplicationGraph: Graph[AppNode, WDiEdge] = {
    GraphGenerator.generateSeriesParallelDecomposableGraph[AppNode, WDiEdge](config, AppNode.generateRandomly, WDiEdge)
  }
}

case class ConnectedSparsenessApplicationGraphGenerator(config: ConnectedDensityGraphGenConfig) extends ApplicationGraphGenerator {
  implicit val graphConfig: GraphGenBase = config.graphGenBase

  def generateApplicationGraph: Graph[AppNode, WDiEdge] = {
    GraphGenerator.generateConnectedDensityGraph[AppNode, WDiEdge](config, AppNode.generateRandomly, WDiEdge)
  }
}

case class MapReduceApplicationGraphGenerator(config: MapReduceGenConfig) extends ApplicationGraphGenerator {
  implicit val graphConfig: GraphGenBase = config.graphGenBase

  def generateApplicationGraph: Graph[AppNode, WDiEdge] = {
    GraphGenerator.generateMapReduceGraph[AppNode, WDiEdge](config, AppNode.generateRandomly, WDiEdge)
  }
}

case class ABBControlApplicationGraphGenerator(config: ABBControlApplicationGraphGenConfig) extends ApplicationGraphGenerator {
  def generateApplicationGraph: Graph[AppNode, WDiEdge] = {
    ABBControlApplicationGraphGen.generateABBControlApplicationGraph[WDiEdge](config, WDiEdge)
  }
}