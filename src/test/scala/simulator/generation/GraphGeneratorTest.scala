package simulator.generation

import GraphDefinition.{AppNode, FogNode}
import io.dot.DotApplicationGraphGenerator
import io.{ExperimentExporter, PathHandler}
import org.scalatest.FunSuite

import scala.language.higherKinds
import scalax.collection.Graph
import scalax.collection.GraphPredef.EdgeLikeIn
import scalax.collection.edge.{WDiEdge, WUnDiEdge}
import scala.reflect.ClassTag

/**
  * Tests the various graph generators [[simulator.generation.GraphGenerator]]
  */
class GraphGeneratorTest extends FunSuite {
  implicit val graphConfigBase: GraphGenBase = GraphGenBase()

  def hasNoSelfLoops[N, E[X] <: EdgeLikeIn[X]](graph: Graph[N, E])(implicit edgeTag: ClassTag[E[N]], nodeTag: ClassTag[N]): Boolean = {
    graph.edges.forall(edge => edge._1.toOuter != edge._2.toOuter)
  }

  val scales = Seq(2, 3, 4, 5, 10, 50, 100, 500, 1000, 2000, 5000)

  private def testScale(generatedGraph: Graph[AppNode, WDiEdge], scale: Int) = {
    assert(generatedGraph.order == scale)
  }

  test("testGenerateSeriesParallelDecomposableGraphScale") {
    scales.foreach(scale => {
      val generatedGraph = GraphGenerator.generateSeriesParallelDecomposableGraph[AppNode, WDiEdge](SeriesParallelDecomposableGraphGenConfig(scale),
        AppNode.generateRandomly,
        WDiEdge)
      testScale(generatedGraph, scale)
      assert(hasNoSelfLoops(generatedGraph))
    })
  }

  test("testGenerateCactusGraphScale") {
    scales.foreach(scale => {
      val generatedGraph = GraphGenerator.generateCactusGraph[AppNode, WDiEdge, Graph](CactusGraphGenConfig(scale), AppNode.generateRandomly, WDiEdge)

      testScale(generatedGraph, scale)
    })
  }

  test("testGenerateCactusGraphPropertiesOneCycle") {
    scales.foreach(scale => {
      val config = CactusGraphGenConfig(scale, 1, 1, 1)

      val generatedGraph = GraphGenerator.generateCactusGraph[AppNode, WDiEdge, Graph](config, AppNode.generateRandomly, WDiEdge)

      assert(generatedGraph.findCycle().isDefined)
      assert(generatedGraph.findCycle().get.nodes.toSet == generatedGraph.nodes) // Graph is one cycle
      assert(generatedGraph.isConnected)
    })
  }

  test("testGenerateCactusGraphPropertiesOnlyTree") {
    scales.foreach(scale => {
      val config = CactusGraphGenConfig(scale, 0, 1, 1)
      val generatedGraph = GraphGenerator.generateCactusGraph[AppNode, WDiEdge, Graph](config, AppNode.generateRandomly, WDiEdge)

      assert(generatedGraph.isAcyclic)
      assert(generatedGraph.isConnected)
    })
  }

  test("testGenerateConnectedSparseness") {
    val size = 10

    (1 to 10).foreach(scale => {

      val sparseness = scale / 10
      val config = ConnectedDensityGraphGenConfig(size, sparseness)
      val generatedGraph = GraphGenerator.generateConnectedDensityGraph[FogNode, WUnDiEdge](config, FogNode.generateRandomly, WUnDiEdge)

      assert(generatedGraph.isConnected)
      assert(generatedGraph.order == size)
    })
  }

  test("testGenerateConnectedSparsenessFullyConnectedDirected") {
    def assertIsFullyConnected[N, E[X] <: EdgeLikeIn[X]](graph: Graph[N, E])(implicit edgeTag: ClassTag[E[N]], nodeTag: ClassTag[N]): Unit = {

      val expectedNeighborCount = (graph.order - 1)*2 // Edge to each node except itself

      graph.nodes.foreach(node => {
        assert(node.outDegree + node.inDegree == expectedNeighborCount)
      })
    }

    (1 to 100 by 20).foreach(scale => {
      val config = ConnectedDensityGraphGenConfig(scale, 1) // Fully connected
      val generatedGraph = GraphGenerator.generateConnectedDensityGraph[AppNode, WDiEdge](config, AppNode.generateRandomly, WDiEdge) // Directed
      assert(generatedGraph.order == scale)
      println(generatedGraph.graphSize)
      assert(hasNoSelfLoops(generatedGraph))
      assertIsFullyConnected(generatedGraph)
    })
  }

  test("testGenerateConnectedSparsenessSpanningTreeUndirected") {
    (2 to 100 by 20).foreach(scale => {
      val config = ConnectedDensityGraphGenConfig(scale, 0)
      val generatedGraph = GraphGenerator.generateConnectedDensityGraph[FogNode, WUnDiEdge](config, FogNode.generateRandomly, WUnDiEdge)
      assert(generatedGraph.order == scale)
      assert(generatedGraph.graphSize == scale-1)
      assert(hasNoSelfLoops(generatedGraph))
      assert(generatedGraph.isAcyclic)
    })
  }

  test("testGenerateConnectedSparsenessSpanningTreeDirected") {
    (2 to 100 by 20).foreach(scale => {
      val config = ConnectedDensityGraphGenConfig(scale, 0)
      val generatedGraph = GraphGenerator.generateConnectedDensityGraph[AppNode, WDiEdge](config, AppNode.generateRandomly, WDiEdge)
      assert(generatedGraph.order == scale)
      assert(generatedGraph.graphSize == scale-1)
      assert(hasNoSelfLoops(generatedGraph))
      assert(generatedGraph.isAcyclic)
    })
  }

  test("testGenerateMapReduceGraph") {
    (12 to 12 by 200).foreach(scale => {
      val pathHandler = PathHandler.getTest("graphgen")
      pathHandler.getExp.clear()

      val config = MapReduceGenConfig(scale)
      val generatedGraph = GraphGenerator.generateMapReduceGraph[AppNode, WDiEdge](config, AppNode.generateRandomly, WDiEdge)

      val appGraphPath = pathHandler.getExp/"appGraphMapRed.dot"
      val appGraphPathPng = pathHandler.getExp/"appGraphMapRed.png"


      import better.files.Dsl.SymbolicOperations

      appGraphPath < DotApplicationGraphGenerator.generateDotRepresentation(generatedGraph)
      ExperimentExporter(pathHandler).runDotPngCommand(appGraphPath.path, appGraphPathPng.path)

      // Check if start node exists
      assert(generatedGraph.nodes.count(_.incoming.isEmpty) == 1)
      // Check if end node exists
      assert(generatedGraph.nodes.count(_.outgoing.isEmpty) == 1)

      assert(generatedGraph.order == scale)
      assert(hasNoSelfLoops(generatedGraph))
      assert(generatedGraph.isAcyclic)
    })
  }

  test("testUseCaseGraphGeneration") {
    (1 to 2 by 1).foreach(n => {
      val pathHandler = PathHandler.getTest("graphgen")
      pathHandler.getExp.clear()

      val config = ABBControlApplicationGraphGenConfig(n, wcet = true)
      val generatedGraph = ABBControlApplicationGraphGenerator(config).generateApplicationGraph

      val appGraphPath = pathHandler.getExp/"appGraphABBCtrl.dot"
      val appGraphPathPng = pathHandler.getExp/"appGraphABBCtrl.png"

      import better.files.Dsl.SymbolicOperations

      appGraphPath < DotApplicationGraphGenerator.generateDotRepresentation(generatedGraph)
      ExperimentExporter(pathHandler).runDotPngCommand(appGraphPath.path, appGraphPathPng.path)

      val expectedOrder = config.expectedAppGraphOrder
      assert(expectedOrder == generatedGraph.order, s"Check if correct graph order")
      val expectedGraphSize = config.expectedGraphSize
      assert(expectedGraphSize == generatedGraph.graphSize, "Check if correct graph size (#edges)")

      assert(hasNoSelfLoops(generatedGraph))
      assert(generatedGraph.isConnected)
    })
  }
}
