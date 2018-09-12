package simulator.generation

import GlobalConfig.random
import Utils.{atLeast, sampleFromSeq}
import scalax.collection.GraphEdge.EdgeCompanionBase
import scalax.collection.GraphPredef.EdgeLikeIn
import scalax.collection.edge.WDiEdge
import scalax.collection.generator.{NodeDegreeRange, RandomGraph}
import scalax.collection.{Graph, GraphLike}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.language.higherKinds
import scala.reflect.ClassTag

trait GraphGenerator {
}

/**
  * Generates a number of different graph types, custom node and edge types can be used
  */
object GraphGenerator {
  /**
    * Generation of a completely random connected graph.
    * @param config Configuration
    * @param nodeGenerator Generation of node (either app of fog nodes).
    * @param edgeType Edge type (either directed of undirected used for FAAP)
    * @return The randomly generated graph.
    */
  def generateConnectedGraph[N, E[X] <: EdgeLikeIn[X]](config: ConnectedGraphGenConfig, nodeGenerator: => N, edgeType: EdgeCompanionBase[E])
                                                      (implicit edgeTag: ClassTag[E[N]], nodeTag: ClassTag[N]): Graph[N, E] = {
    val randomGraph = RandomGraph[N, E, Graph](
      Graph, new RandomGraph.Metrics[N] {
        val order: Int = config.scale
        val nodeDegrees: NodeDegreeRange = config.nodeDegreeRange

        def nodeGen: N = nodeGenerator
      },
      Set[EdgeCompanionBase[E]](edgeType)
    )
    randomGraph.draw
  }

  /**
    * Generates the connected density graph (Thesis Section 4.1.1.3 and Algo 4.3)
    * https://stackoverflow.com/questions/2041517/random-simple-connected-graph-generation-with-given-sparseness
    */
  def generateConnectedDensityGraph[N, E[X] <: EdgeLikeIn[X]](config: ConnectedDensityGraphGenConfig, nodeGenerator: => N, edgeType: EdgeCompanionBase[E])
                                                             (implicit edgeTag: ClassTag[E[N]], nodeTag: ClassTag[N], graphGenBase: GraphGenBase): Graph[N, E] = {
    val edges = mutable.Set.empty[(N, N)]
    val nodes = IndexedSeq.fill(config.scale)(nodeGenerator)
    val nodeSet = nodes.toSet
    val nextNodes = mutable.Set.empty[N]

    if (config.scale <= 1) return Graph.from(nodes, Set.empty[E[N]]) // Edge case with single node graph

    val scale = config.scale.toDouble
    val lscale = scale - 1
    val edgeCount = if (edgeType == WDiEdge) {
      math.ceil(lscale * (1 + config.density * lscale)).toInt // Directed graph -> use n*(n-1) possible edges
    } else {
      math.ceil(lscale * (1 + config.density * (scale / 2 - 1))).toInt // Directed graph -> use n*(n-1)/2 possible edges
    }

    var currentNode = sampleFromSeq(nodes)
    nextNodes += currentNode

    while (nextNodes != nodeSet) { // Random walk to generate unbiased connected graph (spanning tree) with all nodes
      val nextNeighbor = sampleFromSeq(nodes)
      if (!nextNodes.contains(nextNeighbor)) {
        edges += ((currentNode, nextNeighbor))
        nextNodes += nextNeighbor
      }
      currentNode = nextNeighbor
    }

    while (edges.size < edgeCount) { // fill remaining edges randomly to achieve sparseness target
      val n1 = sampleFromSeq(nodes)
      val n2 = sampleFromSeq(nodes.filterNot(n => n == n1))
      edges += ((n1, n2))
    }

    Graph.from(nodes, edges.map(n => generateEdge(edgeType, n._1, n._2)))
  }

  /**
    * Generates a random DAG.
    * Follows scheme from https://stackoverflow.com/questions/12790337/generating-a-random-dag (07.09.2018)
    */
  def generateDirectedConnectedAcyclicGraph[N, E[X] <: WDiEdge[X]](config: DAGGraphGenConfig, nodeGenerator: => N, edgeType: EdgeCompanionBase[E])
                                                                  (implicit edgeTag: ClassTag[E[N]], nodeTag: ClassTag[N], graphGenBase: GraphGenBase): Graph[N, E] = {
    val edges = collection.mutable.Set.empty[E[N]]
    val nodes = collection.mutable.Set.empty[N]

    val ranks = config.minRanks + random.nextInt(config.maxRanks - config.minRanks + 1)

    for (_ <- 0 until ranks) {
      val newNodesCount = config.minPerRank + random.nextInt(config.maxPerRank - config.minPerRank + 1)

      val newNodes = (0 until newNodesCount).map(_ => nodeGenerator).toList

      nodes.foreach(currentNode => {
        val currentNewNodeCount = if ((config.edgePercentage * newNodesCount).toInt < 1) 1 else (config.edgePercentage * newNodesCount).toInt

        edges ++= random.shuffle(newNodes).take(currentNewNodeCount).map(node => {
          generateEdge(edgeType, currentNode, node)
        })
      })

      nodes ++= newNodes
    }
    Graph.from(nodes, edges)
  }

  /**
    * A mapReduce graph consists of a single start and end node and two bipartite groups of nodes in between.
    *
    * @return Generated Graph
    */
  def generateMapReduceGraph[N, E[X] <: WDiEdge[X]](config: MapReduceGenConfig, nodeGenerator: => N, edgeType: EdgeCompanionBase[E])
                                                   (implicit edgeTag: ClassTag[E[N]], nodeTag: ClassTag[N], graphGenBase: GraphGenBase): Graph[N, E] = {
    val startNode = nodeGenerator
    val endNode = nodeGenerator

    val innerNodeCount = (config.scale - 2).toDouble

    val mapNodesCount = atLeast((innerNodeCount * random.nextDouble()).toInt, 1)
    val reduceNodeCount = (innerNodeCount - mapNodesCount).toInt
    assert(reduceNodeCount >= 1)

    val mapNodes = Array.fill(mapNodesCount)(nodeGenerator)
    val reduceNodes = Array.fill(reduceNodeCount)(nodeGenerator)

    val startToMapEdges = mapNodes.map(generateEdge(edgeType, startNode, _))
    val oneEdgeForAllReduceNodes = reduceNodes.map(generateEdge(edgeType, Utils.sampleFromSeq(mapNodes), _))
    val shuffleEdges = mapNodes.flatMap(mapNode => {

      val randomReduceNodes = random.shuffle(reduceNodes.toSeq).take(random.nextInt(if (reduceNodeCount > 1) 1 else reduceNodeCount) + 1)

      randomReduceNodes.map(generateEdge(edgeType, mapNode, _))
    })
    val reduceToEndEdges = reduceNodes.map(generateEdge(edgeType, _, endNode))

    val allNodes = Seq(startNode, endNode) ++ mapNodes ++ reduceNodes
    val allEdges = startToMapEdges ++ oneEdgeForAllReduceNodes ++ shuffleEdges ++ reduceToEndEdges

    Graph.from(allNodes, allEdges)
  }

  /**
    * Generates an SPDG graph (Thesis Section 4.1.1.2 and Algo 4.2)
    */
  def generateSeriesParallelDecomposableGraph[N, E[X] <: WDiEdge[X]](config: SeriesParallelDecomposableGraphGenConfig, nodeGenerator: => N, edgeType: EdgeCompanionBase[E])
                                                                    (implicit edgeTag: ClassTag[E[N]], nodeTag: ClassTag[N], graphGenBase: GraphGenBase): Graph[N, E] = {

    def generateSeriesParallelDecomposableGraphRecursive(nodeGenerator: => N, edgeType: EdgeCompanionBase[E], n: Int): Graph[N, E] = {
      if (n == 0) {
        Graph.from(Set.empty[N], Set.empty[E[N]])
      } else if (n == 1) {
        Graph.from(Set(nodeGenerator), Set.empty[E[N]])
      } else {
        val (n1, n2) = config.rangeSplitter(n)
        if (config.parallelSerialRatio > random.nextDouble()) {
          parallelComposition(generateSeriesParallelDecomposableGraphRecursive(nodeGenerator, edgeType, n1),
            generateSeriesParallelDecomposableGraphRecursive(nodeGenerator, edgeType, n2))
        } else {
          serialComposition(generateSeriesParallelDecomposableGraphRecursive(nodeGenerator, edgeType, n1),
            generateSeriesParallelDecomposableGraphRecursive(nodeGenerator, edgeType, n2))
        }
      }
    }

    def serialComposition(graph1: Graph[N, E], graph2: Graph[N, E]): Graph[N, E] = {
      // Composes nodes serial -> right border nodes from spg1 are connected to left border nodes from spg2
      val rightBorderNodes = graph1.nodes.filter(_.outgoing.isEmpty)
      val leftBorderNodes = graph2.nodes.filter(_.incoming.isEmpty)

      val newEdges = for (r <- rightBorderNodes; l <- leftBorderNodes) yield generateEdge[E, N](edgeType, r, l)
      graph1 ++ graph2 ++ Graph.from(newEdges.flatMap(_.nodeSeq), newEdges)
    }

    def parallelComposition(graph1: Graph[N, E], graph2: Graph[N, E]): Graph[N, E] = {
      // Composes nodes parallel -> no new nodes and edges are created
      graph1 ++ graph2
    }

    generateSeriesParallelDecomposableGraphRecursive(nodeGenerator, edgeType, config.scale)
  }


  /**
    * Generates a cactus graph (Thesis Section 4.1.1.1 and Algo 4.1)
    */
  def generateCactusGraph[N, E[X] <: EdgeLikeIn[X], G[X, Y[Z] <: EdgeLikeIn[Z]] <: Graph[X, Y] with GraphLike[X, Y, G]](config: CactusGraphGenConfig,
                                                                                                                        nodeGenerator: => N,
                                                                                                                        edgeGenerator: EdgeCompanionBase[E])
                                                                                                                       (implicit edgeTag: ClassTag[E[N]],
                                                                                                                        nodeTag: ClassTag[N],
                                                                                                                        graphConfig: GraphGenBase): Graph[N, E] = {
    // TODO: Maybe add some randomness to xxNodesCount and xxSize -> draw from gaussian
    val nodes = (1 to config.scale).map(_ => nodeGenerator).toSet

    val cycleNodesCount = config.scale * config.cycleTreeRatio
    val treeNodeCount = config.scale - cycleNodesCount

    var remainingNodes = nodes
    val allEdges = new ListBuffer[E[N]]


    while (remainingNodes.nonEmpty) {
      if (random.nextDouble() < 0.5) {
        if (cycleNodesCount > 0) {
          val cycleSize = (cycleNodesCount * config.cycleCountRatio).toInt
          val newEdges = generateCycle[N, E](allEdges, remainingNodes, cycleSize, edgeGenerator)
          allEdges ++= newEdges
          remainingNodes = nodes.diff(allEdges.flatMap(_.nodeSeq).toSet)
        }
      } else {
        if (treeNodeCount > 0) {
          val treeSize = (treeNodeCount * config.treeCountRatio).toInt
          val newEdges = generateTree(allEdges, remainingNodes, treeSize, edgeGenerator)
          allEdges ++= newEdges
          remainingNodes = nodes.diff(allEdges.flatMap(_.nodeSeq).toSet)
        }
      }
    }

    Graph.from(allEdges.flatMap(_.nodeSeq), allEdges)
  }

  /**
    * Generates a cycle containing a random current edge.
    */
  def generateCycle[N, E[X] <: EdgeLikeIn[X]](currentEdges: Seq[E[N]], nodes: Set[N], cycleSize: Int, edgeGenerator: EdgeCompanionBase[E])(implicit graphConfig: GraphGenBase): Seq[E[N]] = {
    def atLeast(is: Int, should: Int) = if (is < should) should else is

    val (nodeCount, cycleNodes) = getNodes(currentEdges, nodes, atLeast(cycleSize, 3))

    val comp = edgeGenerator.asInstanceOf[EdgeCompanionBase[E]]

    if (nodeCount <= 1) { // No self loops
      List()
    } else {
      val cycleEdges = (cycleNodes, (cycleNodes drop 1) :+ cycleNodes.head).zipped.map((n1: N, n2: N) => {
        generateEdge(comp, n1, n2)
      })

      cycleEdges
    }
  }

  /**
    * Generates a tree using a random current edge as root.
    * To generate a uniform random tree, the Prüfer Sequence method is used (https://en.wikipedia.org/wiki/Pr%C3%BCfer_sequence).
    * https://www.geeksforgeeks.org/prufer-code-tree-creation/
    */
  def generateTree[N, E[X] <: EdgeLikeIn[X]](currentEdges: Seq[E[N]], nodes: Set[N], treeSize: Int, edgeGenerator: EdgeCompanionBase[E])(implicit graphConfig: GraphGenBase): Seq[E[N]] = {
    val (nodeCount, treeNodes) = getNodes(currentEdges, nodes, treeSize)

    val comp = edgeGenerator.asInstanceOf[EdgeCompanionBase[E]]

    if (nodeCount <= 2) {
      val edge = generateEdge(comp, treeNodes.head, treeNodes.last)
      Seq(edge)
    } else {
      val indexSeq = (0 until nodeCount).toSet
      val prueferSeq = (0 until nodeCount - 3).map(_ => random.nextInt(nodeCount)) :+ 0

      val (_, remainingNodes, edges) = prueferSeq.foldLeft((prueferSeq, indexSeq, List.empty[E[N]]))((triple, _) => {

        val next = triple._2.diff(triple._1.toSet).min

        val edge = generateEdge(comp, treeNodes(triple._1.head), treeNodes(next))
        (triple._1.tail, triple._2 - next, triple._3 :+ edge)
      })

      val lastEdge = generateEdge(comp, treeNodes(remainingNodes.head), treeNodes(remainingNodes.last))

      edges :+ lastEdge
    }
  }

  /**
    * Draws some nodes randomly. If there are already some edges present, the returned nodeseq contains one random already existing node in the first position
    */
  private def getNodes[N, E[X] <: EdgeLikeIn[X]](currentEdges: Seq[E[N]], nodes: Set[N], size: Int): (Int, Seq[N]) = {
    val nodeCount = {
      val tmpCount = if (size < nodes.size) size else nodes.size

      if (tmpCount <= 1) 2 else tmpCount
    }

    if (currentEdges.isEmpty) { // If no edges yet exist take a random node
      if (nodes.size <= 1) {
        throw new RuntimeException("A graph with only one node doesn't make sense")
      }
      (nodeCount, random.shuffle(nodes).take(nodeCount).toSeq)
    } else { // If there are already some edges -> take one node from edges to assure connectedness
      val root = random.shuffle(currentEdges.flatMap(_.nodeSeq)).head
      (nodeCount, root +: random.shuffle(nodes).take(nodeCount - 1).toSeq)
    }
  }
}
