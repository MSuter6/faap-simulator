package io.dot

import GraphDefinition.{AppNode, FogNode}
import io.dot.DotParser.DotParserState.{EDGES, FINISHED, FIRSTLINE}
import scalax.collection.Graph
import scalax.collection.GraphEdge.EdgeCompanionBase
import scalax.collection.GraphPredef.EdgeLikeIn
import scalax.collection.edge.{WDiEdge, WUnDiEdge}
import simulator.generation
import simulator.generation.GraphGenBase

import scala.collection.mutable.ListBuffer
import scala.language.higherKinds
import scala.reflect.ClassTag

/**
  * Parser which allows to parse dot files and return corresponding the [[scalax.collection.Graph]] object.
  */
object DotParser {
  val DIGRAPH = "digraph"
  val GRAPH = "graph"

  class DotParserException(private val message: String = "",
                           private val cause: Throwable = None.orNull) extends Exception(message, cause)

  object DotParserState extends Enumeration {
    type DotParserState = Value
    val FIRSTLINE, EDGES, FINISHED = Value
  }

  def parseGraph[N, E[X] <: EdgeLikeIn[X]](lineIterator: Iterator[String],
                                           nodeWeights: Map[String, BigDecimal],
                                           expectedGraphType: String,
                                           expectedGraphName: String,
                                           edgeType: EdgeCompanionBase[E],
                                           expectedEdgeTypeString: String,
                                           nodeGen: (Int, BigDecimal) => N)
                                          (implicit edgeTag: ClassTag[E[N]], nodeTag: ClassTag[N]): Graph[N, E] = {
    var dotParserState = FIRSTLINE

    val edges = new ListBuffer[E[N]]
    val nodes = new ListBuffer[N]
    while (lineIterator.hasNext) {
      val nextLine = lineIterator.next().trim
      if (nextLine.startsWith("//") || nextLine.isEmpty) {
        // ignore comment and empty lines
      } else {
        dotParserState match {
          case FIRSTLINE =>
            parseFirstLine(nextLine, expectedGraphType, expectedGraphName)
            dotParserState = EDGES
          case EDGES =>
            if (nextLine.equals("}")) {
              dotParserState = FINISHED
            } else {
              if (nextLine.split(" ").length > 1) {
                edges += parseEdge(nextLine, expectedEdgeTypeString, edgeType, nodeGen, nodeWeights = nodeWeights)
              } else {
                nodes += getNode(nextLine, nodeGen, nodeWeights)
              }
            }
        }
      }
    }
    assertOrThrowException(dotParserState == FINISHED, "Imported dot file not valid, didn't end with closing bracket on last line.")
    Graph.from[N, E](nodes, edges)
  }

  /**
    * Allows to import an application graph from a dot file.
    * @param lineIterator The lines of the dot file representing this app graph
    * @param appNodeWeights The preimported weights of this app graph
    * @return The imported application graph.
    */
  def parseAppGraph(lineIterator: Iterator[String], appNodeWeights: Map[String, BigDecimal]): Graph[AppNode, WDiEdge] = {
    parseGraph[AppNode, WDiEdge](lineIterator, appNodeWeights, DIGRAPH, "ApplicationGraph", WDiEdge, "->", AppNode.apply)
  }

  /**
    * Allows to import an fog graph from a dot file.
    * @param lineIterator The lines of the dot file representing this fog graph
    * @param fogNodeWeights The preimported weights of this fog graph
    * @return The imported fog graph.
    */
  def parseFogGraph(lineIterator: Iterator[String], fogNodeWeights: Map[String, BigDecimal]): Graph[FogNode, WUnDiEdge] = {
    parseGraph[FogNode, WUnDiEdge](lineIterator, fogNodeWeights, GRAPH, "FogGraph", WUnDiEdge, "--", FogNode.apply)
  }

  private def getNode[N](nString: String, nodeGen: (Int, BigDecimal) => N, nodeWeights: Map[String, BigDecimal]): N = {
    val stripped = nString.stripPrefix("\"").stripSuffix("\"").trim.drop(1)

    nodeGen(stripped.toInt, nodeWeights(nString))
  }

  private def getWeight(weightString: String): Double = {
    assertOrThrowException(weightString.endsWith("]"), s"Edge line should end with ] (found $weightString)")
    weightString.stripSuffix("]").replaceAll("\"", "").toDouble
  }

  private def parseEdge[N, E[X] <: EdgeLikeIn[X]](nextLine: String, expectedEdgeTypeString: String, edgeType: EdgeCompanionBase[E],
                                          nodeGen: (Int, BigDecimal) => N, expectedLabelString: String = "[label",  nodeWeights: Map[String, BigDecimal]): E[N] = {
    val (n1, n2, weight) = nextLine.trim.replaceAll(" +", " ").split(" ") match {
      case Array(n1String, edgeTypeString, n2String, label, equals, weightString) =>
        assertOrThrowException(expectedEdgeTypeString.equals(edgeTypeString), s"Edge type not as expected $expectedEdgeTypeString (found $edgeTypeString)")
        Utils.check(label.equals(expectedLabelString), s"Label string not as expected $expectedLabelString, (found $label)")
        Utils.check(equals.equals("="), s"Equals sign not as expected '=' (found $equals)")

        (getNode(n1String, nodeGen, nodeWeights), getNode(n2String, nodeGen, nodeWeights), getWeight(weightString))

      case _ => throw new DotParserException(s"Edge line wasn't as expected (found $nextLine)")
    }

    generation.generateEdge(edgeType, n1, n2)(GraphGenBase(appLinkWeightGen = () => weight))
  }

  private def parseFirstLine(nextLine: String, expectedGraphType: String, expectedGraphName: String): Unit = {
    val tokens = nextLine.split(" ")
    assertOrThrowException(tokens.size == 3, s"First line of imported app graph is invalid (not 3 tokens provided): $nextLine")

    val graphType = tokens(0)
    assertOrThrowException(graphType.equals(expectedGraphType), s"Wrong graph type for app graph: $graphType, should be $expectedGraphType")

    val graphName = tokens(1)

    Utils.check(graphName.equals(expectedGraphName), s"Graph is not called $expectedGraphName (but: $graphName), are you sure you are importing the correct graph?")

    val openBracket = tokens(2)
    assertOrThrowException(openBracket.equals("{"), s"First line should end with open bracket ({), was $openBracket")
  }

  private def assertOrThrowException(condition: Boolean, msg: String): Unit = {
    if (!condition) {
      throw new DotParserException(msg)
    }
  }
}
