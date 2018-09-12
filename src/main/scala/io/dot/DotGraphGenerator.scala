package io.dot

import GlobalConfig.NL
import GraphDefinition.{AppNode, FogNode}
import scalax.collection.Graph
import scalax.collection.edge.{WDiEdge, WUnDiEdge}
import scalax.collection.io.dot._
import scalax.collection.io.dot.implicits._
import simulator.Mapping.Mapping


/**
  * Generates the dot representation of the fog graph. Used for visualization as well as to reimport
  * the app graph.
  */
object DotApplicationGraphGenerator {
  private val root = DotRootGraph(directed = true, Some("ApplicationGraph"))

  private def edgeTransformer(innerEdge: Graph[AppNode, WDiEdge]#EdgeT):
  Option[(DotGraph, DotEdgeStmt)] = innerEdge.edge match {
    case WDiEdge(source, target, weight) =>
      Some((root, DotEdgeStmt(source.toString, target.toString, List(DotAttr("label", f"$weight")))))
  }

  def iNodeTransformer(innerNode: Graph[AppNode, WDiEdge]#NodeT):
  Option[(DotGraph, DotNodeStmt)] = Some(root, DotNodeStmt(innerNode.toString, Nil))

  def generateDotRepresentation(applicationGraph: Graph[AppNode, WDiEdge]): String = {
    val dot = applicationGraph.toDot(root, edgeTransformer, iNodeTransformer = Some(iNodeTransformer))
    dot
  }
}

/**
  * Generates the dot representation of the fog graph. Used for visualization as well as to reimport
  * the fog graph.
  */
object DotFogGraphGenerator {
  private val root = DotRootGraph(directed = false, Some("FogGraph"))

  private def edgeTransformer(innerEdge: Graph[FogNode, WUnDiEdge]#EdgeT):
  Option[(DotGraph, DotEdgeStmt)] = innerEdge.edge match {
    case WUnDiEdge(source, target, weight) =>
      Some((root, DotEdgeStmt(source.toString, target.toString, List(DotAttr("label", f"$weight")))))
  }

  def iNodeTransformer(innerNode: Graph[FogNode, WUnDiEdge]#NodeT):
  Option[(DotGraph, DotNodeStmt)] = Some(root, DotNodeStmt(innerNode.toString, Nil))

  def generateDotRepresentation(fogGraph: Graph[FogNode, WUnDiEdge]): String = {
    val dot = fogGraph.toDot(root, edgeTransformer, iNodeTransformer = Some(iNodeTransformer))
    dot
  }
}

/**
  * Generates the dot representation of a mapping from an app graph to a fog graph.
  * This is mainly use for visualizaiton purposes.
  */
object MappingGraphGenerator {
  private val firstLine = "graph FogGraphWithEmbeddedAppGraph {"
  private val lastLine = "}"

  def generateDotRepresentation(fogGraph: Graph[FogNode, WUnDiEdge], applicationGraph: Graph[AppNode, WDiEdge], mapping: Mapping): String = {
    val fogToAppNodesMap = mapping.groupBy(_._2)
    val fogToAppNodesMapString = fogToAppNodesMap.mapValues(_.map(_._1.toFullString))

    val nodes = fogGraph.nodes.map(node => {
      val idString = node.toOuter.id
      val color = if (fogToAppNodesMapString.isDefinedAt(node)) "color=green, " else ""
      f"""  $idString [shape=record, ${color}label=\"{{${node.toOuter.toFullString}}|{${fogToAppNodesMapString.getOrElse(node, Seq("-")).mkString(" | ")}}}\"];"""
    })

    val edgeStrings = fogGraph.edges.map(edge => {
      val noMappedAppLinksString = f"  ${edge._1.id} -- ${edge._2.id};"
      noMappedAppLinksString
    })

    s"""$firstLine
       |${nodes.mkString(NL)}
       |${edgeStrings.mkString(NL)}
       |$lastLine
       """.stripMargin
  }
}
