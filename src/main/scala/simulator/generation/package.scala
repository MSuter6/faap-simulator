package simulator

import scalax.collection.GraphEdge.EdgeCompanionBase
import scalax.collection.GraphPredef.EdgeLikeIn
import scalax.collection.edge.WBase.WEdgeCompanion

import scala.language.higherKinds

package object generation {

  type DoubleGen = () => Double

  class GeneratorException(msg: String,
                           private val cause: Throwable = None.orNull) extends Exception(msg, cause)

  /** Generates an edge of the specified type [[scalax.collection.GraphEdge.EdgeCompanionBase]] using the app link
    * weight generator from the [[simulator.generation.GraphGenBase]] object */
  def generateEdge[E[X] <: EdgeLikeIn[X], N](comp: EdgeCompanionBase[E], node1: N, node2: N)(implicit graphConfig: GraphGenBase): E[N] = {
      // Currently, only the app edge weight generator is used independent of the edge type.
      // If the fog link weights shall be used, this has to be adapted.
      generateEdge(comp, node1, node2, Utils.roundToPrecision(graphConfig.appLinkWeightGen(), GlobalConfig.doublePrecision))
  }

  /** Generates an edge of the specified type [[scalax.collection.GraphEdge.EdgeCompanionBase]] using the specified weight */
  def generateEdge[E[X] <: EdgeLikeIn[X], N](comp: EdgeCompanionBase[E], node1: N, node2: N, weight: Double): E[N] = {
    // Currently, only the app edge weight generator is used independent of the edge type.
    // If the fog link weights shall be used, this has to be adapted.
    (comp match {
      case c: WEdgeCompanion[E] => c(node1, node2)(weight)
      case _ => throw new IllegalArgumentException(s"The edge companion '$comp' not supported.")
    }).asInstanceOf[E[N]]
  }
}
