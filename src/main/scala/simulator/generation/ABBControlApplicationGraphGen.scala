package simulator.generation

import GraphDefinition.AppNode

import scala.language.higherKinds
import scalax.collection.Graph
import scalax.collection.GraphEdge.EdgeCompanionBase
import scalax.collection.edge.WDiEdge
import scala.reflect.ClassTag

/**
  * Generates an application graph according to the Scheme of the ABB Control application. (Thesis Section 4.1.2)
  */
object ABBControlApplicationGraphGen {
  case class Weights(wcet: Boolean) {


    val (sensor, scaling, smoothing, featExtr,
    procA, procB, aggr1, aggr2, globalCtrl, actuator) =  if (wcet) {
      (2.0, 27.55, 26.65, 9.6, 15.25, 3.6, 20.875, 20.875, 14.5, 0.25)
    } else { // avg weights
      (2.0, 7.3, 7.65, 7.25, 1.525, 2.575, 19.95, 19.95, 13.4, 0.25)
    }

    val nodeWeights = Seq(sensor, scaling, smoothing, featExtr, procA, procB, aggr1, aggr2, globalCtrl, actuator)

    val blockNodeWeights = Seq(sensor, scaling, smoothing, featExtr, procA, procB, actuator)

    def totalNodeWeights: Double = nodeWeights.sum

    def maxNodeWeight: Double = nodeWeights.max

    val (s2Scm, sc2Sm, sm2Feat, feat2ProcA, feat2ProcB, procA2aggr, procB2aggr,
    aggr12gCtrl, aggr22gCtrl, gCtrl2proc, gCtrl2Act) = if (wcet) {
      (3072, 384, 448, 128, 256, 96, 160, 192, 192, 68, 68)
    } else {
      (3072, 384, 448, 128, 256, 96, 160, 192, 192, 68, 68)
    }
  }

  def generateABBControlApplicationGraph[E[X] <: WDiEdge[X]](config: ABBControlApplicationGraphGenConfig, edgeType: EdgeCompanionBase[E])
                                                            (implicit edgeTag: ClassTag[E[AppNode]], nodeTag: ClassTag[AppNode]): Graph[AppNode, E] = {

    def getPreProcBlock(i: Int, w: Weights, pAggr: AppNode, globalCtrl: AppNode) = {
      val sensor = AppNode.next(w.sensor).setDescription(f"sensor$i")
      val scalingDownS = AppNode.next(w.scaling).setDescription(f"scale$i")
      val smoothing = AppNode.next(w.smoothing).setDescription(f"smooth$i")
      val featExtrSpl = AppNode.next(w.featExtr).setDescription(f"featExtr$i")
      val procA = AppNode.next(w.procA).setDescription(f"procA$i")
      val procB = AppNode.next(w.procB).setDescription(f"procB$i")

      val sensorToScDoS = generateEdge(edgeType, sensor, scalingDownS, w.s2Scm)
      val ScDoSToSmoothing = generateEdge(edgeType, scalingDownS, smoothing, w.sc2Sm)
      val smoothingToFeatExtr = generateEdge(edgeType, smoothing, featExtrSpl, w.sm2Feat)
      val FeatExtrToProcA = generateEdge(edgeType, featExtrSpl, procA, w.feat2ProcA)
      val FeatExtrToProcB = generateEdge(edgeType, featExtrSpl, procB, w.feat2ProcB)

      val ProcAToPAggr = generateEdge(edgeType, procA, pAggr, w.procA2aggr)
      val ProcBToPAggr = generateEdge(edgeType, procB, pAggr, w.procB2aggr)

      val globalCtrlToProcA = generateEdge(edgeType, globalCtrl, procA, w.gCtrl2proc)
      val globalCtrlToProcB = generateEdge(edgeType, globalCtrl, procB, w.gCtrl2proc)

      val allNodes = Seq(sensor, scalingDownS, smoothing, featExtrSpl, procB, procA)
      val allEdges = Seq(sensorToScDoS, ScDoSToSmoothing, smoothingToFeatExtr, FeatExtrToProcB, FeatExtrToProcA,
        ProcAToPAggr, ProcBToPAggr, globalCtrlToProcA, globalCtrlToProcB)

      Graph.from(allNodes, allEdges)
    }

    def getAggrBlock(w: Weights) = {
      val pAggr1 = AppNode.next(w.aggr1).setDescription("aggr1")
      val pAggr2 = AppNode.next(w.aggr2).setDescription("aggr2")
      val globalCtrl = AppNode.next(w.globalCtrl).setDescription("glocalCtrl")
      val actuators = for (i <- 1 to config.n) yield AppNode.next(w.actuator).setDescription(f"act$i")

      val pAggr1ToGlobalCtrl = generateEdge(edgeType, pAggr1, globalCtrl, w.aggr12gCtrl)
      val pAggr2ToGlobalCtrl = generateEdge(edgeType, pAggr2, globalCtrl, w.aggr22gCtrl)

      val globalCtrlToActuators = actuators.map(actor => generateEdge(edgeType, globalCtrl, actor, w.gCtrl2Act))

      val allEdges = globalCtrlToActuators :+ pAggr1ToGlobalCtrl :+ pAggr2ToGlobalCtrl
      val allNodes = actuators :+ pAggr1 :+ pAggr2 :+ globalCtrl
      (pAggr1, pAggr2, globalCtrl, Graph.from(allNodes, allEdges))
    }

    val weights = Weights(config.wcet)

    val (prot1, prot2, globalCtrl, protBlock) = getAggrBlock(weights)
    val preProcBlocks = for (i <- 1 to config.n) yield {
      val n = config.n.toDouble
      if (i <= math.ceil(n/ i)) {
        getPreProcBlock(i, weights, prot1, globalCtrl)
      } else {
        getPreProcBlock(i, weights, prot2, globalCtrl)
      }
    }

    val allPreProcBlocks = preProcBlocks.reduce(_ ++ _)

    allPreProcBlocks ++ protBlock
  }
}
