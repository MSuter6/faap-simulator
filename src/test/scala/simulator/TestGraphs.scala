package simulator

import GraphDefinition.{AppNode, FogNode}
import scalax.collection.Graph
import scalax.collection.edge.{WDiEdge, WUnDiEdge}

/**
  * Some mock graphs to test various simulation functionality on.
  */
object TestGraphs {

  object VerySimpleFaapProblemInstance {

    val appNode1 = AppNode(1, 0.3)
    val appNode2 = AppNode(2, 0.3)
    val appNode3 = AppNode(3, 0.3)

    val appGraph: Graph[AppNode, WDiEdge] = Graph[AppNode, WDiEdge](
      WDiEdge(appNode1, appNode2)(3),
      WDiEdge(appNode2, appNode3)(4)
    )

    val fogNode1 = FogNode(1, 0.6)
    val fogNode2 = FogNode(2, 0.6)
    val fogNode3 = FogNode(3, 0.6)
    val fogNode4 = FogNode(4, 0.6)
    val fogGraph: Graph[FogNode, WUnDiEdge] = Graph[FogNode, WUnDiEdge](
      WUnDiEdge(fogNode1, fogNode2)(1),
      WUnDiEdge(fogNode2, fogNode3)(1),
      WUnDiEdge(fogNode3, fogNode4)(1)
    )

    /** Simple Graph
      *
      * Application Graph
      *   3     4
      * 1 --> 2 --> 3
      *
      * Fog Graph
      *
      * 1 -- 2 -- 3 -- 4
      *
      */
    val verySimpleFaapProblemInstance: FaapProblemInstance = FaapProblemInstance(0, appGraph, fogGraph)

    val locationBoundMapping = Mapping((appNode1, fogNode1), (appNode3, fogNode4))

    /** Simple Graph
      *
      * Application Graph
      *               3     4
      *             1 --> 2 --> 3
      *             |           \
      * Mapping    -| - - - - -  \ - -
      *             |             \
      * Fog Graph   1 -- 2 -- 3 -- 4
      *
      */
    val verySimpleFaapLocationBoundProblemInstance: FaapProblemInstance = FaapProblemInstance(0, appGraph, fogGraph, Some(locationBoundMapping))
  }

  object SimpleFaapLocationBoundProblemInstance {
    /**    5      2
      *  1 --> 2 --> 3
      *   \         /
      *    --->4---
      *     3    1
      *
      *
      * -| - - - - -  \ - -
      *  |             \
      *  1 -- 2 -- 3 -- 4
      *
      */

    val appNode1 = AppNode(1, 3)
    val appNode2 = AppNode(2, 4)
    val appNode3 = AppNode(3, 3)
    val appNode4 = AppNode(4, 5)

    val appGraph: Graph[AppNode, WDiEdge] = Graph[AppNode, WDiEdge](
      WDiEdge(appNode1, appNode2)(5),
      WDiEdge(appNode1, appNode4)(3),
      WDiEdge(appNode2, appNode3)(2),
      WDiEdge(appNode4, appNode3)(1)
    )

    val fogNode1 = FogNode(1, 3)
    val fogNode2 = FogNode(2, 4)
    val fogNode3 = FogNode(3, 4)
    val fogNode4 = FogNode(4, 4)
    val fogNode5 = FogNode(5, 5)
    val fogNode6 = FogNode(6, 3)

    val fogGraph: Graph[FogNode, WUnDiEdge] = Graph[FogNode, WUnDiEdge](
      WUnDiEdge(fogNode1, fogNode2)(1),
      WUnDiEdge(fogNode1, fogNode4)(1),
      WUnDiEdge(fogNode2, fogNode3)(1),
      WUnDiEdge(fogNode2, fogNode4)(1),
      WUnDiEdge(fogNode2, fogNode6)(1),
      WUnDiEdge(fogNode3, fogNode5)(1),
      WUnDiEdge(fogNode3, fogNode6)(1),
      WUnDiEdge(fogNode4, fogNode5)(1),
      WUnDiEdge(fogNode5, fogNode6)(1)
    )

    val locationBoundMapping = Mapping((appNode1, fogNode1), (appNode3, fogNode6))

    val simpleFaapLocationBoundProblemInstance = FaapProblemInstance(0, appGraph, fogGraph, Some(locationBoundMapping))

    val optimalMapping = Mapping((appNode1, fogNode1), (appNode2, fogNode2), (appNode3, fogNode6), (appNode4, fogNode5))
  }


  object SimpleFaapLocationBoundProblemInstance2 {
    val appNode1 = AppNode(1, 2)
    val appNode2 = AppNode(2, 3)
    val appNode3 = AppNode(3, 1)
    val appNode4 = AppNode(4, 4)

    val appGraph: Graph[AppNode, WDiEdge] = Graph[AppNode, WDiEdge](
      WDiEdge(appNode1, appNode2)(3),
      WDiEdge(appNode1, appNode3)(5),
      WDiEdge(appNode2, appNode3)(2),
      WDiEdge(appNode3, appNode4)(4)
    )

    val fogNode1 = FogNode(1, 3)
    val fogNode2 = FogNode(2, 4)
    val fogNode3 = FogNode(3, 2)
    val fogNode4 = FogNode(4, 2)
    val fogNode5 = FogNode(5, 3)
    val fogNode6 = FogNode(6, 4)

    val fogGraph: Graph[FogNode, WUnDiEdge] = Graph[FogNode, WUnDiEdge](
      WUnDiEdge(fogNode1, fogNode2)(1),
      WUnDiEdge(fogNode1, fogNode4)(1),
      WUnDiEdge(fogNode2, fogNode3)(1),
      WUnDiEdge(fogNode2, fogNode4)(1),
      WUnDiEdge(fogNode2, fogNode6)(1),
      WUnDiEdge(fogNode3, fogNode5)(1),
      WUnDiEdge(fogNode3, fogNode6)(1),
      WUnDiEdge(fogNode4, fogNode5)(1),
      WUnDiEdge(fogNode5, fogNode6)(1)
    )

    val locationBoundMapping = Mapping((appNode1, fogNode1), (appNode4, fogNode6))

    val simpleFaapLocationBoundProblemInstance = FaapProblemInstance(0, appGraph, fogGraph, Some(locationBoundMapping))

    val optimalMapping = Mapping((appNode1, fogNode1), (appNode2, fogNode2), (appNode3, fogNode1), (appNode4, fogNode6))
  }

  object SimpleInfeasibleFaapLocationBoundLatencyPathProblemInstance {
    import simulator.TestGraphs.SimpleFaapLocationBoundProblemInstance._

    val latencyPath = LatencyPaths(LatencyPath(Seq(appNode1, appNode4), 1))

    val simpleFaapLocationBoundProblemInstance = FaapProblemInstance(0, appGraph, fogGraph, Some(locationBoundMapping), Some(latencyPath))
  }

  object SimpleFeasibleFaapLocationBoundLatencyPathProblemInstance {
    import simulator.TestGraphs.SimpleFaapLocationBoundProblemInstance._

    val latencyPath = LatencyPaths(LatencyPath(Seq(appNode1, appNode4), 2))

    val simpleFaapLocationBoundProblemInstance = FaapProblemInstance(0, appGraph, fogGraph, Some(locationBoundMapping), Some(latencyPath))

    val optimalMapping = Mapping((appNode1, fogNode1), (appNode2, fogNode2), (appNode3, fogNode6), (appNode4, fogNode5))
  }
}
