package simulator.generation
import experiments.ExperimentCommon._
import experiments.FaapProblemInstancesConfiguration
import simulator.{FaapProblemInstance, FaapProblemInstanceRepetitions}

/**
  * Basic class to generate one to n FAAP instances based on the generators (which generate randomly based on some configuration).
  * @param appGraphGenerator The generator to use for the app graph generation
  * @param fogGraphGenerator The generator to use for the fog graph generation
  * @param locationBoundMappingGenerator The generator to use for the location-bound mapping generation (if any).
  * @param latencyPathGenerator The generator to use for the latency path generation (if any).
  * @param description The description of the problem generator.
  * @param id The ID
  * @param order The order (used for plot generation).
  */
case class FaapProblemGenerator(appGraphGenerator: ApplicationGraphGenerator,
                                fogGraphGenerator: FogGraphGenerator,
                                locationBoundMappingGenerator: LocationBoundMappingGenerator,
                                latencyPathGenerator: Option[LatencyPathGenerator] = None,
                                description: String,
                                id: String,
                                order: Int) {
  private def generateProblemInstance(rep: Int): FaapProblemInstance = {
    val appGraph = appGraphGenerator.generateApplicationGraph
    val fogGraph = fogGraphGenerator.generateFogGraph
    val locationBoundMapping = locationBoundMappingGenerator.generateLocationBoundMappings(appGraph, fogGraph)

    val latencyPath = latencyPathGenerator.map(_.generateLatencyPaths(appGraph, fogGraph, locationBoundMapping))

    FaapProblemInstance(rep, appGraph, fogGraph, Some(locationBoundMapping), latencyPath, id = description)
  }

  def generateProblemInstanceSingleRepetition: FaapProblemInstanceRepetitions = {
    FaapProblemInstanceRepetitions(IndexedSeq(generateProblemInstance(0)), description, id, order)
  }


  def generateProblemInstances(n: Int): FaapProblemInstanceRepetitions = {
    val instanceRepetitions = for (rep <- 0 until n) yield generateProblemInstance(rep)

    FaapProblemInstanceRepetitions(instanceRepetitions, description, id, order)
  }
}


/**
  * Various generator to generate problem instances which use default values that are shared by many experiments.
  */
object FaapProblemGenerator {
  def defaultFaapProblemGenerator(appGraphOrder: Int = defaultAppGraphOrder,
                                 fogGraphOrder: Int = defaultFogGraphOrder,
                                 lbRatio: Double = defaultLocationBoundMappingRatio): FaapProblemGenerator = {
    val appGraphGenerator = defaultAppGraphType(SeriesParallelDecomposableGraphGenConfig(appGraphOrder))
    val fogGraphGenerator = defaultFogGraphType(CactusGraphGenConfig(fogGraphOrder))
    val locationBoundMappingGenerator = DefaultLocationBoundMappingGenerator(LocationBoundMappingConfig(lbRatio))

    val description = f"default_fn${fogGraphOrder}_an${appGraphOrder}_lb$lbRatio"
    FaapProblemGenerator(appGraphGenerator, fogGraphGenerator, locationBoundMappingGenerator, description = description, id = f"$fogGraphOrder", order = fogGraphOrder+appGraphOrder)
  }

  def defaultFaapProblemGenerator(faapProblemInstancesConfiguration: FaapProblemInstancesConfiguration, appToFogNodeRatio: Double): () => Seq[FaapProblemInstanceRepetitions] = {
    () => for (fogGraphOrder <- faapProblemInstancesConfiguration.xAxisRange) yield {
      val appGraphOrder = fogGraphOrder * appToFogNodeRatio
      defaultFaapProblemGenerator(appGraphOrder.toInt, fogGraphOrder).generateProblemInstances(faapProblemInstancesConfiguration.repetitions)
    }
  }

}

object FixedComponentFaapProblemGenerator {
  private def getGenerators(appGraphOrder: Int, lbRatio: Double, fogGraphOrder: Int) = {
    val appGraphGenerator = SpdgApplicationGraphGenerator(SeriesParallelDecomposableGraphGenConfig(appGraphOrder))
    val fogGraphGenerator = CactusFogGraphGenerator(CactusGraphGenConfig(fogGraphOrder))
    val locationBoundMappingGenerator = DefaultLocationBoundMappingGenerator(LocationBoundMappingConfig(lbRatio))
    (appGraphGenerator, fogGraphGenerator, locationBoundMappingGenerator)
  }

  def FixAppGraphOrderFaapProblemGenerator(faapProblemInstancesConfiguration: FaapProblemInstancesConfiguration,
                                           appGraphOrder: Int = defaultAppGraphOrder, lbRatio: Double = defaultLocationBoundMappingRatio): () => IndexedSeq[FaapProblemInstanceRepetitions] = {
    () => for (fogGraphOrder <- faapProblemInstancesConfiguration.xAxisRange) yield {
      val (appGraphGenerator, fogGraphGenerator, locationBoundMappingGenerator) = getGenerators(appGraphOrder, lbRatio, fogGraphOrder)

      val generator = FaapProblemGenerator(appGraphGenerator, fogGraphGenerator, locationBoundMappingGenerator, description = s"fixAppOrder_fn${fogGraphOrder}_an$appGraphOrder", id = f"$fogGraphOrder", order = fogGraphOrder+appGraphOrder)
      generator.generateProblemInstances(faapProblemInstancesConfiguration.repetitions)
    }
  }

  def FixFogGraphOrderFaapProblemGenerator(faapProblemInstancesConfiguration: FaapProblemInstancesConfiguration,
                                          fogGraphOrder: Int = defaultFogGraphOrder, lbRatio: Double = defaultLocationBoundMappingRatio): () => IndexedSeq[FaapProblemInstanceRepetitions] = {
    () => for (appGraphOrder <- faapProblemInstancesConfiguration.xAxisRange) yield {
      val (appGraphGenerator, fogGraphGenerator, locationBoundMappingGenerator) = getGenerators(appGraphOrder, lbRatio, fogGraphOrder)

      val generator = FaapProblemGenerator(appGraphGenerator, fogGraphGenerator, locationBoundMappingGenerator, description = s"fixFogOrder_fn${fogGraphOrder}_an$appGraphOrder", id = f"$appGraphOrder", order = fogGraphOrder+appGraphOrder)
      generator.generateProblemInstances(faapProblemInstancesConfiguration.repetitions)
    }
  }
}

object SPDGToCactusGraphGenerator {
  def SPDGToCactusFaapProblemGenerator(appGraphScale: Int, fogGraphScale: Int, description: String, lbRatio: Double = defaultLocationBoundMappingRatio): FaapProblemGenerator = {
    val appGraphGenerator = SpdgApplicationGraphGenerator(SeriesParallelDecomposableGraphGenConfig(appGraphScale))
    val fogGraphGenerator = CactusFogGraphGenerator(CactusGraphGenConfig(fogGraphScale))
    val locationBoundMappingGenerator = DefaultLocationBoundMappingGenerator(LocationBoundMappingConfig(lbRatio))

    FaapProblemGenerator(appGraphGenerator, fogGraphGenerator, locationBoundMappingGenerator, description = description, id = f"$fogGraphScale", order = fogGraphScale+appGraphScale)
  }

  def SPDGToCactusFaapProblemGenerator(faapProblemInstancesConfiguration: FaapProblemInstancesConfiguration, appToFogNodeRatio: Double): () => Seq[FaapProblemInstanceRepetitions] = {
    () => for (fogGraphScale <- faapProblemInstancesConfiguration.xAxisRange) yield {
      SPDGToCactusFaapProblemGenerator(fogGraphScale, appToFogNodeRatio).generateProblemInstances(faapProblemInstancesConfiguration.repetitions)
    }
  }

  def SPDGToCactusFaapProblemGenerator(fogGraphScale: Int, appToFogNodeRatio: Double): FaapProblemGenerator = {
    val appGraphScale = (fogGraphScale * appToFogNodeRatio).toInt

    SPDGToCactusFaapProblemGenerator(appGraphScale, fogGraphScale, description = s"SPDG2Cac_fn${fogGraphScale}_an$appGraphScale")
  }
}
