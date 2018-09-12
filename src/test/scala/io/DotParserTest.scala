package io

import io.imports.ExperimentImporter
import org.scalatest.FunSuite
import simulator.FaapProblemInstanceRepetitions
import simulator.TestGraphs.VerySimpleFaapProblemInstance.verySimpleFaapLocationBoundProblemInstance

class DotParserTest extends FunSuite {

  test("testParseAppGraph") {
    val pathHandler = PathHandler.getTest("graphparser")

    val reps = FaapProblemInstanceRepetitions(IndexedSeq(verySimpleFaapLocationBoundProblemInstance), "testParseAppGraph", "0", 0)
    val appGraph = verySimpleFaapLocationBoundProblemInstance.appGraph

    val appGraphFile = pathHandler.getAppGraph(reps, 0)
    val appGraphFileNodeWeights = pathHandler.getAppGraphNodeWeights(reps, 0)

    ExperimentExporter(pathHandler).writeAppGraph(reps, verySimpleFaapLocationBoundProblemInstance, withPng = false)

    val nodeWeigths = ExperimentImporter.importNodeWeights(appGraphFileNodeWeights)
    val parsedAppGraph = ExperimentImporter.importAppGraph(appGraphFile, nodeWeigths)

    assert(appGraph.equals(parsedAppGraph))
  }

  test("testParseFogGraph") {
    val pathHandler = PathHandler.getTest("graphparser")

    val reps = FaapProblemInstanceRepetitions(IndexedSeq(verySimpleFaapLocationBoundProblemInstance), "testParseFogGraph", "0", 0)
    val fogGraph = verySimpleFaapLocationBoundProblemInstance.fogGraph

    val fogGraphFile = pathHandler.getFogGraph(reps, 0)
    val fogGraphFileNodeWeights = pathHandler.getFogGraphNodeWeights(reps, 0)

    ExperimentExporter(pathHandler).writeFogGraph(reps, verySimpleFaapLocationBoundProblemInstance, withPng = false)
    val nodeWeigths = ExperimentImporter.importNodeWeights(fogGraphFileNodeWeights)

    val parsedFogGraph = ExperimentImporter.importFogGraph(fogGraphFile, nodeWeigths)

    assert(fogGraph.equals(parsedFogGraph))
  }

}
