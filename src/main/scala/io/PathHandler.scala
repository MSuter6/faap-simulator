package io

import better.files.{File, _}
import experiments.ExperimentInstance
import simulator.allocation.SolverAllocatorConfig
import simulator.{FaapProblemInstance, FaapProblemInstanceRepetitions}

/**
  * Responsible for a unified path handling for all projects. Helps with the organization of all file-system paths
  * during the export and import phases.
  * @param id The id/name of the experiment.
  * @param expType The type/class of the experiment (e.g. class of impact of 'scale' experiments).
  */
@SerialVersionUID(455919947834995827l)
case class PathHandler(id: String, expType: String = "") extends Serializable {

  import PathHandler._

  def getExp: File = {
    (resultsRoot / expType / id).createIfNotExists(asDirectory = true, createParents = true)
  }

  def getConfig(ending: String = ".conf"): File = getExp / (defaultConfig + ending)

  def getScipConfigFile: File = PathHandler.getScipConfigFile

  def getInstancesPath: File = (getExp / defaultFaapInstances).createIfNotExists(asDirectory = true, createParents = true)

  def getFaapInstanceRepetitionPath(repetitions: FaapProblemInstanceRepetitions, repId: Int): File = {
    (getInstancesPath / getFaapRepetitionsName(repetitions) / s"r$repId") createIfNotExists(true, true)
  }

  def getIterPath(experimentInstance: ExperimentInstance): File = {
    (getExp / iterSolution / getInstanceName(experimentInstance)).createIfNotExists(asDirectory = true, createParents = true)
  }

  def getResultsPath(experimentInstance: ExperimentInstance, faapProblemInstance: FaapProblemInstance): File = {
    (getIterPath(experimentInstance) / s"r${faapProblemInstance.repetition}").createIfNotExists(asDirectory = true, createParents = true)
  }

  def getFinalResultPath(ending: String = ".png"): (File, File) = {
    val tempPath = (getExp / defaultTempOutPath).createIfNotExists(asDirectory = true, createParents = true)
    val finalResultFile = getExp / (defaultFinalResult + ending)

    (tempPath, finalResultFile)
  }

  def getIterConfig(experimentInstance: ExperimentInstance, ending: String = ".conf"): File = getIterPath(experimentInstance) / (defaultIterConfig + ending)

  def getRepetitionPath(experimentInstance: ExperimentInstance, faapProblemInstance: FaapProblemInstance): File = {
    (getIterPath(experimentInstance) / s"r${faapProblemInstance.repetition}").createIfNotExists(asDirectory = true, createParents = true)
  }

  def getSolverRepetition(faapProblemInstance: FaapProblemInstance, config: SolverAllocatorConfig): File =
    (getExp / defaultSolverFilesFolder / faapProblemInstance.id / s"r${faapProblemInstance.repetition}_${config.formulation.name}").createIfNotExists(asDirectory = true, createParents = true)

  def getSolverRepetitionForSolver(faapProblemInstance: FaapProblemInstance, config: SolverAllocatorConfig): File =
    (getSolverRepetition(faapProblemInstance, config) / config.solver.toString).createIfNotExists(asDirectory = true, createParents = true)

  def getSolverRepetitionFile(faapProblemInstance: FaapProblemInstance, config: SolverAllocatorConfig, ending: String): File = {
    getSolverRepetitionForSolver(faapProblemInstance, config) / (defaultZimplFile + ending)
  }

  def getSolverFile(faapProblemInstance: FaapProblemInstance, config: SolverAllocatorConfig, ending: String = ".zpl"): File = getSolverRepetition(faapProblemInstance, config) / (defaultZimplFile + ending)

  def getZimplFile(faapProblemInstance: FaapProblemInstance, config: SolverAllocatorConfig): File = getSolverFile(faapProblemInstance, config)

  def getLpFile(faapProblemInstance: FaapProblemInstance, config: SolverAllocatorConfig): File = getSolverFile(faapProblemInstance, config, ".lp")

  def getLpSolutionFile(faapProblemInstance: FaapProblemInstance, config: SolverAllocatorConfig): File = getSolverRepetitionFile(faapProblemInstance, config, ".sol")


  def getAppGraph(repetitions: FaapProblemInstanceRepetitions, repId: Int, ending: String = ".dot"): File = {
    getFaapInstanceRepetitionPath(repetitions, repId) / (defaultAppGraph + ending)
  }

  def getAppGraphNodeWeights(repetitions: FaapProblemInstanceRepetitions, repId: Int, ending: String = ".dot"): File = {
    getFaapInstanceRepetitionPath(repetitions, repId) / (defaultAppGraphNodeWeights + ending)
  }

  def getFogGraph(repetitions: FaapProblemInstanceRepetitions, repId: Int, ending: String = ".dot"): File = {
    getFaapInstanceRepetitionPath(repetitions, repId) / (defaultFogGraph + ending)
  }

  def getFogGraphNodeWeights(repetitions: FaapProblemInstanceRepetitions, repId: Int, ending: String = ".dot"): File = {
    getFaapInstanceRepetitionPath(repetitions, repId) / (defaultFogGraphNodeWeights + ending)
  }

  def getLocationBoundMapping(repetitions: FaapProblemInstanceRepetitions, repId: Int, ending: String = ".txt"): File = {
    getFaapInstanceRepetitionPath(repetitions, repId) / (defaultLocationBoundMapping + ending)
  }

  def getLatencyPath(repetitions: FaapProblemInstanceRepetitions, repId: Int, ending: String = ".txt"): File = {
    getFaapInstanceRepetitionPath(repetitions, repId) / (defaultLatencyPath + ending)
  }


  def getRepConfig(experimentInstance: ExperimentInstance, faapProblemInstance: FaapProblemInstance, ending: String = ".conf"): File = {
    getRepetitionPath(experimentInstance, faapProblemInstance) / (defaultRepConfig + ending)
  }

  def getMappingResult(experimentInstance: ExperimentInstance, faapProblemInstance: FaapProblemInstance, ending: String = ".txt"): File = {
    getRepetitionPath(experimentInstance, faapProblemInstance) / (defaultMapping + ending)
  }

  def getEvaluationResult(experimentInstance: ExperimentInstance, faapProblemInstance: FaapProblemInstance, ending: String = ".txt"): File = {
    getRepetitionPath(experimentInstance, faapProblemInstance) / (defaultEvaluation + ending)
  }

  def getTimestamp(experimentInstance: ExperimentInstance, faapProblemInstance: FaapProblemInstance, ending: String = ".txt"): File = {
    getRepetitionPath(experimentInstance, faapProblemInstance) / (defaultTimestamps + ending)
  }

  def reevaluation: PathHandler = PathHandler.expType(expType, f"reeval_$id")
}

object PathHandler {

  val root = "results"

  val defaultAppGraph = "app_graph"
  val defaultAppGraphNodeWeights = "app_graph_node_weights"
  val defaultFogGraph = "fog_graph"
  val defaultFogGraphNodeWeights = "fog_graph_node_weights"
  val defaultMapping = "mapping"
  val defaultLocationBoundMapping = "location_bound_mapping"
  val defaultLatencyPath = "latency_paths"
  val defaultConfig = "exp_config"
  val defaultIterConfig = "iter_config"
  val defaultRepConfig = "repetition_config"
  val defaultEvaluation = "evaluation"
  val defaultFaapInstances = "fom_problem_instances"
  val defaultTimestamps = "timestamps"
  val defaultTempOutPath = "out_temp"
  val defaultFinalResult = "comparison_plot"
  val defaultZimplFile = "instance"
  val defaultSolverFilesFolder = "solver_files"
  val iterSolution = "solutions"

  def testRoot: File = root / "testsIO"

  val format = new java.text.SimpleDateFormat(GlobalConfig.expTimestamp)
  val timestampNameSplitter = "_"

  def apply(): PathHandler = new PathHandler(getNewId)

  def withName(name: String): PathHandler = new PathHandler(f"$getNewId$timestampNameSplitter$name")

  def withName(id: String, name: String): PathHandler = new PathHandler(f"${id}_$name")

  def expType(expType: String) = new PathHandler((expType / f"$getNewId").name, expType)

  def expType(expType: String, id: String) = new PathHandler((expType / f"$id").name, expType)

  def expTypeWithName(expType: String, name: String) = new PathHandler((expType / f"$getNewId$timestampNameSplitter$name").name, expType)

  def expTypeWithName(expType: String, id: String, name: String) = new PathHandler((expType / f"$id$timestampNameSplitter$name").name, expType)

  def getLatestExperiment(expType: String): PathHandler = {
    val experimentRoots = (resultsRoot / expType).list.filter(_.isDirectory)

    val latestName = experimentRoots.flatMap(exp => {
      val splitted = exp.name.split(timestampNameSplitter)

      val tuple = splitted match {
        case Array(timestampString, nameString) => (timestampString, Some(nameString))
        case Array(timestampString) => (timestampString, None)
        case s => (s(0), None)
      }

      try {
        Some(format.parse(tuple._1), tuple._2)
      } catch {
        case _: Exception => None
      }
    }).maxBy(_._1)

    if (latestName._2.isDefined) {
      PathHandler.expTypeWithName(expType, format.format(latestName._1), latestName._2.get)
    } else {
      PathHandler.expType(expType, format.format(latestName._1))
    }
  }

  def getLatestExperiment: PathHandler = {
    getLatestExperiment("")
  }

  def getTest(testName: String): PathHandler = {
    expType(testRoot.name, testName)
  }

  /**
    * @return Human-readable timestamp
    */
  def getNewId: String = format.format(new java.util.Date())

  def resultsRoot: File = {
    val resultsPath: File = root.toFile.createIfNotExists(asDirectory = true)
    resultsPath
  }

  def getInstanceName(experimentInstance: ExperimentInstance): String = {
    val repetitions = experimentInstance.faapProblemInstanceRepetitions
    f"${repetitions.order}-${repetitions.xAxis}-${experimentInstance.id}"
  }

  def getFaapRepetitionsName(repetitions: FaapProblemInstanceRepetitions): String = {
    f"${repetitions.order}-${repetitions.xAxis}-${repetitions.description}"
  }

  def getScipConfigFile: File = "config" / "scip.set"

  def getAppGraph(path: File, ending: String = ".dot"): File = path / (defaultAppGraph + ending)

  def getAppGraphNodeWeights(path: File, ending: String = ".dot"): File = path / (defaultAppGraphNodeWeights + ending)

  def getFogGraph(path: File, ending: String = ".dot"): File = path / (defaultFogGraph + ending)

  def getFogGraphNodeWeights(path: File, ending: String = ".dot"): File = path / (defaultFogGraphNodeWeights + ending)

  def getLocationBoundMapping(path: File, ending: String = ".txt"): File = path / (defaultLocationBoundMapping + ending)

  def getLatencyPath(path: File, ending: String = ".txt"): File = path / (defaultLatencyPath + ending)

  def getMappingResult(path: File, ending: String = ".txt"): File = path / (defaultMapping + ending)

  def getEvaluationResult(path: File, ending: String = ".txt"): File = path / (defaultEvaluation + ending)

  def getTimestamp(path: File, ending: String = ".txt"): File = path / (defaultTimestamps + ending)
}
