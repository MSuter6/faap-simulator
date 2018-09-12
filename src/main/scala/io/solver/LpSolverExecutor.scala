package io.solver

import better.files.File
import io.PathHandler

import scala.language.postfixOps
import scala.sys.process._

/**
  * Provides methods to envoke the solver processes
  */
object LpSolverExecutor {
  private def isSuccessExitCode(exitCode: Int) = exitCode == 0

  /**
    * @return True if gurobi is installed on the machine
    */
  def isGurobiInstalled: Boolean = {
    val exitCode = Seq("cmd", "/C", "gurobi_cl --help") !

    isSuccessExitCode(exitCode)
  }

  /**
    * @return True is SCIP is installed on the machine.
    */
  def isScipInstalled: Boolean = {
    val exitCode = Seq("cmd", "/C", "scip -v") !

    isSuccessExitCode(exitCode)
  }

  /**
    * Runs ZIMPL on the given source file.
    * @param source ZIMPL file (.zpl) to run ZIMPL on.
    * @return True if the generation of the .lp file succeeded.
    */
  def runZimpl(source: File): Boolean = {
    val exitCode = Seq("cmd", "/C", "zimpl", source.toString) !

    isSuccessExitCode(exitCode)
  }

  /**
    * Runs SCIP on the provided file.
    * @param source .lp file to run SCIP on
    * @param target Where to store the .sol solution file
    * @param timeLimit Time limit for SCIP
    */
  def runScip(source: File, target: File, timeLimit: Int): Unit = {
    assert(isScipInstalled)
    val configFile = PathHandler.getScipConfigFile

    import better.files.Dsl.SymbolicOperations
    configFile < f"limits/time = $timeLimit"

    val exitCode = Seq("cmd", "/C", s"scip -s " + configFile.toString + " -c \"read " + source.toString + " opt " + " write solution " + target.toString + " quit\"") !

    Utils.check(isSuccessExitCode(exitCode), s"SCIP didn't finish successfully (exit code: $exitCode")
  }

  /**
    * Runs Gurobi on the provided file.
    * @param source .lp file to run Gurobi on
    * @param target Where to store the .sol solution file
    * @param timeLimit Time limit for Gurobi
    */
  def runGurobi(source: File, target: File, timeLimit: Int): Unit = {
    assert(isGurobiInstalled)
    val exitCode = Seq("cmd", "/C", "gurobi_cl TimeLimit=" + timeLimit + " ResultFile=" + target.toString + " " + source.toString) !

    Utils.check(isSuccessExitCode(exitCode), s"Gurobi didn't finish successfully (exit code: $exitCode")
  }
}
