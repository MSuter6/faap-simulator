package io.visualization

import better.files.Dsl.SymbolicOperations
import better.files.File
import experiments.{ExperimentIteration, ExperimentResult, ExperimentWrapper, PlotItem}
import simulator.FaapProblemInstanceRepetitions

import scala.collection.immutable.ListMap
import scala.language.implicitConversions
import scala.sys.process.{ProcessLogger, _}

object PlotUtils {
  val ROW_SEP = "\\\\"
  val COL_SEP = "&"

  /**
    * A y-value is represented by a description, a y-value extractor and the unit of the value.
    * This is needed to show a nice representation on the plots
    */
  case class YValue(description: String, valueExtrator: ExperimentResult => PlotItem, unit: Option[String] = None) {
    private def unitString = if (unit.isDefined) s" [${unit.get}]" else ""

    def plotName = s"$description$unitString"

    def sanitizedName: String = description.replaceAll("\\W+", "_")
  }

  object YValueExtractors {
    /**
      * Used to show the cost in the plots
      */
    def costExtractor: YValue = YValue("Cost", res => res.evaluationResult.cost)

    /**
      * Used to show the allocation time/runtime in the plots
      */
    def alloComputeTimeExtractor: YValue = YValue("Alloc Time", res => res.timestamps.get.allocationComputeTime, Some("s"))

    /**
      * Used to show the evaluation time in the plots
      */
    def evalComputeTimeExtractor: YValue = YValue("Eval Time", res => res.timestamps.get.evaluationTime, Some("s"))
  }

  def sanitizePlot(s: String): String = s.replaceAll("[&%_#{}\\\\]", " ").trim

  val colors = Seq("red", "blue", "black", "green")

  /**
    * Trait which provides an interface for the plot generators with the method generate
    */
  trait ExperimentWriter extends Serializable {
    /**
      * Writes an experiment-plot to the specified path.
      * @param experimentWrapper The experiment which should be plotted.
      * @param plotTempPath where to store the temporary files produced during plot generation
      * @param plotPngPath Where to write the png-image of the plot
      * @param yValueExtractor Determines which value should be shown on the y-axis (e.g. cost)
      * @param expId Experiment ID
      */
    def writeExperiment(experimentWrapper: ExperimentWrapper, plotTempPath: File, plotPngPath: File, yValueExtractor: YValue, expId: String): Unit = {
      val texString = generateTexString(experimentWrapper, yValueExtractor)

      val plotTexPath = plotTempPath / s"${plottype}plot_${yValueExtractor.sanitizedName}.tex"
      val plotPdfPath = plotTempPath / s"${plottype}plot_${yValueExtractor.sanitizedName}.pdf"

      plotTexPath << f"%% ExpID: $expId"
      plotTexPath < texString

      // Run latex to pdf
      Seq("cmd", "/C", "pdflatex", plotTexPath.toString, "-output-directory", plotTempPath.toString) ! ProcessLogger(_ => ())

      // Run pdf to png
      Seq("cmd", "/C", "magick", "convert", plotPdfPath.toString, plotPngPath.toString) ! ProcessLogger(_ => ())
    }

    /**
      * Abstract method implemented by the different plot generators
      * @return The latex pgfplots string.
      */
    def generateTexString(experimentWrapper: ExperimentWrapper, yValueExtrator: YValue): String

    val plottype: String
    val aggregationTypeName: Option[String]
  }

  /**
    * State which is used during the plot generation and used by all plot generators
    * @param experimentWrapper Whole experiment
    * @param yValueExtractor y-axis value
    */
  case class PlotState(experimentWrapper: ExperimentWrapper, yValueExtractor: YValue) {
    val groupedRows: Map[FaapProblemInstanceRepetitions, Seq[ExperimentIteration]] = experimentWrapper.experimentIterations.groupBy(_.experimentInstance.faapProblemInstanceRepetitions)

    def expSorting(iters: Seq[ExperimentIteration]): (Int, Int) = {
      (iters.head.experimentInstance.faapProblemInstanceRepetitions.instances.head.appGraph.order, iters.head.experimentInstance.faapProblemInstanceRepetitions.instances.head.fogGraph.order)
    }

    val problemInstanceTypeToResultMap = ListMap(groupedRows.toSeq.sortBy(s=> s._1.order).map { case (id, iterations) =>
      (id, iterations.map(iter => {
        iter.experimentResults.map(_.map(yValueExtractor.valueExtrator))
      }))
    }: _*)

    val sanitizedxAxisEntries: List[String] = problemInstanceTypeToResultMap.map(t => sanitizePlot(t._1.xAxis)).toList

    val xAxisLabels = s"""${sanitizedxAxisEntries.mkString("{", ",", "}")}"""

    val groupSize: Int = groupedRows.head._2.size
    val numberOfGroups: Int = problemInstanceTypeToResultMap.size

    val groupColors: Seq[String] = colors.take(groupSize)

    val legend: Seq[String] = groupedRows.head._2.map(_.experimentInstance.allocator.policyName)

    val yMax: Double = 0.0001 + overallMax * 1.05
    def yMax(max: Double): Double = 1 + max * 1.05

    def overallMax: Double = {
      problemInstanceTypeToResultMap.values.flatten.flatten.flatten.map(_.value) match {
        case Nil => 1
        case nonEmpty => nonEmpty.max
      }
    }

    val yMin: Double = -yMax / 10

    val docHeader =
      s"""\\documentclass[tikz, border=5pt]{standalone}
         |\\usepackage{tikz}
         |\\usepackage{pgfplots}
         |\\usepgfplotslibrary{statistics}
         |\\pgfplotsset{compat=1.5}
         |\\pgfplotsset{every tick label/.append style={font=\\tiny}}
         |\\pgfplotscreateplotcyclelist{custom}{%
         |red, solid, every mark/.append style={solid, fill=red}, mark=*\\\\%
         |blue, dashed, every mark/.append style={solid, fill=blue}, mark=square*\\\\%
         |black, densely dotted, every mark/.append style={solid, fill=black}, mark=diamond*\\\\%
         |green, densely dashed, every mark/.append style={solid, fill=green}, mark=triangle*\\\\%
         |dashed, every mark/.append style={solid, fill=gray},mark=diamond*\\\\%
         |loosely dashed, every mark/.append style={solid, fill=gray},mark=*\\\\%
         |densely dotted, every mark/.append style={solid, fill=gray},mark=square*\\\\%
         |dashdotted, every mark/.append style={solid, fill=gray},mark=otimes*\\\\%
         |dashdotdotted, every mark/.append style={solid},mark=star\\\\%
         |densely dashdotted,every mark/.append style={solid, fill=gray},mark=otimes*\\\\%
         |}
         |\\begin{document}"""

    val colorCycle = s"""cycle list=${colors.mkString("{", ", ", "}")},"""

    val tikzHeader =
      s"""\\begin{figure}[H]
         |\\begin{tikzpicture}
         |   \\begin{axis}["""
    val header: String =
      s"""$docHeader
         |
         |$tikzHeader""".stripMargin

    val footer: String =
      s"""\\end{axis}
         |\\end{tikzpicture}
         |    \\caption{Comparison of ${yValueExtractor.plotName}. x-Axis: ... . Graph Type x to Graph Type y . Solver time limit: x s. Repetitions: ${problemInstanceTypeToResultMap.head._2.head.size}}
         |    \\label{fig:comparisoncostgrowth}
         |\\end{figure}
         |\\end{document}
       """.stripMargin
  }

  /**
    * Experiment repetitions can be aggregated in various ways. the [[io.visualization.PlotUtils.AggregationType]]
    * allows to flexibly chose the desired way (e.g. MEDIAN).
    */
  object AggregationType extends Enumeration {
    def median(s: Seq[Double]): Double = {
      val sortedSeq = s.sortWith(_ < _)
      if (s.isEmpty) 0
      else if (s.size % 2 == 1) sortedSeq(sortedSeq.size / 2)
      else {
        val (lower, upper) = sortedSeq.splitAt(s.size / 2)
        (lower.last + upper.head) / 2
      }
    }

    type AggregationType = Value
    val SUM, AVG, MEDIAN, MIN, MAX = Value

    class Aggregator(aggregator: Value) {
      def aggregate(items: Seq[Option[PlotItem]]): Double = {
        val values = items.flatten.map(_.value)
        if (values.isEmpty) return Double.NaN
        aggregator match {
          case SUM => values.sum
          case AVG => values.sum / values.size
          case MEDIAN => median(values)
          case MAX => values.max
          case MIN => values.min
        }
      }
    }

    implicit def value2Aggreg(aggregator: Value): Aggregator = new Aggregator(aggregator)
  }

}