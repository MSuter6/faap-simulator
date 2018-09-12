package io.visualization

import io.visualization.PlotUtils.AggregationType.{AVG, MEDIAN}
import io.visualization.PlotUtils.{ExperimentWriter, YValue}

import scala.language.implicitConversions

/**
  * A [[io.visualization.PlotWriterConfig]] is a container to configure which plots should be
  * exported. A configuration for a plot consists of the value which should be plotted (e.g. cost) and
  * the plot type (e.g. box-plot)
  */
case class PlotWriterConfig(plots: Seq[(YValue, ExperimentWriter)]) {
  def joining(nextPlotWriterConfig: PlotWriterConfig): PlotWriterConfig = {
    PlotWriterConfig((plots ++ nextPlotWriterConfig.plots).distinct)
  }

}

/**
  * Defines a few commonly used [[io.visualization.PlotWriterConfig]]s such as e.g. all line plots.
  */
object DefaultPlotWriterConfigs {
  implicit def plotSeq2PlotWriterConfig(s: Seq[(YValue, ExperimentWriter)]): PlotWriterConfig = PlotWriterConfig(s)

  import PlotUtils.YValueExtractors._

  val allPlots = Seq((costExtractor, BoxPlotPlotter), (alloComputeTimeExtractor, BoxPlotPlotter),
    (costExtractor, BarPlotPlotter(MEDIAN)), (alloComputeTimeExtractor, BarPlotPlotter(MEDIAN)),
    (costExtractor, LinePlotter(MEDIAN)), (alloComputeTimeExtractor, LinePlotter(MEDIAN)))

  val allBoxPlots = Seq((costExtractor, BoxPlotPlotter), (alloComputeTimeExtractor, BoxPlotPlotter))
  val allLinePlots = Seq((costExtractor, LinePlotter(MEDIAN)), (alloComputeTimeExtractor, LinePlotter(MEDIAN)), (costExtractor, BoxPlotPlotterPerfGuarnt))
  val allBarPlots = Seq((costExtractor, BarPlotPlotter(MEDIAN)), (alloComputeTimeExtractor, BarPlotPlotter(MEDIAN)))

  val evalTimeLinePlots: Seq[(YValue, ExperimentWriter)] = Seq((evalComputeTimeExtractor, LinePlotter(MEDIAN)), (evalComputeTimeExtractor, LinePlotter(MEDIAN)))

  val costPerfRatioPlot = PlotWriterConfig(Seq((costExtractor, BoxPlotPlotterPerfGuarnt)))
  val allocTimePerfRatioPlot = PlotWriterConfig(Seq((alloComputeTimeExtractor, BoxPlotPlotterPerfGuarnt)))

  val allPlotWriterConfig: PlotWriterConfig = allPlots
  val boxPlotWriterConfig: PlotWriterConfig = allBoxPlots
  val barPlotWriterConfig = PlotWriterConfig(allBarPlots)
  val linePlotWriterConfig = PlotWriterConfig(allLinePlots)
  val allPlotPlusEvaltimePlotWriter = PlotWriterConfig((allPlots ++ evalTimeLinePlots).distinct)
  val avgComputeTimeLinePlotter = PlotWriterConfig(Seq((evalComputeTimeExtractor, LinePlotter(AVG)), (evalComputeTimeExtractor, LinePlotter(AVG))))

  val evalTimeBoxPlotter = PlotWriterConfig(Seq((evalComputeTimeExtractor, BoxPlotPlotter), (evalComputeTimeExtractor, BoxPlotPlotter)))

  val DefaultPlotWriterConfig: PlotWriterConfig = allBoxPlots joining allLinePlots
}