package io.visualization


import experiments.ExperimentWrapper
import io.visualization.PlotUtils.AggregationType.AggregationType
import io.visualization.PlotUtils._


/**
  * Tools to produce a line plot from an experiment run
  */
case class LinePlotter(aggregationType: AggregationType) extends ExperimentWriter {
  override val aggregationTypeName = Some(aggregationType.toString)
  override val plottype = "line"

  def generateTexString(experimentWrapper: ExperimentWrapper, yValueExtractor: YValue): String = {
    val plotStatProvider = PlotState(experimentWrapper, yValueExtractor)

    val grouped = experimentWrapper.experimentIterations.groupBy(_.experimentInstance.allocator).mapValues(_.sortBy(_.experimentInstance.faapProblemInstanceRepetitions.order))

    val plotData = experimentWrapper.experimentConfig.allocators.map(alloc => {
      val iterations = grouped(alloc)

      val extracted = iterations.map(iter => {
        iter.experimentResults.map(_.map(yValueExtractor.valueExtrator))
      })

      val aggregated = extracted.map(aggregationType.aggregate)

      f"""
         |  \\addlegendentry{${alloc.policyName}};
         |  \\addplot plot coordinates {
         |          ${aggregated.zip(plotStatProvider.sanitizedxAxisEntries).map(t => f"(${t._2}, ${t._1})").mkString(" ")}
         |      };
       """.stripMargin
    }).mkString("")

    import plotStatProvider._

    f"""
       |$header
       |      ylabel={${plotStatProvider.yValueExtractor.plotName}},
       |      xlabel={Fog Nodes},
       |      height=.5\\textwidth,
       |      legend style={at={(0.5,1.2)},
       |                  anchor=north,legend columns=-1},
       |      legend image post style={scale=0.5},
       |      legend cell align={center},
       |      xmajorgrids,
       |      ymajorgrids,
       |   ]
       |   $plotData
       |$footer
    """.stripMargin
  }
}
