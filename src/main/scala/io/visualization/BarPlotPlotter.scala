package io.visualization

import GlobalConfig.NL
import experiments.ExperimentWrapper
import io.visualization.PlotUtils.AggregationType.AggregationType
import io.visualization.PlotUtils.{ExperimentWriter, PlotState, YValue, _}

/**
  * Tools to produce a bar plot from an experiment run
  */
case class BarPlotPlotter(aggregationType: AggregationType) extends ExperimentWriter {
  override val plottype = "bar"
  override val aggregationTypeName: Option[String] = Some(aggregationType.toString)

  override def generateTexString(experimentWrapper: ExperimentWrapper, yValueExtrator: YValue): String = {
    val plotStateProvider = PlotState(experimentWrapper, yValueExtrator)
    import plotStateProvider._

    val interval = "interval"
    val header = groupedRows.head._2.map(_.experimentInstance.allocator.policyName)

    val plotData = header.map(row => {
      s"\\addplot table[x=interval,y=$row]{\\costdata};"
    }).mkString(NL)

    val dataTable = problemInstanceTypeToResultMap.map({ case (faapInstance, values) =>
      val aggregated = values.map(aggregationType.aggregate)
      s"${sanitizePlot(faapInstance.xAxis)} $COL_SEP ${aggregated.mkString(COL_SEP)} $ROW_SEP"
    }).mkString(NL)

    f"""$docHeader
       |\\pgfplotstableread[row sep=$ROW_SEP,col sep=$COL_SEP]{
       |${(interval +: header).mkString(COL_SEP)} $ROW_SEP
       |$dataTable
       |}\\costdata$NL
       |
       |$tikzHeader
       |    ybar,
       |    bar width=.5cm,
       |    width=\\textwidth,
       |    height=.5\\textwidth,
       |    ylabel={${yValueExtrator.plotName}},
       |    xlabel={Fog Nodes},
       |    height=.5\\textwidth,
       |   legend style={at={(0.5,1.2)},
       |               anchor=north,legend columns=-1},
       |   symbolic x coords={$xAxisLabels},
       |   xtick=data,
       |   legend image post style={scale=0.3},
       |   legend cell align={center},
       |   $colorCycle
       |   ]
       |
       |   ${legend.map(entry => f"\\addlegendentry{$entry};").mkString(NL)}
       |
       |   $plotData
       |
       |$footer
    """.stripMargin
  }

}
