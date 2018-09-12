package io.visualization

import GlobalConfig.NL
import experiments.ExperimentWrapper
import io.visualization.PlotUtils._

/**
  * Tools to produce a box plot from an experiment run which shows the performance ratio
  * compared to the first allocation type of the plot. Usually this types of plots are used to show the optimality gap
  * of the heuristics and produce y-axis values between 0 (indefinitely worse than the optimal solution)
  * and 1 (as good as the optimal solution).
  */
@SerialVersionUID(-4760956453146641344L)
object BoxPlotPlotterPerfGuarnt extends ExperimentWriter {
  override val aggregationTypeName: Option[String] = None
  override val plottype = "box_perf"

  override def generateTexString(experimentWrapper: ExperimentWrapper, yValueExtrator: YValue): String = {
    val plotStatProvider = PlotState(experimentWrapper, yValueExtrator)
    import plotStatProvider._
    val boxWidth = groupSize -1  match {
      case 1 => 2
      case _ => groupSize + 2
    }

    val optimalValues =  problemInstanceTypeToResultMap.map(_._2.head).toList

    val plotData = problemInstanceTypeToResultMap.zipWithIndex.map({ case ((_, values), i1) => {
      val l = values.zip(groupColors).tail.zipWithIndex.map { case ((vals, color), i2) =>

        val opt = optimalValues(i1)
        val valueratio = vals.zip(opt).filter(t => t._1.isDefined && t._2.isDefined).map(t => if(t._2.get.value == 0 && t._1.get.value == 0) {
          1
        } else {
          if (yValueExtrator.valueExtrator == YValueExtractors.alloComputeTimeExtractor.valueExtrator) {
            t._1.get.value/ t._2.get.value
          } else {
            t._2.get.value/ t._1.get.value
          }
        })

        val index = i1 * (groupSize -1) + i2
        val basicOffset = 1.0 / groupSize
        val interval = math.floor(index.toDouble / (groupSize-1))
        val groupOffset = index.toDouble % (groupSize-1)
        val halfBoxExtend = 0.5 / boxWidth
        val drawPosition = basicOffset + interval + basicOffset * groupOffset
        val successRate = vals.count(_.isDefined).toDouble / opt.count(_.isDefined)

        val boxUpper = -0.1

        val successRateVisualizer =
          f"""   \\draw[thin] (axis cs:${drawPosition - halfBoxExtend}%.5f, -1) rectangle (axis cs:${drawPosition + halfBoxExtend}%.5f, $boxUpper); %% Success rate $successRate
             |    \\filldraw[draw=black, fill=$color] (axis cs:${drawPosition - halfBoxExtend}%.5f, -1) rectangle (axis cs:${drawPosition - halfBoxExtend + successRate * 2 * halfBoxExtend}%.5f, $boxUpper);
           """.stripMargin

        if (valueratio.size <= 0) {
          successRateVisualizer
        } else {
          val (median, lwhisker, uwhisker, lquart, uquart, loutliers, uoutliers) = BoxPlotPlotter.getBoxStats(valueratio)

          val outliers = loutliers ++ uoutliers
          val outliersString = if (outliers.nonEmpty) f"coordinates{${outliers.mkString(f"($drawPosition, ", f") ($drawPosition, ", ")")}}" else "coordinates{}"
          val box =
            f"""\\addplot[$color, mark = x, boxplot prepared={draw position=$drawPosition%.5f, lower whisker=$lwhisker%.5f, lower quartile=$lquart%.5f, median=$median%.5f,
               |      upper quartile=$uquart%.5f, upper whisker=$uwhisker%.5f, },] $outliersString;
           """.stripMargin

          f"""
             |$successRateVisualizer
             |$box
             | %% Outliers: lower: ${loutliers.size}, upper: ${uoutliers.size}
           """.stripMargin
        }
      }
      l
    }.mkString(NL)
    }).mkString("")

    f"""
       |$header
       |   xmin=-0.1,
       |   xmax=$numberOfGroups.1,
       |   ymin=-0.2,
       |   ymax=2,
       |   boxplot/draw direction=y,
       |   ylabel={${yValueExtrator.plotName}},
       |   height=.5\\textwidth,
       |   boxplot={box extend=1/$boxWidth,},
       |   x=2.5cm,
       |   xtick={0,1,2,...,40},
       |   x tick label as interval,
       |   xticklabels={$xAxisLabels},
       |   x tick label style={
       |       text width=2.5cm,
       |       align=center
       |   },
       |   extra y ticks={${yMin - yMin / 4}},
       |   extra y tick label={ \\%%},
       |   legend style={at={(0.5,1.2)},
       |               anchor=north,legend columns=-1},
       |   legend image post style={scale=0.3},
       |   legend cell align={center},
       |   ]
       |
       |   ${legend.tail.map(entry => f"   \\addlegendentry{$entry};").mkString(NL)}
       |
       |   $plotData
       |
       |$footer
    """.stripMargin
  }

}
