package io.visualization

import GlobalConfig.NL
import experiments.{ExperimentWrapper, PlotItem}
import io.visualization.PlotUtils._

/**
  * Tools to produce a box plot from an experiment run
  */
@SerialVersionUID(3548954633031141643L)
object BoxPlotPlotter extends ExperimentWriter {
  override val aggregationTypeName: Option[String] = None
  override val plottype = "box"

  def getBoxStats(vals: Seq[Double]): (Double, Double, Double, Double, Double, List[Double], List[Double]) = {
    // https://www.researchgate.net/figure/The-main-components-of-a-boxplot-median-quartiles-whiskers-fences-and-outliers_303779929

    val values = vals.sortWith(_ < _).toList

    val median = AggregationType.median(values)
    val lquart = AggregationType.median(values.filter(_ <= median))
    val uquart = AggregationType.median(values.filter(_ >= median))
    val k = 2
    val interquartile = (uquart - lquart) * k
    val lFence = Utils.atLeast(lquart - interquartile, 0) // Not possible to have negative cost or runtime
    val uFence = uquart + interquartile

    val lwhisk = values.find(_ >= lFence).get
    val uwhisk = values.filter(_ <= uFence).max

    val louliers = values.filter(_ < lFence)
    val uouliers = values.filter(_ > uFence)
    (median, lwhisk, uwhisk, lquart, uquart, louliers, uouliers)
  }

  override def generateTexString(experimentWrapper: ExperimentWrapper, yValueExtrator: YValue): String = {
    val plotStatProvider = PlotState(experimentWrapper, yValueExtrator)
    import plotStatProvider._
    val boxWidth = groupSize match { case 1 => 2 case _ => groupSize + 2}

    val plotData = problemInstanceTypeToResultMap.zipWithIndex.map({ case ((_, values), i1) => {
      val l = values.zip(groupColors).zipWithIndex.map { case ((vals, color), i2) =>
        val index = i1 * groupSize  + i2
        val basicOffset = 1.0 / (groupSize + 1)
        val interval = math.floor(index.toDouble / groupSize)
        val groupOffset = index.toDouble % groupSize
        val halfBoxExtend = 0.5/boxWidth
        val drawPosition = basicOffset + interval + basicOffset * groupOffset
        val successRate = vals.count(_.isDefined).toDouble/vals.size

        val boxUpper = yMin / 2

        val successRateVisualizer =
          f"""   \\draw[thin] (axis cs:${drawPosition-halfBoxExtend}, $yMin) rectangle (axis cs:${drawPosition+halfBoxExtend}, $boxUpper); %% Success rate $successRate
             |    \\filldraw[draw=black, fill=$color] (axis cs:${drawPosition-halfBoxExtend}, $yMin) rectangle (axis cs:${drawPosition-halfBoxExtend+successRate*2*halfBoxExtend}, $boxUpper);
           """.stripMargin

        if (successRate <= 0) {
          successRateVisualizer
        } else {

          val (median, lwhisker, uwhisker, lquart, uquart, loutliers, uoutliers) = getBoxStats(vals.flatten.map(_.value))

          val outliers = loutliers ++ uoutliers
          val outliersString = if (outliers.nonEmpty) f"coordinates{${outliers.mkString(f"($drawPosition, ", f") ($drawPosition, ", ")")}}" else "coordinates{}"
          val box =
            f"""\\addplot[$color, mark = x, boxplot prepared={draw position=$drawPosition, lower whisker=$lwhisker, lower quartile=$lquart, median=$median,
               |      upper quartile=$uquart, upper whisker=$uwhisker, },] $outliersString;
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
       |   ymin=$yMin,
       |   ymax=$yMax,
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
       |   extra y ticks={${yMin-yMin/4}},
       |   extra y tick label={ \\%%},
       |   legend style={at={(0.5,1.2)},
       |               anchor=north,legend columns=-1},
       |   legend image post style={scale=0.3},
       |   legend cell align={center},
       |   ]
       |
       |   ${legend.map(entry => f"   \\addlegendentry{$entry};").mkString(NL)}
       |
       |   $plotData
       |
       |$footer
    """.stripMargin
  }

}
