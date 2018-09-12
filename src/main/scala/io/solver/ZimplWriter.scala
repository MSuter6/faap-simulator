package io.solver

import GraphDefinition.{AppNode, FogNode, GraphNode}
import better.files.Dsl.SymbolicOperations
import better.files.File
import io.solver.ZimplBase._
import scalax.collection.edge.{WDiEdge, WUnDiEdge}
import simulator.LatencyPaths
import simulator.Mapping.Mapping

import scala.collection.mutable

/**
  * Basic directives of ZIMPL such as sets, comments, parameters etc.
  * Refer to the ZIMPL user guide for further info: http://zimpl.zib.de/download/zimpl.pdf (07.09.2018)
  */
object ZimplBase {

  implicit class AugmentedString(o: Any) {
    def esc: String = "\"" + o + "\""
  }

  def comment(str: String): String = {
    "# " + str
  }

  def set[A](name: String, seq: Iterable[A]): String = {
    "set " + name + " := " + seq.mkString("{", ", ", "};")
  }

  def tuple(edge: WUnDiEdge[GraphNode]): String = {
    tuple(edge._1, edge._2)
  }

  def tuple(n1: GraphNode, n2: GraphNode): String = {
    "<" + n1.getIdString.esc + ", " + n2.getIdString.esc + ">"
  }

  def singleton(n: GraphNode): String = {
    "<" + n.getIdString.esc + ">"
  }

  def param1(name: String, setName: String, params: Iterable[(GraphNode, Double)]): String = {
    "param " + name + "[" + setName + "] := " + params.map({ case (n1, value) => singleton(n1) + " " + value }).mkString(", ") + ";"
  }

  def param2(name: String, setName: String, params: Seq[(GraphNode, GraphNode, Double)]): String = {
    "param " + name + "[" + setName + "] := " + params.map({ case (n1, n2, value) => tuple(n1, n2) + " " + value }).mkString(", ") + ";"
  }
}

/**
  * Basic generator of the Zimpl formulation for a standard FAAP instance.
  * @param name Name/Description for this ZIMPL writer
  */
@SerialVersionUID(2610563523077821565l)
class ZimplWriter(val name: String) extends Serializable {
  val AN = "AN"
  val AE = "AE"
  val FN = "FN"
  val FE = "FE"
  val LB = "LOCATIONBOUND"

  val computational_cost = "computational_cost"
  val cpu_capacity = "cpu_capacity"
  val data_bandwidth = "data_bandwidth"
  val link_bandwidth = "link_bandwidth"
  val closest_path_cost = "closest_path_cost"

  var locationBoundConstraint = ""

  val latencyPathConstraints: mutable.StringBuilder = new StringBuilder()

  val vars: String =
    f"""
       |set Z := AE * FN * FN;
       |# Cartesian produt between app and fog nodes
       |set ANFN := AN * FN;
       |
       |# Decides whether an app node is mapped on a fog node
       |var y[ANFN] binary;
       |
       |# Decides whether an app edge if mapped on two fog nodes
       |var z[Z];
       |
  """.stripMargin

  val costs: String =
    f"""minimize cost: sum<an1, an2> in $AE: ($data_bandwidth[an1, an2] * (sum<fn1, fn2> in $FE: $closest_path_cost[fn1, fn2] * (z[an1,an2,fn1,fn2] + z[an1,an2,fn2,fn1])));
    """.stripMargin

  // http://yetanothermathprogrammingconsultant.blogspot.com/2008/05/multiplication-of-binary-variables.html
  val z2y: String =
    f"""subto zConstr: forall <an1, an2, fn1, fn2> in Z: z[an1, an2, fn1, fn2] <= 1;
       |subto z2y: forall <an1, an2, fn1, fn2> in Z: z[an1, an2, fn1, fn2] <= y[an1, fn1]
       |                                             and z[an1, an2, fn1, fn2] <= y[an2, fn2]
       |                                             and z[an1, an2, fn1, fn2] >= y[an1, fn1] + y[an2, fn2] - 1;
     """.stripMargin

  val constraints: String =
    f"""# Constraints
       |# Each app node must only be mapped on one fog node
       |subto mapToOne: forall <an> in AN: (sum <fn> in FN: y[an, fn]) == 1;
       |# The capacity on each fog node must not be exceeded by the mapped app nodes thereon
       |subto nodeCapacities: forall <fn> in FN: (sum<an> in AN: computational_cost[an] * y[an, fn]) <= cpu_capacity[fn];
    """.stripMargin

  implicit class AugmentedStringBuilder(stringBuilder: mutable.StringBuilder) {
    def nl: mutable.StringBuilder = stringBuilder.append(GlobalConfig.NL)
  }

  val stringBuilder: mutable.StringBuilder = new StringBuilder()

  def appNodes(nodes: Iterable[AppNode]): this.type = {
    stringBuilder.append(comment("Application graph nodes")).nl
    stringBuilder.append(set(AN, nodes.map(singleton))).nl
    this
  }

  def fogNodes(nodes: Iterable[FogNode]): this.type = {
    stringBuilder.append(comment("Fog graph nodes")).nl
    stringBuilder.append(set(FN, nodes.map(singleton))).nl
    this
  }

  def appEdges(edges: Set[WDiEdge[AppNode]]): this.type = {
    stringBuilder.append(comment("Application Graph edges")).nl
    stringBuilder.append(set(AE, edges.map(tuple(_)))).nl
    this
  }

  def fogEdges(edges: Seq[(GraphNode, GraphNode)]): this.type = {
    stringBuilder.append(comment("Fog Graph edges")).nl
    stringBuilder.append(set(FE, edges.map(t => tuple(t._1, t._2)))).nl
    this
  }

  def locationBound(locationBoundMapping: Mapping): this.type = {
    stringBuilder.append(comment("Location Bound mapping")).nl
    stringBuilder.append(set(LB, locationBoundMapping.map(t => tuple(t._1, t._2)))).nl
    locationBoundConstraint = f"subto lb: forall <an, fn> in $LB: y[an, fn] == 1;"
    this
  }

  def latencyPaths(latencyPaths: LatencyPaths): Unit = {
    latencyPathConstraints.append(comment("Latency path constraints")).nl
    latencyPaths.paths.zipWithIndex.foreach { case (path, index) =>
      if (path.path.size > 1) {
        latencyPathConstraints.append(f"subto lp$index: ")

        val sums = path.path.sliding(2).map {
          case Seq(an1, an2) =>
            val z_an = f"z[${an1.getIdString.esc},${an2.getIdString.esc}"
            f"(sum<fn1, fn2> in FE: closest_path_cost[fn1, fn2] * ($z_an,fn1,fn2] + $z_an,fn2,fn1]))"
        }.mkString(" + ")
        latencyPathConstraints.append(sums)
        latencyPathConstraints.append(f" <= ${path.deadline};").nl
      }
    }
  }

  def computationalCost(nodes: Set[AppNode]): this.type = {
    stringBuilder.append(comment("Resource used by App node")).nl
    stringBuilder.append(param1(computational_cost, AN, nodes.map(n => (n, n.resourceUsage.toDouble)))).nl
    this
  }

  def cpuCapacity(nodes: Set[FogNode]): this.type = {
    stringBuilder.append(comment("Capacities of fog nodes")).nl
    stringBuilder.append(param1(cpu_capacity, FN, nodes.map(n => (n, n.cpuConstraint.toDouble)))).nl
    this
  }

  def dataBandwidth(edges: Seq[WDiEdge[AppNode]]): this.type = {
    stringBuilder.append(comment("Bandwidth used between app nodes")).nl
    stringBuilder.append(param2(data_bandwidth, AE, edges.map(n => (n._1, n._2, n.weight)))).nl
    this
  }

  def linkBandwidth(edges: Seq[WUnDiEdge[FogNode]]): this.type = {
    stringBuilder.append(comment("Available bandwidth on link")).nl
    stringBuilder.append(param2(link_bandwidth, FE, edges.map(n => (n._1, n._2, n.weight)))).nl
    this
  }

  def closestPathCost[N, E](pathCosts: Seq[(GraphNode, GraphNode, Double)]): this.type = {
    stringBuilder.append(comment("Minimum number of hops between every two fog nodes")).nl
    stringBuilder.append(param2(closest_path_cost, FE, pathCosts)).nl
    this
  }

  def appendCostAndConstraints: this.type = {
    stringBuilder.append(vars).nl
    stringBuilder.append(costs).nl
    stringBuilder.append(constraints).nl
    stringBuilder.append(z2y).nl
    stringBuilder.append(locationBoundConstraint).nl
    stringBuilder.append(latencyPathConstraints).nl
    this
  }

  def writeToFile(zimplFile: File): Unit = {
    zimplFile < stringBuilder.mkString
    stringBuilder.clear()
    latencyPathConstraints.clear()
  }
}

/**
  * ZIMPL formulation which produces the lp relaxation variant of the problem
  * Decision variable y is not limited to be binary.
  * @param name Name/Description for this ZIMPL writer
  */
class LpRoundingZimplWriter(name: String) extends ZimplWriter(name: String) {
  override val vars: String =
    f"""
       |set Z := AE * FN * FN;
       |# Cartesian produt between app and fog nodes
       |set ANFN := AN * FN;
       |
       |# Decides whether an app node is mapped on a fog node
       |var y[ANFN] ;
       |
       |# Decides whether an app edge if mapped on two fog nodes
       |var z[Z];
       |
  """.stripMargin

}

object ZimplWriter {
  def apply(name: String = "Default"): ZimplWriter = new ZimplWriter(name)

  def lp(name: String = "LpRounding") = new LpRoundingZimplWriter(name)
}
