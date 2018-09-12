name := "faap-simulator"

version := "1.0"

scalaVersion := "2.12.6"

/** Testing */
libraryDependencies += "org.scalatest" % "scalatest_2.12" % "3.0.5" % "test"

/** Scala-graph */
libraryDependencies += "org.scala-graph" %% "graph-core" % "1.12.5"
libraryDependencies += "org.scala-graph" %% "graph-constrained" % "1.12.3"
libraryDependencies += "org.scala-graph" %% "graph-dot" % "1.12.1"
libraryDependencies += "org.scala-graph" %% "graph-json" % "1.12.1"

/** Logging */
libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3"
libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.0"

/** Convenient IO */
libraryDependencies += "com.github.pathikrit" %% "better-files" % "3.4.0"

javaOptions += "-Xmx4G"