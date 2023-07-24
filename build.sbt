// general project attributes
ThisBuild / organization := "com.uralian"
ThisBuild / organizationName := "Uralian"
ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.6"
ThisBuild / crossScalaVersions := Seq("2.12.14", "2.13.6")

// build options
ThisBuild / wartremoverErrors ++= Warts.unsafe.filterNot { w =>
  import Wart._
  Set(NonUnitStatements, TripleQuestionMark, DefaultArguments) contains w
  //  Set(NonUnitStatements, DefaultArguments, StringPlusAny) contains w
}

// scoverage options
ThisBuild / coverageHighlighting := true
ThisBuild / coverageMinimumStmtTotal := 80

// publishing options
publishTo := sonatypePublishToBundle.value

lazy val root = project.in(file("."))
  .settings(name := "jason-core")
  .settings(
    libraryDependencies ++= commonDependencies ++ testDependencies,
    trapExit := false
  )


lazy val commonDependencies = Seq(
  "com.typesafe" % "config" % "1.4.2",
  "ch.qos.logback" % "logback-classic" % "1.4.8",
  "org.clapper" %% "grizzled-slf4j" % "1.3.4" excludeAll ("org.slf4j"),
  "com.beachape" %% "enumeratum-json4s" % "1.7.3" excludeAll ("org.json4s"),
  "org.json4s" %% "json4s-native" % "4.0.6",
  "com.eed3si9n" %% "treehugger" % "0.4.4",
  "org.scalactic" %% "scalactic" % "3.2.16"
)

lazy val testDependencies = Seq(
  "org.scalatest" %% "scalatest" % "3.2.9" % Test,
  "org.scalatestplus" %% "scalacheck-1-15" % "3.2.9.0" % Test,
  "org.scalatestplus" %% "mockito-3-4" % "3.2.9.0" % Test
)
