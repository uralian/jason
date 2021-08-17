// general project attributes
organization in ThisBuild := "com.uralian"
organizationName in ThisBuild := "Uralian"
version in ThisBuild := "0.1.0-SNAPSHOT"

scalaVersion in ThisBuild := "2.13.6"
crossScalaVersions in ThisBuild := Seq("2.12.14", "2.13.6")

// build options
wartremoverErrors in ThisBuild ++= Warts.unsafe

// scoverage options
coverageHighlighting in ThisBuild := true
coverageMinimum in ThisBuild := 80

// publishing options
publishTo := sonatypePublishToBundle.value

lazy val root = project.in(file("."))
  .settings(name := "jason-core")
  .settings(
    libraryDependencies ++= commonDependencies ++ testDependencies,
    trapExit := false
  )

lazy val commonDependencies = Seq(
  "com.typesafe" % "config" % "1.4.1",
  "ch.qos.logback" % "logback-classic" % "1.2.5",
  "org.clapper" %% "grizzled-slf4j" % "1.3.4" excludeAll("org.slf4j"),
  "com.beachape" %% "enumeratum-json4s" % "1.7.0" excludeAll("org.json4s"),
  "org.json4s" %% "json4s-native" % "4.0.3",
)

lazy val testDependencies = Seq(
  "org.scalatest" %% "scalatest" % "3.2.9" % Test,
  "org.scalacheck" %% "scalacheck" % "1.15.4" % Test,
  "org.mockito" % "mockito-core" % "3.11.2" % Test
)
