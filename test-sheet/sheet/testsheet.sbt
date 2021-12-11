enablePlugins(ScalaJSPlugin)
import sbtcrossproject.CrossPlugin.autoImport.{CrossType, crossProject}
import scala.sys.process._

name := "Test Sheet Root"

ThisBuild / organization := "com.lkroll.roll20"

ThisBuild / version := "1.0.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.7"

ThisBuild / resolvers += "Apache" at "https://repo.maven.apache.org/maven2"
ThisBuild / resolvers += Resolver.sonatypeRepo("releases")
ThisBuild / resolvers += Resolver.mavenLocal

lazy val submitSheet = taskKey[Unit]("Submit the script that assembled and uploads the sheet");
lazy val submit = taskKey[Unit]("Assemble and fastOpt, and then upload the sheet");
lazy val submitSheetFull = taskKey[Unit]("Submit the script that assembled and uploads the sheet in fullOpt");
lazy val submitFull = taskKey[Unit]("Assemble and fullOpt, and then upload the sheet");

submitSheet := {
  s"./assemble.sc --version ${version.value}" !
}

submitSheetFull := {
  s"./assemble.sc --version ${version.value} --full true" !
}

lazy val root = project
  .in(file("."))
  .aggregate(testsheetJS, testsheetJVM)
  .settings(
    publish := {},
    publishLocal := {},
    submit in Compile := Def
      .sequential(
        assembly in Compile in testsheetJVM,
        fastOptJS in Compile in testsheetJS,
        submitSheet in Compile
      )
      .value,
    submitFull in Compile := Def
      .sequential(
        assembly in Compile in testsheetJVM,
        fullOptJS in Compile in testsheetJS,
        submitSheetFull in Compile
      )
      .value
  )

lazy val testsheet = crossProject(JSPlatform, JVMPlatform)
  .in(file("."))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "Test Sheet",
    libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.11.0",
    libraryDependencies += "com.lkroll" %%% "roll20-sheet-framework" % "0.12.0-SNAPSHOT",
    libraryDependencies += "com.lkroll.roll20" %%% "test-model" % version.value,
    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.10" % "test",
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "com.lkroll.roll20.testsheet"
  )
  .jvmSettings(
    // Add JVM-specific settings here
    mainClass in assembly := Some("com.lkroll.roll20.sheet.Packager"),
    libraryDependencies += "org.scala-js" %% "scalajs-stubs" % "1.1.0" % "provided"
  )
  .jsSettings(
    // Add JS-specific settings here
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "2.0.0",
    libraryDependencies += "com.lkroll" %%% "roll20-sheet-facade" % "1.+" % "provided"
  )

lazy val testsheetJVM = testsheet.jvm
lazy val testsheetJS = testsheet.js
