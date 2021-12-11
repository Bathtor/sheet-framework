enablePlugins(ScalaJSPlugin)
import sbtcrossproject.CrossPlugin.autoImport.{CrossType, crossProject}

name := "Test Model Root"

organization in ThisBuild := "com.lkroll.roll20"

version in ThisBuild := "1.0.0-SNAPSHOT"

scalaVersion in ThisBuild := "2.13.7"

resolvers in ThisBuild += "Apache" at "https://repo.maven.apache.org/maven2"
resolvers in ThisBuild += Resolver.sonatypeRepo("releases")
resolvers in ThisBuild += Resolver.mavenLocal

lazy val root = project
  .in(file("."))
  .aggregate(testmodelJS, testmodelJVM)
  .settings(
    publish := {},
    publishLocal := {}
  )

lazy val testmodel = crossProject(JSPlatform, JVMPlatform)
  .in(file("."))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "Test Model",
    libraryDependencies += "com.lkroll" %%% "roll20-sheet-model" % "0.12.0-SNAPSHOT",
    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.10" % "test",
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "com.lkroll.roll20.testmodel"
  )
  .jvmSettings(
    // Add JVM-specific settings here
    libraryDependencies += "org.scala-js" %% "scalajs-stubs" % "1.1.0" % "provided"
  )
  .jsSettings(
    // Add JS-specific settings here
    libraryDependencies += "com.lkroll" %%% "roll20-sheet-facade" % "1.+" % "provided"
  )

lazy val testmodelJVM = testmodel.jvm
lazy val testmodelJS = testmodel.js
