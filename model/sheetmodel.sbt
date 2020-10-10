enablePlugins(ScalaJSPlugin)
import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

name := "Roll20 Sheet Model Root"

organization in ThisBuild := "com.lkroll.roll20"

version in ThisBuild := "0.11.2"

scalaVersion in ThisBuild := "2.12.11"
crossScalaVersions in ThisBuild := Seq("2.12.11")

resolvers += "Apache" at "https://repo.maven.apache.org/maven2"
resolvers += Resolver.bintrayRepo("lkrollcom", "maven")
resolvers += Resolver.mavenLocal

lazy val root = project.in(file(".")).
  aggregate(sheetModelJS, sheetModelJVM).
  settings(
    publish := {},
    publishLocal := {}
  )

lazy val sheetModel = crossProject(JSPlatform, JVMPlatform).in(file(".")).
  settings(
    name := "Roll20 Sheet Model",
    libraryDependencies += "com.lkroll.roll20" %%% "roll20-core" % "0.13.1",
    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.0" % "test",
  ).
  jvmSettings(
    // Add JVM-specific settings here
    parallelExecution in Test := false,
    logBuffered in Test := false
  ).
  jsSettings(
    // Add JS-specific settings here
  )

lazy val sheetModelJVM = sheetModel.jvm
lazy val sheetModelJS = sheetModel.js

licenses in ThisBuild += ("MIT", url("http://opensource.org/licenses/MIT"))
bintrayPackageLabels in ThisBuild := Seq("roll20")
bintrayOrganization in ThisBuild := Some("lkrollcom")
bintrayRepository in ThisBuild := "maven"
