enablePlugins(ScalaJSPlugin)
import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

ThisBuild / name := "Roll20 Sheet Model Root"

ThisBuild / organization := "com.lkroll"

ThisBuild / version := "0.11.5-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.5"
ThisBuild / crossScalaVersions := Seq("2.11.12", "2.12.13", "2.13.5")

ThisBuild / licenses += ("MIT", url("http://opensource.org/licenses/MIT"))

ThisBuild / homepage := Some(url("https://github.com/Bathtor/sheet-framework"))
ThisBuild / scmInfo := Some(
                ScmInfo(url("https://github.com/Bathtor/sheet-framework"),
                            "git@github.com:Bathtor/sheet-framework.git"))
ThisBuild / developers := List(Developer(id = "lkroll",
                             name = "Lars Kroll",
                             email = "bathtor@googlemail.com",
                             url = url("https://github.com/Bathtor")))
publishMavenStyle := true

// Add sonatype repository settings
sonatypeCredentialHost := "s01.oss.sonatype.org"
sonatypeRepository := "https://s01.oss.sonatype.org/service/local"
ThisBuild / publishTo := sonatypePublishToBundle.value

lazy val root = project.in(file(".")).
  aggregate(sheetModelJS, sheetModelJVM).
  settings(publish / skip := true)

lazy val sheetModel = crossProject(JSPlatform, JVMPlatform).in(file(".")).
  settings(
    name := "Roll20 Sheet Model",
    libraryDependencies += "com.lkroll" %%% "roll20-core" % "0.13.3",
    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.5" % "test",
  ).
  jvmSettings(
    // Add JVM-specific settings here
    Test / parallelExecution := false,
    Test / logBuffered := false
  ).
  jsSettings(
    // Add JS-specific settings here
  )

lazy val sheetModelJVM = sheetModel.jvm
lazy val sheetModelJS = sheetModel.js
