enablePlugins(ScalaJSPlugin)
import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

ThisBuild / name := "Roll20 Sheet Framework Root"

ThisBuild / organization := "com.lkroll"

ThisBuild / version := "0.11.5"

ThisBuild / scalaVersion := "2.13.5"
ThisBuild / crossScalaVersions := Seq("2.12.13", "2.13.5")

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

ThisBuild / resolvers += Resolver.sonatypeRepo("releases")


lazy val root = project.in(file(".")).
  aggregate(sheetframeworkJS, sheetframeworkJVM).
  settings(publish / skip := true)

lazy val sheetframework = crossProject(JSPlatform, JVMPlatform).in(file(".")).
  enablePlugins(BuildInfoPlugin).
  settings(
    name := "Roll20 Sheet Framework",
    libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.9.+",
    libraryDependencies += "com.lkroll" %%% "roll20-core" % "0.13.3",
    libraryDependencies += "com.lkroll" %%% "roll20-sheet-model" % version.value,
    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.0" % "test",
    libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.0" % "test",
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "com.lkroll.roll20.sheet",
    scalacOptions ++= Seq(
      "-deprecation", 
      "-feature", 
      "-language:implicitConversions",
      "-Xfatal-warnings")
  ).
  jvmSettings(
    // Add JVM-specific settings here
    //libraryDependencies += "org.scala-js" %% "scalajs-stubs" % scalaJSVersion % "provided",
    //libraryDependencies += "org.scalactic" %% "scalactic" % "3.+",
    libraryDependencies += "org.rogach" %% "scallop" % "3.+",
    libraryDependencies += "com.lihaoyi" %% "upickle" % "1.3.+",
    libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.+",
    libraryDependencies += "org.codehaus.jettison" % "jettison" % "1.+",
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    parallelExecution in Test := false,
    logBuffered in Test := false
  ).
  jsSettings(
    // Add JS-specific settings here
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "1.1.+",
    libraryDependencies += "com.lkroll" %%% "roll20-sheet-facade" % "1.0.2" % "provided"
  )

lazy val sheetframeworkJVM = sheetframework.jvm
lazy val sheetframeworkJS = sheetframework.js
