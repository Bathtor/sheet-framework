enablePlugins(ScalaJSPlugin)
import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

name := "Roll20 Sheet Framework Root"

organization in ThisBuild := "com.lkroll.roll20"

version in ThisBuild := "0.11.2"

scalaVersion in ThisBuild := "2.12.11"
crossScalaVersions in ThisBuild := Seq("2.12.11")

resolvers += "Apache" at "https://repo.maven.apache.org/maven2"
resolvers += Resolver.bintrayRepo("lkrollcom", "maven")
resolvers += Resolver.mavenLocal

lazy val root = project.in(file(".")).
  aggregate(sheetframeworkJS, sheetframeworkJVM).
  settings(
    publish := {},
    publishLocal := {}
  )

lazy val sheetframework = crossProject(JSPlatform, JVMPlatform).in(file(".")).
  enablePlugins(BuildInfoPlugin).
  settings(
    name := "Roll20 Sheet Framework",
    libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.9.+",
    libraryDependencies += "com.lkroll.roll20" %%% "roll20-core" % "0.13.1",
    libraryDependencies += "com.lkroll.roll20" %%% "roll20-sheet-model" % version.value,
    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.0" % "test",
    libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.0" % "test",
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "com.lkroll.roll20.sheet"
  ).
  jvmSettings(
    // Add JVM-specific settings here
    //libraryDependencies += "org.scala-js" %% "scalajs-stubs" % scalaJSVersion % "provided",
    //libraryDependencies += "org.scalactic" %% "scalactic" % "3.+",
    libraryDependencies += "org.rogach" %% "scallop" % "3.+",
    libraryDependencies += "com.lihaoyi" %% "upickle" % "1.2.+",
    libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.+",
    libraryDependencies += "org.codehaus.jettison" % "jettison" % "1.+",
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    parallelExecution in Test := false,
    logBuffered in Test := false
  ).
  jsSettings(
    // Add JS-specific settings here
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "1.1.+",
    libraryDependencies += "com.lkroll.roll20" %%% "roll20-sheet-facade" % "1.0.1" % "provided"
  )

lazy val sheetframeworkJVM = sheetframework.jvm
lazy val sheetframeworkJS = sheetframework.js

licenses in ThisBuild += ("MIT", url("http://opensource.org/licenses/MIT"))
bintrayPackageLabels in ThisBuild := Seq("roll20")
bintrayOrganization in ThisBuild := Some("lkrollcom")
bintrayRepository in ThisBuild := "maven"
