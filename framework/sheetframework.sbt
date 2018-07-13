enablePlugins(ScalaJSPlugin)
//enablePlugins(WorkbenchPlugin)
//enablePlugins(BuildInfoPlugin)

name := "Roll20 Sheet Framework Root"

organization in ThisBuild := "com.lkroll.roll20"

version in ThisBuild := "0.10.5-SNAPSHOT"

scalaVersion in ThisBuild := "2.12.4"
crossScalaVersions in ThisBuild := Seq("2.11.11", "2.12.4")

resolvers += "Apache" at "http://repo.maven.apache.org/maven2"
resolvers += Resolver.bintrayRepo("lkrollcom", "maven")
resolvers += Resolver.mavenLocal

lazy val root = project.in(file(".")).
  aggregate(sheetframeworkJS, sheetframeworkJVM).
  settings(
    publish := {},
    publishLocal := {}
  )

lazy val sheetframework = crossProject.in(file(".")).
  enablePlugins(BuildInfoPlugin).
  settings(
    name := "Roll20 Sheet Framework",
    libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.6.+",
    libraryDependencies += "com.lkroll.roll20" %%% "roll20-core" % "0.12.+",
    libraryDependencies += "com.lkroll.roll20" %%% "roll20-sheet-model" % version.value,
    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.4" % "test",
    EclipseKeys.useProjectId := true,
    EclipseKeys.eclipseOutput := Some("./etarget"),
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "com.lkroll.roll20.sheet"
  ).
  jvmSettings(
    // Add JVM-specific settings here
    libraryDependencies += "org.scala-js" %% "scalajs-stubs" % scalaJSVersion % "provided",
    libraryDependencies += "org.scalactic" %% "scalactic" % "3.+",
    libraryDependencies += "org.rogach" %% "scallop" % "3.+",
    libraryDependencies += "com.lihaoyi" %% "upickle" % "0.6.+",
    libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.+",
    libraryDependencies += "org.codehaus.jettison" % "jettison" % "1.+",
    parallelExecution in Test := false,
    logBuffered in Test := false
  ).
  jsSettings(
    // Add JS-specific settings here
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.+",
    libraryDependencies += "com.lkroll.roll20" %%% "roll20-sheet-facade" % "1.+" % "provided"
  )

lazy val sheetframeworkJVM = sheetframework.jvm
lazy val sheetframeworkJS = sheetframework.js