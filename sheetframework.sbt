enablePlugins(ScalaJSPlugin)
//enablePlugins(WorkbenchPlugin)
//enablePlugins(BuildInfoPlugin)

name := "Roll20 Sheet Framework Root"

organization in ThisBuild := "com.larskroll.roll20"

version in ThisBuild := "0.4-SNAPSHOT"

scalaVersion in ThisBuild := "2.11.8"

//resolvers += sbt.Resolver.bintrayRepo("denigma", "denigma-releases")
resolvers += "Apache" at "http://repo.maven.apache.org/maven2"
resolvers += Resolver.mavenLocal

lazy val root = project.in(file(".")).
  aggregate(sheetframeworkJS, sheetframeworkJVM). //, sheetframeworkPlugin).
  settings(
    publish := {},
    publishLocal := {}
  )

lazy val sheetframework = crossProject.in(file(".")).
  enablePlugins(BuildInfoPlugin).
  settings(
    name := "Roll20 Sheet Framework",
    libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.6.3",
    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.0" % "test",
    EclipseKeys.useProjectId := true,
    EclipseKeys.eclipseOutput := Some("./etarget"),
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "com.larskroll.roll20.sheet"
  ).
  jvmSettings(
    // Add JVM-specific settings here
    libraryDependencies += "org.scala-js" %% "scalajs-stubs" % scalaJSVersion % "provided",
    libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1",
    libraryDependencies += "org.rogach" %% "scallop" % "2.1.0",
    libraryDependencies += "com.lihaoyi" %% "upickle" % "0.4.3",
    libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.6",
    libraryDependencies += "org.codehaus.jettison" % "jettison" % "1.3.8",
    parallelExecution in Test := false,
    logBuffered in Test := false
  ).
  jsSettings(
    // Add JS-specific settings here
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.1",
    libraryDependencies += "com.larskroll.roll20" %%% "roll20-sheet-facade" % "1.0-SNAPSHOT" % "provided"
  )

lazy val sheetframeworkJVM = sheetframework.jvm
lazy val sheetframeworkJS = sheetframework.js

//lazy val sheetframeworkPlugin = project.in(file("./plugin")).
//	enablePlugins(BuildInfoPlugin).
//	settings(
//		//organization := "com.larskroll.roll20",
//		name := "sbt Roll20 Sheet Plugin",
//		sbtPlugin := true,
//		scalaVersion := "2.10.6",
//		addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.14"),
//		buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
//    	buildInfoPackage := "com.larskroll.roll20",
//    	EclipseKeys.createSrc := EclipseCreateSrc.Default + EclipseCreateSrc.Managed
//	)

//libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.1"
//libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.6.3"
//libraryDependencies += "com.outr.scribe" %%% "scribe" % "1.2.4"

//jsDependencies += "org.webjars" % "jquery" % "3.1.0" / "jquery.js" minified "jquery.min.js"
//jsDependencies += ProvidedJS / "ShaderPass.js" dependsOn "EffectComposer.js"

//persistLauncher in Compile := true
//persistLauncher in Test := false
//skip in packageJSDependencies := false

//bootSnippet := "com.larskroll.ep.mapviewer.Main().main();"
//refreshBrowsers <<= refreshBrowsers.triggeredBy(fastOptJS in Compile)
//localUrl := ("lkroll.sics.se", 12345)
//localUrl := ("192.168.0.102", 12345)
//localUrl := ("192.168.178.208", 12345)
//localUrl := ("localhost", 12345)