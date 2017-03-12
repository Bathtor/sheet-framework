package com.larskroll.roll20.sbtplugin

import sbt._
import sbt.Keys._
import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.impl.CrossGroupArtifactID;
import sbt.ConfigKey.configurationToKey
import sbt.Def.macroValueIT
import com.larskroll.roll20.BuildInfo

object Roll20SheetPlugin extends AutoPlugin {
  override def requires: Plugins = plugins.JvmPlugin && ScalaJSPlugin;

  val autoImport = AutoImport

  object AutoImport {

    import KeyRanks._

    lazy val fastOptSheet = taskKey[Attributed[File]]("assemble roll20 sheet (fastOptJS for the javascript part)")
    lazy val fullOptSheet = taskKey[Attributed[File]]("assemble roll20 sheet (fullOptJS for the javascript part)")
    lazy val sheetRenderer = settingKey[String]("object that renders the sheet' HTML")
    lazy val sheetVersion = settingKey[String]("sheet framework version")
    lazy val workerProject = settingKey[Project]("sheet worker sub project")
    lazy val htmlProject = settingKey[Project]("sheet layout sub project")
  }
  import autoImport._

  lazy val globalSheetSettings: Seq[Setting[_]] = Seq(
    sheetVersion := BuildInfo.version);

  //  val wp: Project = Def.taskDyn {
  //    workerProject.value
  //  }

  lazy val baseSheetSettings: Seq[Setting[_]] = Seq(
    fastOptSheet := fastOptSheet.dependsOn(compile.in(htmlProject), ScalaJSPlugin.autoImport.fastOptJS.in(workerProject)).value,
    fastOptSheet := AssembleSheet((ScalaJSPlugin.autoImport.fastOptJS.in(workerProject.value)).value, (fullClasspath.in(htmlProject.value, Runtime)).value, (sheetRenderer in Compile).value),
    //fullOptSheet := fullOptSheet.dependsOn(compile, ScalaJSPlugin.autoImport.fullOptJS).value,
    fullOptSheet := AssembleSheet((ScalaJSPlugin.autoImport.fullOptJS in Compile).value, (fullClasspath in Runtime).value, (sheetRenderer in Compile).value),
    //(sheetRenderer in Compile) := "com.larskroll.roll20.Test",
    libraryDependencies += ScalaJSPlugin.autoImport.toScalaJSGroupID("com.larskroll.roll20") %%% "roll20-sheet-framework" % BuildInfo.version);
  //override lazy val projectSettings = inConfig(Compile)(baseSheetSettings)
  override def globalSettings: Seq[Setting[_]] = globalSheetSettings;
  override def projectSettings: Seq[Setting[_]] = baseSheetSettings;
}
