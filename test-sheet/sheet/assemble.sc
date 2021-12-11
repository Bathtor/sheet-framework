#!/usr/bin/env amm

import java.io.File;
import ammonite.ops._
import ammonite.ops.ImplicitWd._

def javacp(version: String): Path = pwd / RelPath(s"jvm/target/scala-2.13/Test Sheet-assembly-${version}.jar");
val jsfileFast = "js/target/scala-2.13/test-sheet-fastopt.js";
val jsfileFull = "js/target/scala-2.13/test-sheet-opt.js";
val sheetworkers = "TestSheetWorkers";
val sheet = "com.lkroll.roll20.testsheet.TestSheet";

val htmlfile = "test-sheet.html";
val cssfile = "test-sheet.css";
val translationfile = "translation.json";

@main
def main(version: String, full: Boolean = false): Unit = {
  val javacppath = javacp(version);
  if (full) {
    assemble(javacppath, jsfileFull);
  } else {
    assemble(javacppath, jsfileFast);
  }
}

def assemble(javacp: Path, jsfile: String) {
  try {
    println("Assembling...");
    val cmd = %%(
      'java,
      "-jar",
      javacp.toString,
      "--sheet",
      sheet,
      "--javascript",
      jsfile,
      "--sheetworkers",
      sheetworkers,
      "--html",
      htmlfile,
      "--css",
      cssfile,
      "--translation",
      translationfile
    );
    println("*** ERRORS ***");
    cmd.err.lines.foreach(println);
    println("*** OUT ***");
    cmd.out.lines.foreach(println);
    if (cmd.exitCode == 0) {
      println("Assembly complete.");
    } else {
      Console.err.println("Error while assembling sheet:");
      println(cmd);
    }
  } catch {
    case e: Throwable => e.printStackTrace(Console.err);
  }
}
