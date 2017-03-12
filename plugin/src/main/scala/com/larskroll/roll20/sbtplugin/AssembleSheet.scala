package com.larskroll.roll20.sbtplugin

import sbt._
import com.larskroll.roll20.BuildInfo

object AssembleSheet {
  def apply(js: Attributed[File], java: Seq[Attributed[File]], renderer: String): Attributed[File] = {
    println(s"Build: ${BuildInfo.name}")
    println("***JS***");
    println(js.metadata);
    println(js.data);
    println("***JVM***");
    java.foreach(af => {
      println(af.metadata);
      println(af.data);
    });
    val html = GenerateHTML(renderer, java);
    println(html);
    js
  }
}

object GenerateHTML {
  def apply(renderer: String, java: Seq[Attributed[File]]): String = {
    val cl = classpath.ClasspathUtilities.toLoader(java.map(_.data));
    val rC = cl.loadClass(renderer);
    val r = rC.newInstance();
    return "FIXME";
  }
}
