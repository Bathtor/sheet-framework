package com.lkroll.roll20.sheet

import scala.io.Source
import java.util.regex.Pattern

object CSSUtil {

  lazy val pattern = Pattern.compile("(?:/\\*(?:[^*]|(?:\\*+[^*/]))*\\*+/)|(?:^(http:|https:)//.*)");

  def processString(str: String): String = {
    // Gotta strip out comments, because Roll20 doesn't like them, but Github needs license header
    //println(s"Pattern: ${pattern.toString()}");
    //println(s"Input:\n$str");
    val out = pattern.matcher(str).replaceAll("");
    //println(s"Output:\n$out");
    out
  }

  def processFile(src: Source): String = processString(src.mkString);
}
