package com.lkroll.roll20.sheet

import org.scalatest._
import scalatags.Text.all._
import scalatags.stylesheet._
import java.io.File
import scala.io.Source

class CSSTest extends FunSuite with Matchers {
  test("CSS comments must be stripped") {
    val srcURL = this.getClass.getClassLoader.getResource("WEB-INF/tabbed.css");
    val source = io.Source.fromURL(srcURL);
    val res = try {
      val orig = source.mkString;
      orig should include ("The MIT License (MIT)");
      orig should include ("input.sheet-toggle-edit-mode");
      CSSUtil.processString(orig)
    } catch {
      case e: Throwable => e.printStackTrace(System.err); fail()
    } finally source.close();
    res shouldNot include ("The MIT License (MIT)");
    res should include ("input.sheet-toggle-edit-mode");
    println(res)
  }

  test("URLs in CSS should NOT be stripped") {
    val str = """
/* latin */
@font-face {
  font-family: 'Abel';
  font-style: normal;
  font-weight: 400;
  src: local('Abel'), local('Abel-Regular'), url(http://fonts.gstatic.com/s/abel/v6/UzN-iejR1VoXU2Oc-7LsbvesZW2xOQ-xsNqO47m55DA.woff2) format('woff2');
  unicode-range: U+0000-00FF, U+0131, U+0152-0153, U+02C6, U+02DA, U+02DC, U+2000-206F, U+2074, U+20AC, U+2212, U+2215;
}
""";
    val expected = """
@font-face {
  font-family: 'Abel';
  font-style: normal;
  font-weight: 400;
  src: local('Abel'), local('Abel-Regular'), url(http://fonts.gstatic.com/s/abel/v6/UzN-iejR1VoXU2Oc-7LsbvesZW2xOQ-xsNqO47m55DA.woff2) format('woff2');
  unicode-range: U+0000-00FF, U+0131, U+0152-0153, U+02C6, U+02DA, U+02DC, U+2000-206F, U+2074, U+20AC, U+2212, U+2215;
}
""";

    val processed = CSSUtil.processString(str);
    processed.trim should equal (expected.trim);
  }
}
