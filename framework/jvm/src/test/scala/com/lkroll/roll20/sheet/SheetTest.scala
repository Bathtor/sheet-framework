/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2017 Lars Kroll <bathtor@googlemail.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 * 
 */

package com.lkroll.roll20.sheet

import org.scalatest._
import scalatags.Text.all._
import scalatags.stylesheet._
import java.io.File
import scala.io.Source

object TestStyle extends SheetStyle {
  initStyleSheet();
  val x = cls(
    backgroundColor := "red",
    height := 125);
}

object TestSheetModel extends SheetModel {
  import FieldImplicits._
  override def version = "0.0.0";

  implicit val ctx = this.renderingContext;

  val test = "test".default("Test Value");
  val str = "str".default(10);
  val strMod = "str_mod".autocalc(floor(str / 2 - 5));
  val strSave = "str_save".roll((1 d 20).label("D20") + strMod);
  val skills = new RepeatingSection {
    implicit val ctx = this.renderingContext;
    override def name = "skills";
    val mod = "skillmod".default(5);
  }
  val testRoll = "test_roll".roll((1 d 20) + strMod);
}

object Testi18n extends SheetI18N {
  val test = text("test", "Test");
  val skillMod = text("skillmod", "Skill Modifier");
}

object TestSheet extends SimpleSheet {
  val m = TestSheetModel;
  val t = Testi18n;

  import SheetImplicits._
  import Roll20Predef._
  override def main: FieldGroup = roll20row(
    m.versionField like { f => p("Version:", span(name := f.name)) },
    ((t.test ++ t.test.title) -> m.test),
    editOnly(m.str),
    m.strMod,
    m.strSave,
    skillGroup,
    m.test like { tf => span(name := tf.name, SheetI18N.datai18nDynamic) });

  val skillGroup = m.skills(
    label(t.skillMod),
    m.skills.mod);

  override def style(): StyleSheet = TestStyle;
  override def translation(): SheetI18N = Testi18n;
}

class SheetTest extends FunSuite with Matchers {

  test("Sheet and style should render to text") {
    println("******** Sheet **********");
    println(TestSheet.render());
    println("******** Style **********");
    println(TestSheet.renderStyle());
    println("******** i18n **********");
    println(TestSheet.renderTranslation());
  }

  test("Sheet and style should render to file") {
    val html = File.createTempFile("testsheet", ".html");
    val css = File.createTempFile("testsheet", ".css");
    val translation = File.createTempFile("testsheet", ".json");
    Packager.packageSheet(TestSheet, html, css, translation);
    println("******** Sheet **********");
    println(html.getAbsolutePath);
    Source.fromFile(html).foreach {
      print
    }
    println("\n******************");
    println("******** Style **********");
    println(css.getAbsolutePath);
    Source.fromFile(css).foreach {
      print
    }
    println("\n******************");
    println("******** Translation **********");
    println(translation.getAbsolutePath);
    Source.fromFile(translation).foreach {
      print
    }
    println("\n******************");
  }

  test("Accessors and Selectors for fields should return the right values") {
    import TestSheetModel._
    import skills._

    str.accessor shouldBe "str";
    str.selector shouldBe "str";
    mod.accessor shouldBe "repeating_skills_skills_skillmod";
    skills.at("rid", mod).accessor shouldBe "repeating_skills_rid_skills_skillmod";
    mod.selector shouldBe "repeating_skills:skills_skillmod";
    skills.at("rid", mod).selector shouldBe "repeating_skills:skills_skillmod";
    skills.reporder.accessor shouldBe "_reporder_repeating_skills";
    skills.reporder.selector shouldBe "_reporder_repeating_skills";
    TestSheetModel.test.accessor shouldBe "test";
    TestSheetModel.test.selector shouldBe "test";
  }

}
