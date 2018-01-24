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
import SheetImplicits._

//case class ExampleData(lala: String) extends TemplateData {
//  def toMap: Map[String, Renderable] = Map("lala" -> lala)
//}

object ExampleTemplate extends RollTemplate {
  import scalatags.Text.all._

  val test = any("test");
  val str = attribute[Int]("str");
  val roll = rollable[Int]("rollTest");

  override def name: String = "example-template";
  override def content: Tag = div(
    p(str),
    exists(test) {
      p(test)
    },
    rollWasCrit(roll) {
      label("")
    },
    not(rollWasCrit(roll)) {
      div(roll)
    },
    rollTotal(roll, roll) {
      div(roll)
    },
    allProps () { (k, v) =>
      tr(td(k.frag), td(v.frag))
    });
}

class TemplateTest extends FunSuite with Matchers {
  test("Roll Template Rendering") {
    println("******** Template **********");
    println(ExampleTemplate.render);
  }

  test("Roll Template Usage") {
    println("******** Roll With Template **********");
    println(ExampleTemplate(
      ExampleTemplate.str <<= TestSheetModel.str,
      ExampleTemplate.test <<= "testVal",
      ExampleTemplate.roll <<= TestSheetModel.testRoll).render);
  }
}
