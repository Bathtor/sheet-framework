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

package com.larskroll.roll20.sheet

import scalatags.Text.all._
import scalatags.stylesheet._

case class DivRenderer(style: Seq[Cls]) extends GroupRenderer {

  import GroupRenderer._

  def fieldRenderers: FieldRenderer = DefaultRenderer.fieldRenderers;
  override def fieldCombiner: FieldCombiner = { tags =>
    div(style, tags)
  };
}

object Roll20Predef {
  //  case class Roll20Row(s: Seq[SheetElement]) extends FieldGroup {
  //    def renderer(): GroupRenderer;
  //  }
  def roll20row(elems: SheetElement*): FieldGroup = GroupWithRenderer(DivRenderer(Seq(Roll20Style.row)), elems);
  def roll202colrow(elems: SheetElement*): FieldGroup = GroupWithRenderer(DivRenderer(Seq(Roll20Style.`2colrow`)), elems);
  def roll203colrow(elems: SheetElement*): FieldGroup = GroupWithRenderer(DivRenderer(Seq(Roll20Style.`3colrow`)), elems);
  def roll20col(elems: SheetElement*): FieldGroup = GroupWithRenderer(DivRenderer(Seq(Roll20Style.col)), elems);
}
