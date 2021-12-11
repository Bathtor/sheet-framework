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

package com.lkroll.roll20.testsheet

import com.lkroll.roll20.sheet._
import com.lkroll.roll20.sheet.model._
import com.lkroll.roll20.core._
import com.lkroll.roll20.testmodel._
import scalatags.Text.all._

object CoreTab extends FieldGroup {
  import SheetImplicits._

  val char = TestCharModel;
  val t = TestTranslation;
  val sty = TestStyle;

  val dynamicRenderer: GroupRenderer.FieldDualRenderer = (f, mode) => {
    sup(span(name := f.name, SheetI18NAttrs.datai18nDynamic))
  }

  val members: Seq[SheetElement] =
    Seq(
      span("Input:"),
      input(`type` := "text", name := char.dynamicField.name, value := char.dynamicField.defaultValue.get),
      br,
      span("Output:"),
      span(name := char.dynamicField.name),
      br,
      span("Dynamic:"),
      char.dynamicField.like(dynamicRenderer)
    );

  override def renderer = CoreTabRenderer;

}
