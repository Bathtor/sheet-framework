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

package com.lkroll.roll20.testmodel

import com.lkroll.roll20.sheet.model._;
import com.lkroll.roll20.core._;

object TestCharModel extends SheetModel {
  import FieldImplicitsLabels._

  implicit class LabelledField[T: Numeric](f: FieldLike[T]) {
    def labelled: RollExprs.LabelledRoll[T] =
      RollExprs.LabelledRoll(f.arith(), f.attr);
  }

  override def version = BuildInfo.version;

  override def outputTemplate: Option[APIOutputTemplate] = Some(APIOutputTemplateRef);

  implicit val ctx = this.renderingContext;

  val sheetName = "TestSheet";
  val author = "Lars Kroll";
  val github = "https://github.com/Bathtor/sheet-framework";

  val characterSheet = text("character_sheet").default(s"$sheetName v???");

  val dynamicField = "dynamic_field".editable(true).options("valuea", "valueb").default("valuea")

}
