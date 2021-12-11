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
import com.lkroll.roll20.sheet.tabbed._
import com.lkroll.roll20.sheet.model._
import com.lkroll.roll20.testmodel.{TestTranslation => TranslationKeys, _}
import scalatags.Text.all._
import scalatags.stylesheet._

object TestSheet extends TabbedSheet {
  import SheetImplicits._;
  import Roll20Predef._;

  val char = TestCharModel;
  val t = TestTranslation;
  val sty = TestStyle;

  override def hidden = Seq[SheetElement](char.characterSheet);
  override def header = Header;
  override def tabs = Seq(core);
  override def footer = Footer;

  val core = tab(t.core, CoreTab);

  override def style: StyleSheet = TestStyle;
  override def externalStyles = List(this.getClass.getClassLoader.getResource("WEB-INF/defaults.css"));
  override def translation: SheetI18NDefaults = TestTranslation;
  override def colourScheme = TestPalette;
}
