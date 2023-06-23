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

import scalatags.Text.all._
import scalatags.stylesheet._
import java.net.URL
import com.lkroll.roll20.core._
import com.lkroll.roll20.sheet.model._

trait Sheet extends Renderable {
  def renderStyle(): String;
  def renderTranslation(): String;
}

trait SimpleSheet extends Sheet {
  def main: FieldGroup;
  def style: StyleSheet;
  def externalStyles: Seq[URL] = Seq.empty;
  def translation: SheetI18NDefaults;
  override def render: String = main.render().render;
  override def renderStyle(): String = {
    val es = externalStyles.map(styleURL => {
      val source = io.Source.fromURL(styleURL);
      try CSSUtil.processFile(source)
      finally source.close()
    });
    (es ++ Seq(style.styleSheetText)).mkString("\n");
  }
  override def renderTranslation(): String = translation.render;

}

sealed trait SheetElement
case class TagElement(tag: Tag) extends SheetElement
case class MarkupElement(elem: SheetElement) extends SheetElement
case class FieldElement(field: FieldLike[_]) extends SheetElement
case class GroupElement(group: FieldGroup) extends SheetElement
case class FieldWithRenderer(field: FieldLike[_], renderer: GroupRenderer.FieldSingleRenderer)
  extends SheetElement
case class FieldWithDualRenderer(field: FieldLike[_], renderer: GroupRenderer.FieldDualRenderer)
  extends SheetElement
case class LabelledElement(l: LabelsI18N, elem: SheetElement) extends SheetElement
case class EditOnlyElement(elem: Seq[SheetElement]) extends SheetElement
case class PresentationOnlyElement(elem: Seq[SheetElement]) extends SheetElement
case class DualModeElement(edit: SheetElement, presentation: SheetElement) extends SheetElement
case class RollElement(roll: Button, child: SheetElement) extends SheetElement

trait FieldGroup {
  def render(mode: RenderMode = RenderMode.Normal): Tag = {
    renderer.render(this, mode)
  }
  def renderer: GroupRenderer;
  def members: Seq[SheetElement];
}

object FieldGroup {
  def apply(elems: SheetElement*) = DefaultRendererGroup(elems);
  def apply(renderer: GroupRenderer, elems: SheetElement*) = GroupWithRenderer(renderer, elems);
}

case class GroupWithRenderer(renderer: GroupRenderer, members: Seq[SheetElement])
  extends FieldGroup {}

case class FieldSet(
    renderer: GroupRenderer,
    repeating: RepeatingSection,
    members: Seq[SheetElement])
  extends FieldGroup {}
