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
import com.lkroll.roll20.core._
import com.lkroll.roll20.sheet.model._

object SheetImplicits {
  implicit def seqToFieldGroup(elems: Seq[SheetElement]): FieldGroup = DefaultRendererGroup(elems);
  implicit def fieldToSheetElem(f: FieldLike[_]): FieldElement = FieldElement(f);
  implicit def tagToSheetElem(t: Tag): TagElement = TagElement(t);
  implicit def groupToSheetElem(fg: FieldGroup): GroupElement = GroupElement(fg);

  private val hiddenRenderer: GroupRenderer.FieldSingleRenderer = f => f match {
    case b: Button => button(`type` := "roll", name := b.name, value := b.roll.render, display.none)
    case _         => input(`type` := "hidden", name := f.name, value := f.initialValue)
  }

  implicit class RenderableField(f: FieldLike[_]) {
    def like(r: GroupRenderer.FieldSingleRenderer): FieldWithRenderer = FieldWithRenderer(f, r);
    def like(r: GroupRenderer.FieldDualRenderer): FieldWithDualRenderer = FieldWithDualRenderer(f, r);
    def hidden: FieldWithRenderer = FieldWithRenderer(f, hiddenRenderer);
  }
  implicit class FieldSetBuilder(repeating: RepeatingSection) {
    def apply(elems: SheetElement*): FieldSet = FieldSet(DefaultFieldSetRenderer(repeating), repeating, elems);
  }
  implicit def pairToLabelledElement[E](p: Tuple2[LabelsI18N, E])(implicit f: E => SheetElement): LabelledElement = LabelledElement(p._1, p._2);

  def editOnly[E](e: E)(implicit f: E => SheetElement): EditOnlyElement = EditOnlyElement(Seq(e));
  def editOnly(elems: SheetElement*): EditOnlyElement = EditOnlyElement(elems);
  def presOnly[E](e: E)(implicit f: E => SheetElement): PresentationOnlyElement = PresentationOnlyElement(Seq(e));
  def presOnly(elems: SheetElement*): PresentationOnlyElement = PresentationOnlyElement(elems);
  def dualMode[E](e: E)(implicit f: E => SheetElement): DualModeElement = DualModeElement(editOnly(e), presOnly(e));
  def dualMode[E1, E2](e1: E1, e2: E2)(implicit f1: E1 => SheetElement, f2: E2 => SheetElement): DualModeElement = DualModeElement(editOnly(e1), presOnly(e2));
  def roll[E](roll: Button, e: E)(implicit f: E => SheetElement): RollElement = RollElement(roll, e);
  def roll[E](ctx: RenderingContext, name: String, chat: ChatCommand, template: TemplateApplication, e: E)(implicit f: E => SheetElement): RollElement = RollElement(Button(ctx, name, Rolls.TemplateRoll(chat, template)), e);
  def roll[E](ctx: RenderingContext, name: String, chat: ChatCommand, template: TemplateApplication): Button = Button(ctx, name, Rolls.TemplateRoll(chat, template));
  def roll[E](ctx: RenderingContext, name: String, chatf: FieldLike[ChatCommand], template: TemplateApplication, e: E)(implicit f: E => SheetElement): RollElement = RollElement(Button(ctx, name, Rolls.TemplateRoll(Chat.FromField(chatf), template)), e);
  def roll[E](ctx: RenderingContext, name: String, chatf: FieldLike[ChatCommand], template: TemplateApplication): Button = Button(ctx, name, Rolls.TemplateRoll(Chat.FromField(chatf), template));
  def roll[E](ctx: RenderingContext, name: String, cmd: String, args: List[(String, Renderable)], e: E)(implicit f: E => SheetElement): RollElement = RollElement(Button(ctx, name, Rolls.APIRoll(cmd, args)), e);

  implicit def seqToLabels(labels: Seq[LabelI18N]): LabelsI18N = LabelSeq(labels);
  implicit def labelsToAttrs(labels: LabelsI18N): Modifier = labels.attrs;

  implicit class ValRenderable(v: AnyVal) extends Renderable {
    override def render: String = v.toString();
  }
  implicit class StringRenderable(s: String) extends Renderable {
    override def render: String = s;
  }
  implicit def tfieldToModifier(f: TemplateField[Nothing]): Modifier = f.frag;
  implicit class RollFieldRenderable[T](rf: RollField[T]) extends Renderable {
    override def render: String = rf.inline.render;
  }
  implicit class FieldRenderable[T](f: Field[T]) extends Renderable {
    override def render: String = AutocalcExprs.FieldAccess(f, false).render;
  }
  implicit def i18nRenderable(l: LabelI18N): DynamicLabel = l.dynamic;
  //implicit def stringTemplateField(s: String): TField = TField(s);
  //implicit def tupleToTemplateData[T1, T2](t: Tuple2[T1, T2])(implicit f1: T1 => TemplateField, f2: T2 => Renderable): (TemplateField, Renderable) = (t._1 -> t._2);

}
