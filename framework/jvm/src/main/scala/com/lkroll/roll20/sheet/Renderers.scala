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


object GroupRenderer {
  type FieldRenderer = PartialFunction[(FieldLike[_], RenderMode), Tag];
  type FieldSingleRenderer = Function[FieldLike[_], Tag];
  type FieldDualRenderer = Function2[FieldLike[_], RenderMode, Tag];
  type FieldCombiner = Function[Seq[Tag], Tag];
}

sealed trait RenderMode {}
object RenderMode {
  object Edit extends RenderMode
  object Presentation extends RenderMode
  object Normal extends RenderMode
}

trait GroupRenderer {

  import GroupRenderer._

  def apply(elems: SheetElement*) = GroupWithRenderer(this, elems);

  val defaultFieldRenderer: FieldRenderer = {
    case (f, _) => span(name := f.name, visibility.hidden)
  };

  val defaultFieldCombiner: FieldCombiner = { tags =>
    div(tags)
  };

  def fieldRenderers: FieldRenderer;

  def fieldCombiner: FieldCombiner = defaultFieldCombiner;

  def renderField(f: FieldLike[_], mode: RenderMode): Tag = {
    fieldRenderers.applyOrElse((f, mode), defaultFieldRenderer)
  }

  def renderChild(fg: FieldGroup, mode: RenderMode): Tag = {
    fg.render(mode);
  }

  def renderLabelled(l: LabelsI18N, e: Tag): Tag = div(label(l.attrs), e);

  def renderUnlabelled(e: Tag): Tag = e;

  def renderEditWrapper(e: Tag): Tag = {
    span(TabbedStyle.edit, e)
  }

  def renderPresentationWrapper(e: Tag): Tag = {
    span(TabbedStyle.presentation, e)
  }

  def renderDualModeWrapper(edit: Tag, pres: Tag): Tag = {
    div(edit, pres);
  }

  def renderRoll(roll: Button, e: Tag): Tag = {
    button(`type` := "roll", name := roll.name, value := roll.roll.render,
      e)
  }

  def render(fg: FieldGroup, mode: RenderMode = RenderMode.Normal): Tag = {
    val tags = fg.members().map(renderElement(_, mode));
    fieldCombiner(tags)
  }

  protected def renderElement(e: SheetElement, mode: RenderMode, labelled: Boolean = false): Tag = e match {
    case TagElement(t) if labelled               => t
    case TagElement(t)                           => renderUnlabelled(t)
    case MarkupElement(e)                        => renderElement(e, mode, true)
    case FieldElement(f) if labelled             => renderField(f, mode)
    case FieldElement(f)                         => renderUnlabelled(renderField(f, mode))
    case GroupElement(fg)                        => renderChild(fg, mode)
    case FieldWithRenderer(f, r) if labelled     => r(f)
    case FieldWithRenderer(f, r)                 => renderUnlabelled(r(f))
    case FieldWithDualRenderer(f, r) if labelled => r(f, mode)
    case FieldWithDualRenderer(f, r)             => renderUnlabelled(r(f, mode))
    case LabelledElement(l, e) if labelled       => throw new RuntimeException("Do not put labels within labels!")
    case LabelledElement(l, e)                   => renderLabelled(l, renderElement(e, mode, true))
    case EditOnlyElement(e)                      => renderEditWrapper(renderElement(e, RenderMode.Edit))
    case PresentationOnlyElement(e)              => renderPresentationWrapper(renderElement(e, RenderMode.Presentation))
    case DualModeElement(edit, presentation)     => renderDualModeWrapper(renderElement(edit, RenderMode.Edit), renderElement(presentation, RenderMode.Presentation))
    case RollElement(roll, e) if labelled        => throw new RuntimeException("Do not label rolls!")
    case RollElement(roll, e)                    => renderRoll(roll, renderElement(e, mode))
  }

  val fieldset = tag("fieldset");
}

object DefaultRenderer extends GroupRenderer {

  import GroupRenderer._

  override def fieldRenderers: FieldRenderer = {
    case (b: Button, _)                      => p(button(`type` := "roll", name := b.name, value := b.roll.render))
    case (f: AutocalcField[_], _)            => p(input(`type` := "text", name := f.name, value := f.initialValue, disabled := true))
    case (f: Field[_], _) if f.editable()    => p(input(`type` := "text", name := f.name, value := f.initialValue))
    case (f: Field[_], _) if !(f.editable()) => p(span(name := f.name, f.initialValue))
  };
}

case class DefaultFieldSetRenderer(repeating: RepeatingSection) extends GroupRenderer {
  import GroupRenderer._

  override def fieldRenderers: FieldRenderer = DefaultRenderer.fieldRenderers;

  val repeatingFieldCombiner: FieldCombiner = { tags =>
    fieldset(`class` := repeating.cls, tags)
  };

  override def fieldCombiner: FieldCombiner = repeatingFieldCombiner;
}

case class DefaultRendererGroup(members: Seq[SheetElement]) extends FieldGroup {

  def renderer(): GroupRenderer = DefaultRenderer;
}
