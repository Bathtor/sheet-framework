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
import com.lkroll.roll20.testmodel.{TestTranslation => TranslationKeys, _}
import scalatags.Text.all._

trait CoreTabRenderer extends GroupRenderer {
  import GroupRenderer._
  import RenderMode._

  implicit def obool2Checked(ob: Option[Boolean]): Modifier = ob match {
    case Some(true)  => checked
    case Some(false) => ()
    case None        => ()
  }

  def renderNumberField[N](f: NumberField[N]): Tag = {
    import Math.{max => nmax, abs, floor, log10};
    implicit val nev = f.numericEvidence;
    f.valid match {
      case Some(NumberValidity(minV, maxV, stepV)) => {
        val (minD, maxD, stepD) = (nev.toDouble(minV), nev.toDouble(maxV), nev.toDouble(stepV));
        assert(maxD > minD);
        val largestBound = nmax(abs(maxD), abs(minD));
        val integerDigits = floor(log10(abs(largestBound))).toInt + 1; // +1 for first digit
        var count = 0;
        var v = stepD;
        while (!v.isWhole) {
          v *= 10.0;
          count += 1;
        }
        val decimalDigits = count + 1; // for the decimal point
        val digits = integerDigits + decimalDigits + (if (nev.signum(minV) == -1) {
                                                        1
                                                      } else {
                                                        0
                                                      }); // +1 for leading minus if necessary
        span(
          TestStyle.inlineNumber,
          maxWidth := digits.em,
          input(`type` := "number",
                name := f.name,
                value := f.initialValue,
                min := minV.toString(),
                max := maxV.toString(),
                step := stepV.toString())
        )
      }
      case None => {
        span(TestStyle.max3charinline, input(`type` := "number", name := f.name, value := f.initialValue))
      }
    }

  }

  def renderNumberFieldNoWidth[N](f: NumberField[N]): Tag = {
    f.valid match {
      case Some(NumberValidity(minV, maxV, stepV)) => {
        input(`type` := "number",
              name := f.name,
              value := f.initialValue,
              min := minV.toString(),
              max := maxV.toString(),
              step := stepV.toString())
      }
      case None => {
        input(`type` := "number", name := f.name, value := f.initialValue)
      }
    }
  }

  private def enumRender(ef: EnumField): Tag = {

    val selector: String => Option[Modifier] = ef.defaultValue match {
      case Some(dv) =>
        println(s"Default value for $ef is $dv");
        (o: String) =>
          if (dv == o) {
            //println(s"Match of $dv with $o!");
            Some(selected)
          } else {
            //println(s"No match between $dv and $o");
            None
          }
      case None => println(s"No default value for $ef"); (o: String) => None
    }
    ef.enumeration match {
      case Some(e) =>
        TestTranslation.allFullOptions.get(e) match {
          case Some(l) => {
            val options = ef.options.map((o) => option(value := o, l.apply(o).attr, selector(o))).toSeq
            select(name := ef.name, options)
          }
          case None =>
            println(s"Translation missing for enumeration: ${e.toString()}");
            select(name := ef.name, ef.options.map(o => option(value := o, o, selector(o))).toSeq)
        }
      case None => select(name := ef.name, ef.options.map(o => option(value := o, o, selector(o))).toSeq)
    }

  }

  override def fieldRenderers: FieldRenderer = {
    case (b: Button, _) =>
      button(`type` := "roll", name := b.name, value := b.roll.render)
    case (f: AutocalcField[_], _) =>
      span(input(`type` := "hidden", name := f.name, value := f.initialValue), span(name := f.name))
    case (f: Field[_], Normal) if f.editable() =>
      f match {
        case n: NumberField[_] => renderNumberField(n)
        case ff: FlagField     => input(`type` := "checkbox", name := ff.name, ff.defaultValue)
        case ef: EnumField     => enumRender(ef)
        case _                 => input(`type` := "text", name := f.name, value := f.initialValue)
      }
    case (f: Field[_], Presentation) if f.editable() =>
      f match {
        case ff: FlagField => input(`type` := "checkbox", name := ff.name, ff.defaultValue)
        case _             => span(TestStyle.labelledValue, name := f.name)
      }

    case (f: Field[_], Edit) if f.editable() =>
      f match {
        case n: NumberField[_] => renderNumberField(n)
        case ff: FlagField     => input(`type` := "checkbox", name := ff.name, ff.defaultValue)
        case ef: EnumField     => enumRender(ef)
        case _                 => input(`type` := "text", name := f.name, value := f.initialValue)
      }
    case (f: Field[_], _) if !(f.editable()) =>
      span(input(`type` := "hidden", name := f.name, value := f.initialValue),
           span(TestStyle.labelledValue, name := f.name))
  };

  val textareaField: FieldDualRenderer = (f, mode) => {
    mode match {
      case RenderMode.Edit | RenderMode.Normal =>
        textarea(TestStyle.`two-line-textarea`, name := f.name, f.initialValue)
      case RenderMode.Presentation => span(TestStyle.labelledValue, name := f.name)
    }
  }

  val textareaFieldGrow: FieldDualRenderer = (f, mode) => {
    mode match {
      case RenderMode.Edit | RenderMode.Normal =>
        div(TestStyle.`flex-grow`,
            TestStyle.inlineContentGroup,
            textarea(TestStyle.`two-line-textarea`, name := f.name, f.initialValue))
      case RenderMode.Presentation => span(TestStyle.labelledValue, name := f.name)
    }
  }

  val largeTextareaField: FieldDualRenderer = (f, mode) => {
    mode match {
      case RenderMode.Edit | RenderMode.Normal =>
        textarea(TestStyle.`eight-line-textarea`, name := f.name, f.initialValue)
      case RenderMode.Presentation => span(TestStyle.labelledValue, name := f.name)
    }
  }

  def largeTextareaFieldWithPlaceholder(ph: DataLabel): FieldDualRenderer = (f, mode) => {
    mode match {
      case RenderMode.Edit | RenderMode.Normal =>
        textarea(TestStyle.`eight-line-textarea`, name := f.name, f.initialValue, ph.placeholder.attrs)
      case RenderMode.Presentation => span(TestStyle.labelledValue, name := f.name)
    }
  }

  val presEditableNum: FieldDualRenderer = (f, mode) => {
    f match {
      case n: NumberField[_] => renderNumberField(n)
      case _                 => span(TestStyle.max3charinline, input(`type` := "number", name := f.name, value := f.initialValue))
    }
  }

  def textWithPlaceholder(placeholder: PlaceholderLabel): FieldSingleRenderer =
    (f) => input(`type` := "text", name := f.name, value := f.initialValue, placeholder.attrs)

  val descriptionToggle: FieldSingleRenderer = (f) => {
    input(`type` := "checkbox", name := f.name, TestStyle.`description-toggle`)
  }

  val descriptionToggleWrapped: FieldSingleRenderer = (f) => {
    label(
      TestStyle.`toggle-wrapper-label`,
      input(`type` := "checkbox", name := f.name, TestStyle.`description-toggle`),
      span(TestTranslation.showHideDescription.title.attr)
    )
  }

  val inlineDescription: FieldSingleRenderer = (f) => {
    span(TestStyle.description, raw(" &mdash; "), span(name := f.name))
  }

  val description: FieldSingleRenderer = (f) => {
    span(TestStyle.description, span(name := f.name))
  }

  val italic: FieldSingleRenderer = (f) => {
    span(fontStyle.italic, TestStyle.labelledValue, name := f.name)
  }

  def labelDescription(label: LabelsI18N) = span(TestStyle.description, raw(" &mdash; "), span(label.attrs));
}

object CoreTabRenderer extends CoreTabRenderer;
