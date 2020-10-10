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

import com.lkroll.roll20.core.{CoreImplicits, Renderable, TemplateApplication, TemplateRef}
import com.lkroll.roll20.sheet.model.APIOutputTemplate

sealed trait ContinuationType;
object ContinuationType {
  case object Full extends ContinuationType;
  case object Header extends ContinuationType;
  case object Middle extends ContinuationType;
  case object Footer extends ContinuationType;
}

trait APIOutputRollTemplate extends RollTemplate with APIOutputTemplate {
  import CoreImplicits._;

  override def ref: TemplateRef = this.name;

  def apply(title: String,
            content: String,
            continuation: ContinuationType = ContinuationType.Full): TemplateApplication = {
    var apps: List[(TemplateField[Nothing], Renderable)] = List(titleField <<= title, contentField <<= content);
    apps ++= continuationToApps(continuation);
    apply(apps: _*)
  }

  def warning(title: String,
              content: String,
              continuation: ContinuationType = ContinuationType.Full): TemplateApplication = {
    var apps: List[(TemplateField[Nothing], Renderable)] =
      List(titleField <<= title, contentField <<= content, isWarning <<= true);
    apps ++= continuationToApps(continuation);
    apply(apps: _*)
  }

  def error(title: String,
            content: String,
            continuation: ContinuationType = ContinuationType.Full): TemplateApplication = {
    var apps: List[(TemplateField[Nothing], Renderable)] =
      List(titleField <<= title, contentField <<= content, isError <<= true);
    apps ++= continuationToApps(continuation);
    apply(apps: _*)
  }

  private def continuationToApps(ct: ContinuationType): List[(TemplateField[Nothing], Renderable)] = {
    import ContinuationType._;
    ct match {
      case Full   => List(showHeader <<= true, showFooter <<= true)
      case Header => List(showHeader <<= true)
      case Middle => Nil
      case Footer => List(showFooter <<= true)
    }
  }

  val titleField = value[String](fields.titleField.name);
  val contentField = value[String](fields.contentField.name);

  val showHeader = value[Boolean](fields.showHeader.name);
  val showFooter = value[Boolean](fields.showFooter.name);

  val isWarning = value[Boolean](fields.isWarning.name);
  val isError = value[Boolean](fields.isError.name);
}
