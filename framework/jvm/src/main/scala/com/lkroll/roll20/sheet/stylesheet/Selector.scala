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
package com.lkroll.roll20.sheet.stylesheet

import scalatags.Text.all.{ConcreteHtmlTag, Modifier}

trait Selector {
  import SpecialSelectors._

  def render: String

  def &(selector: Selector): Selector = And(this, selector);
  def |(selector: Selector): Selector = selector match {
    case Or(selectors) => Or(selector :: selectors)
    case _             => Or(List(this, selector))
  }
  def containing(selector: Selector): Selector = Under(this, selector);
  def >(selector: Selector): Selector = DirectlyUnder(this, selector);
  def +(selector: Selector): Selector = DirectlyAfter(this, selector);
  def ~(selector: Selector): Selector = After(this, selector);

  def prefixWith(selector: Selector): Selector = selector containing this;
}
object SpecialSelectors {
  case class And(left: Selector, right: Selector) extends Selector {
    override def render: String = s"${left.render}${right.render}";
  }
  case class Or(selectors: List[Selector]) extends Selector {

    override def |(selector: Selector): Selector = selector match {
      case Or(rightSelectors) => Or(selectors ::: rightSelectors)
      case _                  => Or(selectors ::: List(selector))
    }

    override def prefixWith(selector: Selector): Selector = Or(selectors.map(s => selector containing s));

    override def render: String = selectors.map(_.render).mkString(",\n")
  }
  case class Under(left: Selector, right: Selector) extends Selector {
    override def render: String = s"${left.render} ${right.render}";
  }
  case class DirectlyUnder(left: Selector, right: Selector) extends Selector {
    override def render: String = s"${left.render} > ${right.render}";
  }
  case class DirectlyAfter(left: Selector, right: Selector) extends Selector {
    override def render: String = s"${left.render} + ${right.render}";
  }
  case class After(left: Selector, right: Selector) extends Selector {
    override def render: String = s"${left.render} ~ ${right.render}";
  }

  case class RawSelector(s: String) extends Selector {
    override def render: String = s;
  }

  object Roll20 {
    final val CHARSHEET = RawSelector(".charsheet");
    final val DARK_MODE = RawSelector("body.sheet-darkmode");
  }
}

case class ClassSelector(className: String) extends Selector with Modifier {
  import scalatags.Text.all._
  override def render: String = s".$className";
  override def applyTo(t: scalatags.text.Builder): Unit =
    t.appendAttr("class", scalatags.text.Builder.GenericAttrValueSource(className));
}

trait ImplicitSelectors {
  def cls(className: String): ClassSelector = ClassSelector(className);
  implicit class TagSelector(tag: ConcreteHtmlTag[String]) extends Selector {
    override def render: String = s"${tag.tag}"
  }
}
