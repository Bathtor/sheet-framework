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
import com.lkroll.roll20.core.Renderable
import com.lkroll.roll20.sheet.RollTemplate

trait Selector extends Renderable {
  import SpecialSelectors._

  def &(selector: Selector): And = And(this, selector);
  def |(selector: Selector): Or = selector match {
    case Or(selectors) => Or(selector :: selectors)
    case _             => Or(List(this, selector))
  }
  def /(selector: Selector): Under = Under(this, selector);
  def >(selector: Selector): DirectlyUnder = DirectlyUnder(this, selector);
  def +(selector: Selector): DirectlyAfter = DirectlyAfter(this, selector);
  def ~(selector: Selector): After = After(this, selector);
  def not(selector: Selector): Not = Not(this, selector);

  def prefixWith(selector: Selector): Selector = selector / this;

  def forall(cond: Selector => Boolean): Boolean;
}
object SpecialSelectors {

  trait BinarySelectory extends Selector {
    def left: Selector
    def right: Selector

    override def forall(cond: Selector => Boolean): Boolean =
      cond(this) && cond(left) && cond(right);
  }

  case class And(left: Selector, right: Selector) extends BinarySelectory {
    override def render: String = s"${left.render}${right.render}";
  }
  case class Or(selectors: List[Selector]) extends Selector {

    override def |(selector: Selector): Or = selector match {
      case Or(rightSelectors) => Or(selectors ::: rightSelectors)
      case _                  => Or(selectors ::: List(selector))
    }

    override def prefixWith(selector: Selector): Selector = Or(selectors.map(s => selector / s));

    override def render: String = selectors.map(_.render).mkString(",\n");

    override def forall(cond: Selector => Boolean): Boolean =
      cond(this) && selectors.forall(cond);
  }
  case class Under(left: Selector, right: Selector) extends BinarySelectory {
    override def render: String = s"${left.render} ${right.render}";
  }
  case class DirectlyUnder(left: Selector, right: Selector) extends BinarySelectory {
    override def render: String = s"${left.render} > ${right.render}";
  }
  case class DirectlyAfter(left: Selector, right: Selector) extends BinarySelectory {
    override def render: String = s"${left.render} + ${right.render}";
  }
  case class After(left: Selector, right: Selector) extends BinarySelectory {
    override def render: String = s"${left.render} ~ ${right.render}";
  }
  case class Not(outer: Selector, inner: Selector)
    extends Selector
    with PseudoSelectors
    with AttributeSelectors {
    override def render: String = s"${outer.render}:not(${inner.render})";

    override def forall(cond: Selector => Boolean): Boolean =
      cond(this) && cond(outer) && cond(inner);
  }

  case class RawSelector(s: String) extends Selector {
    override def render: String = s;

    override def forall(cond: Selector => Boolean): Boolean = cond(this);
  }

  case class PseudoClassSelector(main: Selector, pseudoClass: String)
    extends Selector
    with PseudoSelectors
    with AttributeSelectors {
    override def render: String = s"${main.render}:$pseudoClass";

    override def forall(cond: Selector => Boolean): Boolean = cond(this) && cond(main);
  }
  case class PseudoElementSelector(main: Selector, pseudoElement: String)
    extends Selector
    with PseudoSelectors
    with AttributeSelectors {
    override def render: String = s"${main.render}::$pseudoElement";

    override def forall(cond: Selector => Boolean): Boolean = cond(this) && cond(main);
  }

  case class HasAttributeSelector(main: Selector, attribute: String)
    extends Selector
    with PseudoSelectors {
    override def render: String = s"${main.render}[$attribute]";

    override def forall(cond: Selector => Boolean): Boolean = cond(this) && cond(main);
  }
  sealed trait AttributeComparator extends Renderable
  object AttributeComparator {
    case object Equals extends AttributeComparator {
      override def render: String = "=";
    }
    case object Contains extends AttributeComparator {
      override def render: String = "*=";
    }
    case object ContainsSpaceSeparated extends AttributeComparator {
      override def render: String = "~=";
    }
    case object StartsWithDashSeparated extends AttributeComparator {
      override def render: String = "|=";
    }
    case object StartsWith extends AttributeComparator {
      override def render: String = "^=";
    }
    case object EndsWith extends AttributeComparator {
      override def render: String = "$=";
    }
  }
  case class CompareAttributeSelector(
      main: Selector,
      attribute: String,
      comparator: AttributeComparator,
      value: String)
    extends Selector
    with PseudoSelectors {
    override def render: String = s"""${main.render}[$attribute${comparator.render}"$value"]""";

    override def forall(cond: Selector => Boolean): Boolean = cond(this) && cond(main);
  }

  // Hacky way of getting `:pseudo` to render without a left-size class.
  case object Any extends Selector with PseudoSelectors with AttributeSelectors {
    override def render: String = s"";
    override def forall(cond: Selector => Boolean): Boolean = cond(this);
  }
  case object All extends Selector {
    override def render: String = s"*";
    override def forall(cond: Selector => Boolean): Boolean = cond(this);
  }

  object Roll20 {
    final val CHARSHEET = RawSelector(".charsheet");
    final val DARK_MODE = RawSelector("body.sheet-darkmode");
    final val TEMPLATE_DARK_MODE = RawSelector(".sheet-rolltemplate-darkmode");
    def rollTemplateSelector(template: RollTemplate): Selector = RawSelector(
      s".sheet-rolltemplate-${template.name}");
  }
}

case class ClassSelector(className: String)
  extends Selector
  with Modifier
  with PseudoSelectors
  with AttributeSelectors {
  import scalatags.Text.all._
  override def render: String = s".$className";
  override def applyTo(t: scalatags.text.Builder): Unit =
    t.appendAttr("class", scalatags.text.Builder.GenericAttrValueSource(className));

  override def forall(cond: Selector => Boolean): Boolean = cond(this);
}

trait ImplicitSelectors {
  def cls(className: String): ClassSelector = ClassSelector(className);
  implicit class TagSelector(tag: ConcreteHtmlTag[String])
    extends Selector
    with PseudoSelectors
    with AttributeSelectors {
    override def render: String = s"${tag.tag}";
    override def forall(cond: Selector => Boolean): Boolean = cond(this);

    // Allows to force this to convert to a Selector instance.
    def selector: TagSelector = this;
  }
  final val ANY = SpecialSelectors.Any;
  final val ALL = SpecialSelectors.All;
}

trait PseudoClassSelectors { this: Selector =>

  private def pseudoExtend(s: String): SpecialSelectors.PseudoClassSelector =
    SpecialSelectors.PseudoClassSelector(this, s);
  // https://developer.mozilla.org/en-US/docs/Web/CSS/Pseudo-classes
  def active = pseudoExtend("active")
  def checked = pseudoExtend("checked")
  def default = pseudoExtend("default")
  def disabled = pseudoExtend("disabled")
  def empty = pseudoExtend("empty")
  def enabled = pseudoExtend("enabled")
  def first = pseudoExtend("first")
  def firstChild = pseudoExtend("first-child")
  def firstOfType = pseudoExtend("first-of-type")
  def fullscreen = pseudoExtend("fullscreen")
  def focus = pseudoExtend("focus")
  def hover = pseudoExtend("hover")
  def indeterminate = pseudoExtend("indeterminate")
  def inRange = pseudoExtend("in-range")
  def invalid = pseudoExtend("invalid")
  def lastChild = pseudoExtend("last-child")
  def lastOfType = pseudoExtend("last-of-type")
  def left = pseudoExtend("left")
  def link = pseudoExtend("link")
  def onlyChild = pseudoExtend("only-child")
  def onlyOfType = pseudoExtend("only-of-type")
  def optional = pseudoExtend("optional")
  def outOfRange = pseudoExtend("out-of-range")
  def readOnly = pseudoExtend("read-only")
  def readWrite = pseudoExtend("read-write")
  def required = pseudoExtend("required")
  def right = pseudoExtend("right")
  def root = pseudoExtend("root")
  def scope = pseudoExtend("scope")
  def target = pseudoExtend("target")
  def valid = pseudoExtend("valid")
  def visited = pseudoExtend("visited")

  def nthChild(selector: String) = pseudoExtend(s"nth-child($selector)")
}

trait PseudoElementSelectors { this: Selector =>

  private def pseudoExtend(s: String): SpecialSelectors.PseudoElementSelector =
    SpecialSelectors.PseudoElementSelector(this, s);
  // https://www.w3schools.com/css/css_pseudo_elements.asp
  def firstLine = pseudoExtend("first-line");
  def firstLetter = pseudoExtend("first-letter");
  def after = pseudoExtend("after");
  def before = pseudoExtend("before");
  def marker = pseudoExtend("marker");
  def selection = pseudoExtend("selection");
}

trait PseudoSelectors extends PseudoClassSelectors with PseudoElementSelectors { this: Selector => }

trait AttributeSelectors { this: Selector =>
  import SpecialSelectors.{CompareAttributeSelector, HasAttributeSelector}
  import SpecialSelectors.AttributeComparator._

  def hasAttribute(name: String): HasAttributeSelector = HasAttributeSelector(this, name);
  def attributeEquals(name: String, value: String): CompareAttributeSelector =
    CompareAttributeSelector(this, name, Equals, value);
  def attributeContains(name: String, value: String): CompareAttributeSelector =
    CompareAttributeSelector(this, name, Contains, value);

  def attributeContainsInList(name: String, value: String): CompareAttributeSelector =
    CompareAttributeSelector(this, name, ContainsSpaceSeparated, value);

  def attributeStartsWithDashSeparated(name: String, value: String): CompareAttributeSelector =
    CompareAttributeSelector(this, name, StartsWithDashSeparated, value);

  def attributeStartsWith(name: String, value: String): CompareAttributeSelector =
    CompareAttributeSelector(this, name, StartsWith, value);
  def attributeEndsWith(name: String, value: String): CompareAttributeSelector =
    CompareAttributeSelector(this, name, EndsWith, value);
}
