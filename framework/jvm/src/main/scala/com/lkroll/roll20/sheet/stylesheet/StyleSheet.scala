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

import scalatags.Text.all._
import com.lkroll.roll20.core.Renderable

class StyleContext(val selector: Selector) {
  private var children: List[Style] = Nil;
  private var styles: Map[StyleAttribute, StyleValue] = Map.empty;
  private var darkStyles: Map[StyleAttribute, StyleValue] = Map.empty;
  private var lightStyles: Map[StyleAttribute, StyleValue] = Map.empty;

  def addChild(child: Style): Unit = {
    children ::= child;
  }

  def addStyle(attr: StyleAttribute, value: StyleValue): Unit = {
    styles += attr -> value;
  }

  def addDarkStyle(attr: StyleAttribute, value: StyleValue): Unit = {
    darkStyles += attr -> value;
  }

  def addLightStyle(attr: StyleAttribute, value: StyleValue): Unit = {
    lightStyles += attr -> value;
  }

  def result(cascading: Boolean): List[Style] = {
    import SpecialSelectors.Roll20._
    if (cascading) {
      // We don't allow light/dark mode in cascading rules.
      // Prepend no prefix to cascading rules, since they inherit the parent prefix.
      Style(selector, children, styles) :: Nil
    } else {
      val dark = if (darkStyles.isEmpty) {
        Nil
      } else {
        List(Style(selector.prefixWith(DARK_MODE containing CHARSHEET), Nil, darkStyles))
      };
      val light = if (lightStyles.isEmpty) {
        Nil
      } else {
        List(Style(selector.prefixWith(CHARSHEET), Nil, lightStyles))
      };
      val others = Style(selector.prefixWith(CHARSHEET), children, styles) :: Nil
      // dark style needs to come last to override the others.
      others ++ light ++ dark
    }
  }
}

trait StyleAttribute extends Renderable {
  def :-(value: StyleValue)(implicit ctx: StyleContext): Unit = {
    assert(ctx != null, "Assigning values to attribues is only legal within a style block.");
    ctx.addStyle(this, value);
  }
  def :-(value: DualModeValue)(implicit ctx: StyleContext): Unit = {
    assert(ctx != null, "Assigning values to attribues is only legal within a style block.");
    ctx.addDarkStyle(this, value.dark);
    ctx.addLightStyle(this, value.light);
  }
}

trait StyleValue extends Renderable

case class DualModeValue(dark: StyleValue, light: StyleValue)

case class Style(selector: Selector, children: Seq[Style], styles: Map[StyleAttribute, StyleValue]) extends Renderable {
  override def render: String = render(nestingDepth = 0);

  def render(nestingDepth: Int): String = {
    val sb = new StringBuilder;
    val nestingSpace = "  " * nestingDepth;
    sb ++= nestingSpace;
    sb ++= selector.render;
    sb ++= " {\n"
    for ((key, value) <- styles) {
      sb ++= s"$nestingSpace  ${key.render}: ${value.render};";
      sb += '\n';
    }
    for (child <- children) {
      sb ++= child.render(nestingDepth + 1);
      sb += '\n';
    }
    sb ++= nestingSpace;
    sb ++= "}"
    sb.result()
  }
}

trait SheetStyleSheet extends Renderable with ImplicitSelectors {

  implicit class StringValue(s: String) extends StyleValue {
    override def render: String = s
  }

  implicit class RawStyleAttribute(s: String) extends StyleAttribute {
    override def render: String = s
  }
  implicit class ScalatagsAttribute(s: scalatags.generic.Style) extends StyleAttribute {
    override def render: String = s.cssName
  }

  private var styles: List[Style] = Nil;
  private var contextStack: List[StyleContext] = Nil;
  implicit protected def currentContext: StyleContext = {
    assert(contextStack.nonEmpty, "Assigning values to attribues is only legal within a style block.");
    contextStack.head
  }
  implicit class StyleSelector(selector: Selector) {
    def apply(thunk: => Unit): Unit = {
      contextStack ::= new StyleContext(selector);
      thunk;
      contextStack match {
        case Nil => throw new RuntimeException("Unreachable code reached.")
        case ctx :: Nil => {
          contextStack = Nil;
          styles ++= ctx.result(cascading = false);
        }
        case ctx :: parent :: rest => {
          val result = ctx.result(cascading = true);
          assert(result.nonEmpty, "Do not use empty style rules!");
          assert(result.size < 2, "Do not use dark/light mode rules in cascading styles.");
          parent.addChild(result.head);
          contextStack = parent :: rest;
        }
      }
    }
  }

  def dualMode(dark: StyleValue, light: StyleValue): DualModeValue = DualModeValue(dark, light)

  override def render: String = styles.map(_.render).mkString("\n");
}

// TODO: Remove me.
object TestStyleSheet extends SheetStyleSheet {
  val custom = cls("custom");
  val anotherOne = cls("another-one");
  val testy = custom containing div;
  val testy2 = div containing custom;
  val testy3 = (div & custom ~ span) | (custom > div + span) | testy;

  testy3 {
    color :- dualMode(dark = "#AA0000", light = "#330000");
    border :- "1px solid";

    custom {
      textTransform :- "uppercase";
    }
  }

  // TODO: Replace usage of scalatags.generic.Style with our own StyleValue system,
  //        since that one has very rules.

  val testy4 = div(custom, anotherOne, "test").toString;
}
