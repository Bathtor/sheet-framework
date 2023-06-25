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

import scalatags.Text.tags._

import com.lkroll.roll20.core.Renderable

case class EmbeddedCss(cssText: String)

class StyleContext(val selector: Selector) {

  private var styles: List[(StyleAttribute, StyleValue)] = Nil;
  private var darkStyles: List[(StyleAttribute, StyleValue)] = Nil;
  private var lightStyles: List[(StyleAttribute, StyleValue)] = Nil;

  def addStyle(attr: StyleAttribute, value: StyleValue): Unit = {
    styles ::= attr -> value;
  }

  def addDarkStyle(attr: StyleAttribute, value: StyleValue): Unit = {
    darkStyles ::= attr -> value;
  }

  def addLightStyle(attr: StyleAttribute, value: StyleValue): Unit = {
    lightStyles ::= attr -> value;
  }

  def result(): List[Style] = {
    import SpecialSelectors.Roll20._
    // reverse all the styles so they override eachother in the expected order.
    val dark = if (darkStyles.isEmpty) {
      Nil
    } else {
      List(Style(selector.prefixWith(DARK_MODE / CHARSHEET), darkStyles.reverse))
    };
    val light = if (lightStyles.isEmpty) {
      Nil
    } else {
      List(Style(selector.prefixWith(CHARSHEET), lightStyles.reverse))
    };
    val others = Style(selector.prefixWith(CHARSHEET), styles.reverse) :: Nil
    // dark style needs to come last to override the others.
    others ++ light ++ dark
  }
}

case class Style(selector: Selector, styles: List[(StyleAttribute, StyleValue)])
  extends Renderable {
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
    sb ++= nestingSpace;
    sb ++= "}"
    sb.result()
  }
}

trait SheetStyleSheet
  extends Renderable
  with StyleAttributes
  with StyleImplicits
  with ImplicitSelectors {

  private var styles: List[Style] = Nil;
  protected var embeddings: List[EmbeddedCss] = Nil;
  private var context: Option[StyleContext] = None;
  implicit protected def currentContext: StyleContext = {
    assert(context.nonEmpty, "Assigning values to attribues is only legal within a style block.");
    context.get
  }
  implicit class StyleSelector(selector: Selector) {
    def apply(thunk: => Unit): Unit = {
      require(context.isEmpty, "Nested rules are not supported");
      context = Some(new StyleContext(selector));
      thunk;
      styles ++= context.get.result();
      context = None;
    }
  }

  def dualMode(dark: String, light: String): DualModeValue = DualModeValue(dark, light)
  def dualMode(dark: Int, light: Int): IntDualModeValue = IntDualModeValue(dark, light)
  def dualMode(dark: Colour, light: Colour): ColourDualModeValue = ColourDualModeValue(dark, light)

  def embed(cssText: String): Unit = {
    require(context.isEmpty, "Do not use embeddings in style rules");
    embeddings ::= EmbeddedCss(cssText);
  }

  override def render: String = styles.map(_.render).mkString("\n");
}

// TODO: Remove me.
// object TestStyleSheet extends SheetStyleSheet {
//   val custom = cls("custom");
//   val anotherOne = cls("another-one");
//   val testy = custom / div;
//   val testy2 = div / custom;
//   val testy3 = (div & custom ~ span) | (custom > div + span) | testy;

//   testy3 {
//     color :- dualMode(dark = "#AA0000", light = "#330000");
//     color :- dualMode(dark = 0xaa0000, light = 0x330000);
//     border :- "1px solid";
//     border.left :- "1px solid";
//     border.left.color :- 0xff0000;

//     margin.left :- 2.px;
//     margin.right :- 2.px;
//     margin.top :- 2.px;
//   }

//   p.selector {
//     textTransform :- "uppercase";
//     color :- dualMode(dark = "#AA0000", light = "#330000");
//   }
// }
