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

import com.lkroll.roll20.core.Renderable

sealed trait StyleValue extends Renderable

case class StringValue(s: String) extends StyleValue {
  override def render: String = s
}

// This could be made much easier with generics, but since they get elided at runtime, invocation
// may pick the wrong function based on generics parameters alone.
case class DualModeValue(dark: String, light: String)
case class IntDualModeValue(dark: Int, light: Int) {
  def map[R](f: Int => String): DualModeValue = DualModeValue(dark = f(dark), light = f(light));
}
case class ColourDualModeValue(dark: Colour, light: Colour) {
  def css: DualModeValue = DualModeValue(dark = dark.css, light = light.css);
}

sealed trait StyleAttribute extends Renderable {
  def :-(value: String)(implicit ctx: StyleContext): Unit = {
    assert(ctx != null, "Assigning values to attribues is only legal within a style block.");
    ctx.addStyle(this, StringValue(value));
  }
  def :-(value: DualModeValue)(implicit ctx: StyleContext): Unit = {
    assert(ctx != null, "Assigning values to attribues is only legal within a style block.");
    ctx.addDarkStyle(this, StringValue(value.dark));
    ctx.addLightStyle(this, StringValue(value.light));
  }
}

case class RawStyleAttribute(cssName: String) extends StyleAttribute {
  override def render: String = cssName;
}

case class IntStyleAttribute(cssName: String) extends StyleAttribute {
  override def render: String = cssName;

  def :-(value: Int)(implicit ctx: StyleContext): Unit = {
    ctx.addStyle(this, StringValue(value.toString));
  }
}

case class DoubleStyleAttribute(cssName: String) extends StyleAttribute {
  override def render: String = cssName;

  def :-(value: Double)(implicit ctx: StyleContext): Unit = {
    ctx.addStyle(this, StringValue(value.toString));
  }
}

case class ColourStyleAttribute(cssName: String) extends StyleAttribute {
  override def render: String = cssName;

  def :-(value: Int)(implicit ctx: StyleContext): Unit = {
    val hexValue = value.toHexString.toUpperCase;
    assert(hexValue.size <= 6, s"Illegal hex colour code: $hexValue ($value)");
    ctx.addStyle(this, StringValue(s"#$hexValue"));
  }

  def :-(value: Colour)(implicit ctx: StyleContext): Unit = {
    ctx.addStyle(this, StringValue(value.css));
  }

  def :-(dualModeValue: IntDualModeValue)(implicit ctx: StyleContext): Unit = {
    val mappedValue = dualModeValue.map { value =>
      val hexValue = value.toHexString.toUpperCase;
      assert(hexValue.size <= 6, s"Illegal hex colour code: $hexValue ($value)");
      s"#$hexValue"
    }
    this :- mappedValue
  }

  def :-(dualModeValue: ColourDualModeValue)(implicit ctx: StyleContext): Unit = {
    this :- dualModeValue.css
  }
}

sealed trait SizeValue {
  def toStyleValue: StyleValue
}
object SizeValue {
  case class Pixels[N: Numeric](num: N) extends SizeValue {
    override def toStyleValue: StyleValue = StringValue(s"${num}px");
  }
  case class Points[N: Numeric](num: N) extends SizeValue {
    override def toStyleValue: StyleValue = StringValue(s"${num}pt");
  }

  case class ElementFontSizeRelative[N: Numeric](num: N) extends SizeValue {
    override def toStyleValue: StyleValue = StringValue(s"${num}em");
  }

  case class RootFontSizeRelative[N: Numeric](num: N) extends SizeValue {
    override def toStyleValue: StyleValue = StringValue(s"${num}rem");
  }

  case class Percent[N: Numeric](num: N) extends SizeValue {
    override def toStyleValue: StyleValue = StringValue(s"${num}%");
  }
}

trait SizeValueImplicits {
  import SizeValue._

  implicit class SizeExtInt(num: Int) {
    def px: SizeValue = Pixels(num);
    def pt: SizeValue = Points(num);
    def em: SizeValue = ElementFontSizeRelative(num);
    def rem: SizeValue = RootFontSizeRelative(num);
    def perc: SizeValue = Percent(num);
  }

  implicit class SizeExtDouble(num: Double) {
    def px: SizeValue = Pixels(num);
    def pt: SizeValue = Points(num);
    def em: SizeValue = ElementFontSizeRelative(num);
    def rem: SizeValue = RootFontSizeRelative(num);
    def perc: SizeValue = Percent(num);
  }
}

trait TSizeStyleAttribute extends StyleAttribute {
  def :-(value: SizeValue)(implicit ctx: StyleContext): Unit = {
    ctx.addStyle(this, value.toStyleValue);
  }
  // Some size things (like line-height) accept a simple double value.
  def :-(value: Double)(implicit ctx: StyleContext): Unit = {
    ctx.addStyle(this, StringValue(value.toString));
  }
}
case class SizeStyleAttribute(cssName: String) extends TSizeStyleAttribute {
  override def render: String = cssName;
}

trait StyleAttributes {

  def attr(s: String): RawStyleAttribute = RawStyleAttribute(s);

  val alignItems = attr("align-items");

  trait PositionalAttributes[T] {
    def prefix: String
    def nestedAttribute(parentPrefix: String): T

    val all = nestedAttribute(prefix);
    val left = nestedAttribute(s"${prefix}-left");
    val right = nestedAttribute(s"${prefix}-right");
    val top = nestedAttribute(s"${prefix}-top");
    val bottom = nestedAttribute(s"${prefix}-bottom");
  }

  val color = ColourStyleAttribute("color");

  val background = attr("background");
  val backgroundColor = ColourStyleAttribute("background-color");
  val backgroundImage = attr("background-image");
  val backgroundRepeat = attr("background-repeat");
  val backgroundPosition = attr("background-position");
  val backgroundSize = SizeStyleAttribute("background-size");

  trait BorderAttributes extends StyleAttribute {

    def prefix: String
    override def render: String = prefix;

    lazy val color = ColourStyleAttribute(s"${prefix}-color");
    lazy val radius = SizeStyleAttribute(s"${prefix}-radius");
    lazy val style = attr(s"${prefix}-style");
    lazy val width = SizeStyleAttribute(s"${prefix}-width");
  }
  object border
    extends PositionalAttributes[BorderAttributes]
    with BorderAttributes
    with StyleAttribute {
    override lazy val prefix: String = "border";
    override def nestedAttribute(parentPrefix: String): BorderAttributes = new BorderAttributes {
      override lazy val prefix: String = parentPrefix;
    }
    override def render: String = prefix;
  }
  val bottom = SizeStyleAttribute("bottom");
  val boxShadow = attr("box-shadow");
  val boxSizing = attr("box-sizing");

  val clear = attr("clear");
  val content = attr("content");
  val cursor = attr("cursor");

  val display = attr("display");

  val flexDirection = IntStyleAttribute("flex-direction");
  val flexGrow = IntStyleAttribute("flex-grow");
  val flexWrap = attr("flex-wrap");
  val float = attr("float");
  val fontFamily = attr("font-family");
  val fontSize = SizeStyleAttribute("font-size");
  val fontWeight = IntStyleAttribute("font-weight");

  val height = SizeStyleAttribute("height");

  val left = SizeStyleAttribute("left");
  val lineHeight = SizeStyleAttribute("line-height");

  object margin extends PositionalAttributes[TSizeStyleAttribute] with TSizeStyleAttribute {
    override lazy val prefix: String = "margin";
    override def nestedAttribute(parentPrefix: String): TSizeStyleAttribute =
      new TSizeStyleAttribute {
        override def render: String = parentPrefix;
      }
    override def render: String = prefix;
  }

  val maxHeight = SizeStyleAttribute("max-height");
  val maxWidth = SizeStyleAttribute("max-width");
  val minHeight = SizeStyleAttribute("min-height");
  val minWidth = SizeStyleAttribute("min-width");

  val opacity = DoubleStyleAttribute("opacity");
  val outline = attr("outline");
  val overflow = attr("overflow");

  object padding extends PositionalAttributes[TSizeStyleAttribute] with TSizeStyleAttribute {
    override lazy val prefix: String = "padding";
    override def nestedAttribute(parentPrefix: String): TSizeStyleAttribute =
      new TSizeStyleAttribute {
        override def render: String = parentPrefix;
      }
    override def render: String = prefix;
  }
  val position = attr("position");

  val resize = attr("resize");
  val right = SizeStyleAttribute("right");

  val textAlign = attr("text-align");
  val textIndent = SizeStyleAttribute("text-indent");
  val textShadow = attr("text-shadow");
  val textTransform = attr("text-transform");
  val top = SizeStyleAttribute("top");

  val userSelect = attr("user-select");

  val verticalAlign = attr("vertical-align");
  val visibility = attr("visibility");

  val whiteSpace = attr("white-space");
  val width = SizeStyleAttribute("width");

  val zIndex = IntStyleAttribute("z-index");
}

trait StyleImplicits extends SizeValueImplicits {
  // TODO: Put something or remove
}
