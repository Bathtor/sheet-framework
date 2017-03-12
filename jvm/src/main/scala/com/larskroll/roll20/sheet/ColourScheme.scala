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

package com.larskroll.roll20.sheet

import collection.mutable;
import scala.util.matching.Regex;

trait Colour {
  def css: String;
}

case class ColourNum(n: Int) extends Colour {
  override def css: String = {
    val masked = (0xFFFFFF & n);
    "#%06X".format(masked);
  }
}

case class ColourString(s: String) extends Colour {
  override def css: String = s;
}

sealed trait ColourLiteralMode {}
case object RGB extends ColourLiteralMode
case object HSL extends ColourLiteralMode

case class ColourLiteral(mode: ColourLiteralMode, rh: Int, gs: Int, bl: Int, a: Option[Double] = None) extends Colour {
  override def css: String = (mode, a) match {
    case (RGB, Some(av)) => s"rgba($rh, $gs, $bl, $av)";
    case (RGB, None)     => s"rgb($rh, $gs, $bl)";
    case (HSL, Some(av)) => s"hsla($rh, $gs, $bl, $av)";
    case (HSL, None)     => s"hsl($rh, $gs, $bl)";
  }
}

case object Transparent extends Colour {
  override def css: String = "transparent";
}

case class ColourField(key: String, colour: Colour) {
  def css: String = colour.css;
}

trait ColourScheme {

  private val colours = mutable.Map.empty[String, ColourField];
  private val aliases = mutable.Map.empty[String, String];

  def apply(key: String): Colour = {
    this.get(key) match {
      case Some(c) => c
      case None    => throw new NoSuchElementException(key);
    }
  }

  def get(key: String): Option[Colour] = {
    colours.get(key) match {
      case Some(cf) => Some(cf.colour)
      case None => {
        aliases.get(key) match {
          case Some(ckey) => colours.get(ckey).map(_.colour)
          case None       => None
        }
      }
    }
  }

  // Just in case I change my mind later on how the fields should look like
  protected def colourToField(key: String, c: Colour): ColourField = {
    val cf = ColourField(key, c);
    colours += (key -> cf);
    cf
  }

  def defaultColour = hex("default", 0);

  def hex(key: String, num: Int) = colourToField(key, ColourNum(num));
  def css(key: String, s: String) = colourToField(key, ColourString(s));
  def rgb(key: String, r: Int, g: Int, b: Int) = colourToField(key, ColourLiteral(RGB, r, g, b));
  def rgba(key: String, r: Int, g: Int, b: Int, a: Double) = colourToField(key, ColourLiteral(RGB, r, g, b, Some(a)));
  def hsl(key: String, h: Int, s: Int, l: Int) = colourToField(key, ColourLiteral(HSL, h, s, l));
  def hsla(key: String, h: Int, s: Int, l: Int, a: Double) = colourToField(key, ColourLiteral(HSL, h, s, l, Some(a)));
  def transparent(key: String) = colourToField(key, Transparent);

  def alias(aliasKey: String, colourKey: String): ColourField = {
    val cf = colours(colourKey);
    assert(cf != null);
    aliases += (aliasKey -> colourKey);
    cf // change if colourToField changes!
  }

  def alias(aliasKey: String, cf: ColourField): ColourField = alias(aliasKey, cf.key); // this will make sure you don't a field from another palette

  def replaceColoursInText(input: String): String = ColourScheme.replacePattern.replaceAllIn(input, _ match {
    case m if m.subgroups.size > 0 => this.get(m.subgroups(0)) match {
      case Some(c) => c.css
      case None => {
        println(s"Found no match for placeholder ($m). Using the defaultColour.");
        defaultColour.css
      }
    }
    case m => {
      println(s"Invalid match ($m). Using the defaultColour.");
      defaultColour.css
    }
  });
}

object ColourScheme {
  val replacePattern = """\$\{((\w|\-)+)\}""".r;
}

case class XMLColorPalette(data: scala.xml.Node) extends ColourScheme {

  assert(data.label == "palette");

  val source: String = data.child.flatMap {
    case <url>{ text }</url> => Some(text.text)
    case _                   => None
  }.apply(0);

  val colourSets = data.child.flatMap {
    case cs @ <colorset>{ _* }</colorset> => Some(cs)
    case x                                => None
  };

  def getColourModulo(setId: Int, colourId: Int): ColourField = {

    val colourSet = if (colourSets.size > setId) {
      colourSets(setId)
    } else {
      println(s"Provided palette does not have a colour set $setId. Wrapping around.");
      colourSets(setId % colourSets.size)
    };
    val colourSetCleaned = colourSet.child.filter {
      case <color/> => true
      case _        => false
    }
    val colourNode = if (colourSetCleaned.size > colourId) {
      colourSetCleaned(colourId)
    } else {
      println(s"Provided palette does not have a colour $colourId. Wrapping around.");
      colourSetCleaned(colourId % colourSetCleaned.size)
    }
    rgb(
      (colourNode \ "@id").text,
      (colourNode \ "@r").text.toInt,
      (colourNode \ "@g").text.toInt,
      (colourNode \ "@b").text.toInt)
  }

  // *** Primary color:

  val primaryShade0 = getColourModulo(0, 0);
  val primaryShade1 = getColourModulo(0, 1);
  val primaryShade2 = getColourModulo(0, 2);
  val primaryShade3 = getColourModulo(0, 3);
  val primaryShade4 = getColourModulo(0, 4);

  // *** Secondary color (1):

  val secondaryShade0 = getColourModulo(1, 0);
  val secondaryShade1 = getColourModulo(1, 1);
  val secondaryShade2 = getColourModulo(1, 2);
  val secondaryShade3 = getColourModulo(1, 3);
  val secondaryShade4 = getColourModulo(1, 4);

  // *** Secondary color (2):

  val tertiaryShade0 = getColourModulo(2, 0);
  val tertiaryShade1 = getColourModulo(2, 1);
  val tertiaryShade2 = getColourModulo(2, 2);
  val tertiaryShade3 = getColourModulo(2, 3);
  val tertiaryShade4 = getColourModulo(2, 4);

  // *** Complement color:

  val complementShade0 = getColourModulo(3, 0);
  val complementShade1 = getColourModulo(3, 1);
  val complementShade2 = getColourModulo(3, 2);
  val complementShade3 = getColourModulo(3, 3);
  val complementShade4 = getColourModulo(3, 4);
}
