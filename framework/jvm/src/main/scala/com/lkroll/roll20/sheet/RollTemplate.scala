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

trait RollTemplate extends Renderable {
  import RollTemplate._
  import TemplateFields._

  def name: String;
  def content: Tag;
  val rolltemplate = tag("rolltemplate");
  override def render: String = rolltemplate(`class` := s"sheet-rolltemplate-${name}", content).render;
  def apply(data: (TemplateField[Nothing], Renderable)*): TemplateApplication = MapTemplateApplication(this, data.toMap);

  def exists(f: TemplateField[Nothing])(body: => Modifier): Modifier = {
    val helper = f.name;
    val start = raw(s"{{#${helper}}}");
    val end = raw(s"{{/${helper}}}");
    Seq[Modifier](start, body, end)
  }

  def notExists(f: TemplateField[Nothing])(body: => Modifier): Modifier = {
    val helper = f.name;
    val start = raw(s"{{^${helper}}}");
    val end = raw(s"{{/${helper}}}");
    Seq[Modifier](start, body, end)
  }

  def switchExists(f: TemplateField[Nothing], ifexists: => Modifier, ifnotexists: => Modifier): Modifier = {
    val helper = f.name;
    val startIf = raw(s"{{#${helper}}}");
    val startIfNot = raw(s"{{^${helper}}}");
    val end = raw(s"{{/${helper}}}");
    Seq[Modifier](startIf, ifexists, end, startIfNot, ifnotexists, end)
  }

  def not(f: HelperFunc)(body: => Modifier): Modifier = {
    val helper = f.render;
    val start = raw(s"{{#^${helper}}}");
    val end = raw(s"{{/^${helper}}}");
    Seq[Modifier](start, body, end)
  }

  def allPropsS(except: String*): AllProps = AllProps(except);

  def allProps(except: TemplateField[Any]*): AllProps = AllProps(except.map(_.name));

  def rollWasCrit[T](f: TemplateField[RollField[T]]) = RollWasCrit(f);

  def rollWasFumble[T](f: TemplateField[RollField[T]]) = RollWasFumble(f);

  def rollTotal[T](f: TemplateField[RollField[T]], cp: T) = RollTotal(f, Left(cp));
  def rollTotal[T](f: TemplateField[RollField[T]], cp: TemplateField[RollField[T]]) = RollTotal(f, Right(cp));

  def rollGreater[T](f: TemplateField[RollField[T]], cp: T) = RollGreater(f, Left(cp));
  def rollGreater[T](f: TemplateField[RollField[T]], cp: TemplateField[RollField[T]]) = RollGreater(f, Right(cp));

  def rollLess[T](f: TemplateField[RollField[T]], cp: T) = RollLess(f, Left(cp));
  def rollLess[T](f: TemplateField[RollField[T]], cp: TemplateField[RollField[T]]) = RollLess(f, Right(cp));

  def rollBetween[T](f: TemplateField[RollField[T]], lcp: T, hcp: T) = RollBetween(f, Left(lcp), Left(hcp));
  def rollBetween[T](f: TemplateField[RollField[T]], lcp: T, hcp: TemplateField[RollField[T]]) = RollBetween(f, Left(lcp), Right(hcp));
  def rollBetween[T](f: TemplateField[RollField[T]], lcp: TemplateField[RollField[T]], hcp: T) = RollBetween(f, Right(lcp), Left(hcp));
  def rollBetween[T](f: TemplateField[RollField[T]], lcp: TemplateField[RollField[T]], hcp: TemplateField[RollField[T]]) = RollBetween(f, Right(lcp), Right(hcp));

  def any(s: String) = AnyField(s);

  def value[T](s: String) = DirectField[T](s);

  def rollable[T: Numeric](s: String) = RollableField[T](s);

  def attribute[T](s: String) = AttributeField[T](s);

  def labeli18n(s: String) = LabelField(s);

  def button(s: String) = ButtonField(s);
}

object RollTemplate {
  import TemplateFields._

  case class AllProps(exceptions: Seq[String]) {
    def apply(mapper: (TemplateField[Any], TemplateField[Any]) => Modifier): Modifier = {
      val helper = s"allProps() ${exceptions.mkString(" ")}";
      val start = raw(s"{{#${helper}}}");
      val end = raw(s"{{/${helper}}}");
      Seq[Modifier](start, mapper(KeyField, ValueField), end)
    }
  }

  sealed trait HelperFunc extends Renderable {
    def apply(body: => Modifier): Modifier = {
      val helper = this.render;
      val start = raw(s"{{#${helper}}}");
      val end = raw(s"{{/${helper}}}");
      Seq[Modifier](start, body, end)
    }
  }

  case class RollWasCrit[T](f: TemplateField[RollField[T]]) extends HelperFunc {
    override def render: String = s"rollWasCrit() ${f.name}";
  }

  case class RollWasFumble[T](f: TemplateField[RollField[T]]) extends HelperFunc {
    override def render: String = s"rollWasFumble() ${f.name}";
  }

  case class RollTotal[T](f: TemplateField[RollField[T]], cpE: Either[T, TemplateField[RollField[T]]]) extends HelperFunc {
    val name = "rollTotal()";
    override def render: String = cpE match {
      case Left(cp)   => s"$name ${f.name} ${cp}";
      case Right(cpf) => s"$name ${f.name} ${cpf.name}";
    }
  }

  case class RollGreater[T](f: TemplateField[RollField[T]], cpE: Either[T, TemplateField[RollField[T]]]) extends HelperFunc {
    val name = "rollGreater()";
    override def render: String = cpE match {
      case Left(cp)   => s"$name ${f.name} ${cp}";
      case Right(cpf) => s"$name ${f.name} ${cpf.name}";
    }
  }

  case class RollLess[T](f: TemplateField[RollField[T]], cpE: Either[T, TemplateField[RollField[T]]]) extends HelperFunc {
    val name = "rollLess()";
    override def render: String = cpE match {
      case Left(cp)   => s"$name ${f.name} ${cp}";
      case Right(cpf) => s"$name ${f.name} ${cpf.name}";
    }
  }

  case class RollBetween[T](f: TemplateField[RollField[T]], lcpE: Either[T, TemplateField[RollField[T]]], hcpE: Either[T, TemplateField[RollField[T]]]) extends HelperFunc {
    val name = "rollBetween()";
    override def render: String = {
      val lcp: String = lcpE match {
        case Left(cp)   => cp.toString()
        case Right(cpf) => cpf.name
      }
      val hcp: String = hcpE match {
        case Left(cp)   => cp.toString()
        case Right(cpf) => cpf.name
      }
      s"$name ${f.name} $lcp $hcp"
    }
  }
}

trait TemplateField[-T] extends Renderable {
  def name: String;
  def render: String = s"{{${name}}}";
  def frag = raw(this.render);

  def <<=[V <: T](v: V)(implicit f: V => Renderable): (TemplateField[T], Renderable) = (this -> v);
}

object TemplateFields {
  case class AnyField(name: String) extends TemplateField[Any] {
  }

  case class DirectField[T](name: String) extends TemplateField[T] {
  }

  case class RollableField[T](name: String) extends TemplateField[RollField[T]] {
    //override def <<=[V <: RollField[T]](v: V)(implicit f: V => Renderable): (TemplateField[RollField[T]], Renderable) = (this -> v.inline);
  }

  case class AttributeField[T](name: String) extends TemplateField[Field[T]] {
  }

  case object KeyField extends TemplateField[Any] {
    override def name: String = "key";
  }

  case object ValueField extends TemplateField[Any] {
    override def name: String = "value";
  }

  case class LabelField(name: String) extends TemplateField[LabelI18N] {

  }

  case class ButtonField(name: String) extends TemplateField[CommandButton] {

  }
}

case class MapTemplateApplication(template: RollTemplate, data: Map[TemplateField[Nothing], Renderable]) extends TemplateApplication {
  override def render: String = data.map({
    case (k, v) => s"{{${k.name}=${v.render}}}"
  }).mkString(s"&{template:${template.name}} ", " ", "");
}

