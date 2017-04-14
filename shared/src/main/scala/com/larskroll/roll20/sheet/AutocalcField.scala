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

sealed trait AutocalcExpression[T] extends Renderable {

  import AutocalcExprs._
  //import Arith._

  def +(other: AutocalcExpression[T])(implicit n: Numeric[T]) = NumericExpr(this) + NumericExpr(other);
  def -(other: AutocalcExpression[T])(implicit n: Numeric[T]) = NumericExpr(this) - NumericExpr(other);
  def /(other: AutocalcExpression[T])(implicit n: Numeric[T]) = NumericExpr(this) / NumericExpr(other);
  def *(other: AutocalcExpression[T])(implicit n: Numeric[T]) = NumericExpr(this) * NumericExpr(other);
  def %(other: AutocalcExpression[T])(implicit n: Numeric[T]) = NumericExpr(this) % NumericExpr(other);

  def +(other: ArithmeticExpression[T])(implicit n: Numeric[T]) = NumericExpr(this) + other;
  def -(other: ArithmeticExpression[T])(implicit n: Numeric[T]) = NumericExpr(this) - other;
  def /(other: ArithmeticExpression[T])(implicit n: Numeric[T]) = NumericExpr(this) / other;
  def *(other: ArithmeticExpression[T])(implicit n: Numeric[T]) = NumericExpr(this) * other;
  def %(other: ArithmeticExpression[T])(implicit n: Numeric[T]) = NumericExpr(this) % other;
  // TODO simplify
  def as[C](): AutocalcExpression[C] = Cast[T, C](this);
}

sealed trait RoundingFunction {
  def name: String;
}
object RoundingFunction {
  case object Ceil extends RoundingFunction {
    override def name: String = "ceil";
  }
  case object Floor extends RoundingFunction {
    override def name: String = "floor";
  }
  case object Round extends RoundingFunction {
    override def name: String = "round";
  }
}

object AutocalcExprs {

  case class FieldAccess[T](field: FieldLike[T]) extends AutocalcExpression[T] {
    override def render: String = s"@{${field.qualifiedAttr}}"
    def selected = SelectedAttributeAccess(field);
    def target = TargetedAttributeAccess(field, None);
    def target(t: String) = TargetedAttributeAccess(field, Some(t));
  }

  case class TargetedAttributeAccess[T](field: FieldLike[T], target: Option[String]) extends AutocalcExpression[T] {
    override def render: String = target match {
      case Some(t) => s"@{target|${t}|${field.qualifiedAttr}}"
      case None    => s"@{target|${field.qualifiedAttr}}"
    }
  }

  case class SelectedAttributeAccess[T](field: FieldLike[T]) extends AutocalcExpression[T] {
    override def render: String = s"@{selected|${field.qualifiedAttr}}"
  }

  case class Literal[T](t: T) extends AutocalcExpression[T] {
    override def render: String = t.toString();
  }

  case class Cast[I, O](expr: AutocalcExpression[I]) extends AutocalcExpression[O] {
    override def render: String = expr.render;
  }

  case class Arithmetic[T](expr: ArithmeticExpression[T]) extends AutocalcExpression[T] {
    override def render: String = expr.render;
  }

  case class Macro[T](name: String) extends AutocalcExpression[T] {
    override def render: String = s"#{${name}}";
  }

  case class Ability[T](name: String) extends AutocalcExpression[T] {
    override def render: String = s"%{${name}}";
    def selected = SelectedAbilityAccess(name);
    def target = TargetedAbilityAccess(name, None);
    def target(t: String) = TargetedAbilityAccess(name, Some(t));
  }

  case class TargetedAbilityAccess[T](name: String, target: Option[String]) extends AutocalcExpression[T] {
    override def render: String = target match {
      case Some(t) => s"%{target|${t}|${name}}"
      case None    => s"%{target|${name}}"
    }
  }

  case class SelectedAbilityAccess[T](name: String) extends AutocalcExpression[T] {
    override def render: String = s"%{selected|${name}}"
  }

  case class NativeExpr[T](expr: String) extends AutocalcExpression[T] {
    override def render: String = expr;
  }

  def native[T](s: String) = NativeExpr[T](s);

  case class SeqExpr[T](exprs: Seq[AutocalcExpression[T]]) extends AutocalcExpression[T] {
    override def render: String = exprs.map(_.render).mkString;
  }

  case class NumericExpr[T](expr: AutocalcExpression[T]) extends ArithmeticExpression[T] {
    override def render: String = expr.render;
  }

  def exprs[T](expressions: AutocalcExpression[T]*) = SeqExpr(expressions);

  def ceil[T: Numeric](expr: AutocalcExpression[T]) = Arith.ceil(NumericExpr(expr));
  def floor[T: Numeric](expr: AutocalcExpression[T]) = Arith.floor(NumericExpr(expr));
  def round[T: Numeric](expr: AutocalcExpression[T]) = Arith.round(NumericExpr(expr));

  def abs[T: Numeric](expr: AutocalcExpression[T]) = Arith.abs(NumericExpr(expr));
}

class AutocalcField[T](val base: Field[T], val aexpr: AutocalcExpression[T]) extends FieldLike[T] {
  override def editable(): Boolean = false; // autocalc is never editable
  override def attr: String = base.attr;
  override def name: String = base.name;
  override def initialValue: String = {
    aexpr.render;
  }
  override def reader: Readable[T] = base.reader; // should be the same reader
  override def ctx = base.ctx;
}
