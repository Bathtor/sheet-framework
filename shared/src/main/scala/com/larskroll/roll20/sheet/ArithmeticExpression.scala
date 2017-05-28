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

trait ArithmeticExpression[T] extends Renderable {
  import Arith._

  def +(other: ArithmeticExpression[T])(implicit n: Numeric[T]) = PlusExpr[T](this, other);
  def -(other: ArithmeticExpression[T])(implicit n: Numeric[T]) = MinusExpr[T](this, other);
  def /(other: ArithmeticExpression[T])(implicit n: Numeric[T]) = DivExpr[T](this, other);
  def *(other: ArithmeticExpression[T])(implicit n: Numeric[T]) = MultExpr[T](this, other);
  def %(other: ArithmeticExpression[T])(implicit n: Numeric[T]) = ModExpr[T](this, other);

  def paren(implicit n: Numeric[T]) = Parenthesised(this);
  def as[N: Numeric](): ArithmeticExpression[N] = CastExpr[T, N](this);
}

object Arith {

  case class Literal[T: Numeric](t: T) extends ArithmeticExpression[T] {
    override def render: String = t.toString();
  }

  case class Parenthesised[T: Numeric](expr: ArithmeticExpression[T]) extends ArithmeticExpression[T] {
    def render: String = s"(${expr.render})";
  }

  case class PlusExpr[T: Numeric](left: ArithmeticExpression[T], right: ArithmeticExpression[T]) extends ArithmeticExpression[T] {
    def render: String = s"${left.render}+${right.render}";
  }

  case class MinusExpr[T: Numeric](left: ArithmeticExpression[T], right: ArithmeticExpression[T]) extends ArithmeticExpression[T] {
    def render: String = s"${left.render}-${right.render}";
  }

  case class DivExpr[T: Numeric](left: ArithmeticExpression[T], right: ArithmeticExpression[T]) extends ArithmeticExpression[T] {
    def render: String = s"${left.render}/${right.render}";
  }

  case class MultExpr[T: Numeric](left: ArithmeticExpression[T], right: ArithmeticExpression[T]) extends ArithmeticExpression[T] {
    def render: String = s"${left.render}*${right.render}";
  }

  case class ModExpr[T: Numeric](left: ArithmeticExpression[T], right: ArithmeticExpression[T]) extends ArithmeticExpression[T] {
    def render: String = s"${left.render}%${right.render}";
  }

  case class RoundingExpr[T: Numeric](function: RoundingFunction, expr: ArithmeticExpression[T]) extends ArithmeticExpression[Int] {
    def render: String = s"${function.name}(${expr.render})";
  }

  def ceil[T: Numeric](expr: ArithmeticExpression[T]) = RoundingExpr[T](RoundingFunction.Ceil, expr);
  def floor[T: Numeric](expr: ArithmeticExpression[T]) = RoundingExpr[T](RoundingFunction.Floor, expr);
  def round[T: Numeric](expr: ArithmeticExpression[T]) = RoundingExpr[T](RoundingFunction.Round, expr);

  case class AbsExpr[T: Numeric](expr: ArithmeticExpression[T]) extends ArithmeticExpression[T] {
    def render: String = s"abs(${expr.render})";
  }

  def abs[T: Numeric](expr: ArithmeticExpression[T]) = AbsExpr(expr);

  case class CastExpr[I, O](expr: ArithmeticExpression[I]) extends ArithmeticExpression[O] {
    def render: String = expr.render;
  }
}
