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

object FieldImplicits {

  import Readable._

  implicit def stringToExpr(s: String): AutocalcExpression[String] = AutocalcExprs.Literal(s);
  implicit def numToExpr[T: Numeric](num: T): ArithmeticExpression[T] = Arith.Literal(num);
  implicit def fieldToAuto[T](f: FieldLike[T]): AutocalcExpression[T] = AutocalcExprs.FieldAccess(f);
  implicit def seqToExpr[T](s: Seq[AutocalcExpression[T]]): AutocalcExpression[T] = AutocalcExprs.SeqExpr(s);
  implicit def stringToField(s: String)(implicit ctx: RenderingContext): VoidField = VoidField(ctx, s);
  implicit def autocalcToDiceParam(x: AutocalcExpression[Int]): DiceParameter = DiceParams.AutocalcParameter(x);
  implicit def intToDiceParam(n: Int): DiceParameter = DiceParams.ArithmeticParameter(n);
  implicit class DiceNumInt(n: Int) {
    def d(x: Int): DiceExpression = DiceExprs.BasicRoll(n, x);
    def d(x: DiceParameter): DiceExpression = DiceExprs.BasicRoll(n, x);
    def dF(): DiceExpression = DiceExprs.FateRoll(n);
  }
  implicit class DiceNumCalc(n: DiceParameter) {
    def d(x: Int): DiceExpression = DiceExprs.BasicRoll(n, x);
    def d(x: AutocalcExpression[Int]): DiceExpression = DiceExprs.BasicRoll(n, x);
    def dF(): DiceExpression = DiceExprs.FateRoll(n);
  }
  implicit def modLackCPToDefault[Out](mlcp: RollModifiers.ModifierLackingCP[Out]): Out = mlcp(ComparePoints.DefaultCP);
  implicit def diceToRoll(dice: DiceExpression): RollExprs.Dice = RollExprs.Dice(dice, None);
  implicit def arithToAuto[T: Numeric](expr: ArithmeticExpression[T]): AutocalcExpression[T] = AutocalcExprs.Arithmetic(expr);
  implicit def autoToArith[T: Numeric](expr: AutocalcExpression[T]): ArithmeticExpression[T] = AutocalcExprs.NumericExpr(expr);
  //implicit def fieldToArith[T: Numeric](f: FieldLike[T]): ArithmeticExpression[T] = fieldToExpr(f);
  //implicit def rollToArith(roll: RollExpression): RollExprs.Arith = RollExprs.Arith(roll);
  implicit def arithToRoll[T: Numeric](expr: ArithmeticExpression[T]): RollExpression[T] = RollExprs.Math(expr);

  def ceil[T: Numeric](expr: ArithmeticExpression[T]) = Arith.ceil(expr);
  def floor[T: Numeric](expr: ArithmeticExpression[T]) = Arith.floor(expr);
  def round[T: Numeric](expr: ArithmeticExpression[T]) = Arith.round(expr);
  def abs[T: Numeric](expr: ArithmeticExpression[T]) = Arith.abs(expr);
  def `macro`[T](s: String) = AutocalcExprs.Macro[T](s);
  def ability[T](s: String) = AutocalcExprs.Ability[T](s);

  implicit val readableNull = toReadable[Void](_ => null);
  implicit val readableDouble = toReadable[Double](_.toDouble);
  implicit val readableInt = toReadable[Int](_.toInt);
  implicit val readableLong = toReadable[Long](_.toLong);
  implicit val readableString = toReadable[String](identity);
  implicit val readableBoolean = toReadable[Boolean](_ match {
    case "0"     => false
    case "1"     => true
    case "off"   => false
    case "on"    => true
    case "false" => false
    case "true"  => true
    case x       => throw new IllegalArgumentException(x);
  });
  implicit val readableChat = toReadable[ChatCommand](Chat.fromString(_));
  implicit val readableArrayString = toReadable[Array[String]](_ match {
    case "[]" => Array[String]()
    case x    => ??? //TODO implement me (simply convert from json array)
  });

}
