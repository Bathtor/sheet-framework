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

sealed trait RollExpression[T] extends Renderable {
  import RollExprs._

  def +(other: ArithmeticExpression[T])(implicit n: Numeric[T]) = Arith(this) + other;
  def -(other: ArithmeticExpression[T])(implicit n: Numeric[T]) = Arith(this) - other;
  def /(other: ArithmeticExpression[T])(implicit n: Numeric[T]) = Arith(this) / other;
  def *(other: ArithmeticExpression[T])(implicit n: Numeric[T]) = Arith(this) * other;
  def %(other: ArithmeticExpression[T])(implicit n: Numeric[T]) = Arith(this) % other;
  def as[O: Numeric](implicit n: Numeric[T]) = Arith(this).as[O];
  // special version for common case
  def +(other: FieldLike[T])(implicit n: Numeric[T]) = Arith(this) + other.arith();
  def -(other: FieldLike[T])(implicit n: Numeric[T]) = Arith(this) - other.arith();
  def /(other: FieldLike[T])(implicit n: Numeric[T]) = Arith(this) / other.arith();
  def *(other: FieldLike[T])(implicit n: Numeric[T]) = Arith(this) * other.arith();
  def %(other: FieldLike[T])(implicit n: Numeric[T]) = Arith(this) % other.arith();

  def &(option: RollOption) = WithOption(this, option)
}

sealed trait IntRollExpression extends RollExpression[Int] {
  import ComparePoints._
  import RollModifiers._
  import DiceExprs._

  def ++(mod: RollModifier): IntRollExpression;
  def <(target: Int): IntRollExpression = this ++ TargetRoll(LeqCP(target));
  def >(target: Int): IntRollExpression = this ++ TargetRoll(GeqCP(target));
  def `=`(target: Int): IntRollExpression = this ++ TargetRoll(EqualCP(target));
  def cs(): ModifierLackingCP[IntRollExpression] = ELCP(cp => this ++ CriticalSuccess(cp));
  def cf(): ModifierLackingCP[IntRollExpression] = ELCP(cp => this ++ CriticalFailure(cp));
  def f(): ModifierLackingCP[IntRollExpression] = ELCP(cp => this ++ FailuresRoll(cp));
  def !(): ModifierLackingCP[IntRollExpression] = ELCP(cp => this ++ ExplodingRoll(cp));
  def !!(): ModifierLackingCP[IntRollExpression] = ELCP(cp => this ++ CompoundingRoll(cp));
  def `!p`(): ModifierLackingCP[IntRollExpression] = ELCP(cp => this ++ PenetratingRoll(cp));
  def r(): ModifierLackingCP[IntRollExpression] = ELCP(cp => this ++ RerollRoll(cp));
  def ro(): ModifierLackingCP[IntRollExpression] = ELCP(cp => this ++ RerollOnceRoll(cp));
  def kh(target: Int): IntRollExpression = this ++ KeepHighestRoll(target);
  def kl(target: Int): IntRollExpression = this ++ KeepLowestRoll(target);
  def dh(target: Int): IntRollExpression = this ++ DropHighestRoll(target);
  def dl(target: Int): IntRollExpression = this ++ DropLowestRoll(target);
  def s(): IntRollExpression = this ++ SortAscRoll;
  def sa(): IntRollExpression = this ++ SortAscRoll;
  def sd(): IntRollExpression = this ++ SortDescRoll;
}

object RollExprs {

  case class Dice(dice: DiceExpression, label: Option[String]) extends IntRollExpression {
    override def render: String = label match {
      case Some(l) => s"${dice.render}[$l]"
      case None    => dice.render
    }
    override def ++(mod: RollModifier): IntRollExpression = WithMods(dice, mod, label);
    def label(l: String): Dice = Dice(dice, Some(l));
  }

  case class WithMods(dice: DiceExpression, mod: RollModifier, label: Option[String]) extends IntRollExpression {
    override def render: String = label match {
      case Some(l) => s"${dice.render}${mod.render}[$l]";
      case None    => s"${dice.render}${mod.render}";
    }
    override def ++(mod: RollModifier): IntRollExpression = WithMods(dice, this.mod ++ mod, label);
    def label(l: String): WithMods = WithMods(dice, mod, Some(l));
  }

  case class Group(exprs: Seq[IntRollExpression]) extends IntRollExpression {
    override def render: String = exprs.map(_.render).mkString("{", ",", "}");
    def ++(mod: RollModifier): IntRollExpression = GroupWithMods(exprs, mod);
  }

  case class GroupWithMods(exprs: Seq[IntRollExpression], mod: RollModifier) extends IntRollExpression {
    override def render: String = exprs.map(_.render).mkString("{", ",", "}") + mod.render;
    override def ++(mod: RollModifier): IntRollExpression = GroupWithMods(exprs, this.mod ++ mod);
  }

  case class Inline(roll: Rolls.InlineRoll[Int]) extends IntRollExpression {
    override def render: String = roll.render;
    override def ++(mod: RollModifier): IntRollExpression = GroupWithMods(Seq(this), mod);
  }

  case class Math[T: Numeric](expr: ArithmeticExpression[T]) extends RollExpression[T] {
    override def render: String = expr.render;
  }

  case class Arith[T: Numeric](expr: RollExpression[T]) extends ArithmeticExpression[T] {
    override def render: String = expr.render;
  }

  case class WithIntQuery(query: RollQuery[Int]) extends IntRollExpression {
    override def render: String = query.render;
    override def ++(mod: RollModifier): IntRollExpression = ???; // not allowed here
  }

  case class WithOption[T](expr: RollExpression[T], option: RollOption) extends RollExpression[T] {
    override def render: String = s"${expr.render} ${option.render}";
  }

}

sealed trait RollOption extends Renderable {
}

object RollOptions {
  case object NoError extends RollOption {
    override def render: String = "&{noerror}";
  }

  case object Tracker extends RollOption {
    override def render: String = "&{tracker}";
  }

  case object TrackerPlus extends RollOption {
    override def render: String = "&{tracker:+}";
  }

  case object TrackerMinus extends RollOption {
    override def render: String = "&{tracker:-}";
  }
}

sealed trait Roll extends Renderable {
}

object Rolls {

  case class SimpleRoll[T](expr: RollExpression[T]) extends Roll {
    override def render: String = s"/roll ${expr.render}";
  }

  case class InlineRoll[T](expr: RollExpression[T]) extends Roll {
    override def render: String = s"[[ ${expr.render} ]]";
  }

  case class TemplateRoll(chat: ChatCommand, template: TemplateApplication) extends Roll {
    override def render: String = s"${chat.render}${template.render}";
  }
}

trait TemplateApplication extends Renderable {

}

sealed trait ComparePoint extends Renderable {
}

object ComparePoints {

  case object DefaultCP extends ComparePoint {
    override def render: String = "";
  }
  case class EqualCP(target: Int) extends ComparePoint {
    override def render: String = s"$target"; // =-sign is optional
  }
  case class GeqCP(target: Int) extends ComparePoint {
    override def render: String = s">$target";
  }
  case class LeqCP(target: Int) extends ComparePoint {
    override def render: String = s"<$target";
  }
}

// TODO roll templates

sealed trait RollModifier extends Renderable {
  import ComparePoints._
  import RollModifiers._

  def ++(mod: RollModifier): RollModifier = ModifierSeq(Seq(this, mod));
  def <(target: Int): RollModifier = this ++ TargetRoll(LeqCP(target));
  def >(target: Int): RollModifier = this ++ TargetRoll(GeqCP(target));
  def `=`(target: Int): RollModifier = this ++ TargetRoll(EqualCP(target));
  def cs(): ModifierLackingCP[RollModifier] = MLCP(cp => this ++ CriticalSuccess(cp));
  def cf(): ModifierLackingCP[RollModifier] = MLCP(cp => this ++ CriticalFailure(cp));
  def f(): ModifierLackingCP[RollModifier] = MLCP(cp => this ++ FailuresRoll(cp));
  def !(): ModifierLackingCP[RollModifier] = MLCP(cp => this ++ ExplodingRoll(cp));
  def !!(): ModifierLackingCP[RollModifier] = MLCP(cp => this ++ CompoundingRoll(cp));
  def `!p`(): ModifierLackingCP[RollModifier] = MLCP(cp => this ++ PenetratingRoll(cp));
  def r(): ModifierLackingCP[RollModifier] = MLCP(cp => this ++ RerollRoll(cp));
  def ro(): ModifierLackingCP[RollModifier] = MLCP(cp => this ++ RerollOnceRoll(cp));
  def kh(target: Int): RollModifier = this ++ KeepHighestRoll(target);
  def kl(target: Int): RollModifier = this ++ KeepLowestRoll(target);
  def dh(target: Int): RollModifier = this ++ DropHighestRoll(target);
  def dl(target: Int): RollModifier = this ++ DropLowestRoll(target);
  def s(): RollModifier = this ++ SortAscRoll;
  def sa(): RollModifier = this ++ SortAscRoll;
  def sd(): RollModifier = this ++ SortDescRoll;
}

object RollModifiers {

  import ComparePoints._

  class ModifierLackingCP[Out](val completer: ComparePoint => Out) {

    def <(target: Int): Out = completer(LeqCP(target));
    def >(target: Int): Out = completer(GeqCP(target));
    def `=`(target: Int): Out = completer(EqualCP(target));
    def apply(target: Int): Out = completer(EqualCP(target));
    def apply(cp: ComparePoint): Out = completer(cp);
  }

  object MLCP {
    def apply(completer: ComparePoint => RollModifier): ModifierLackingCP[RollModifier] = new ModifierLackingCP[RollModifier](completer);
  }

  case class TargetRoll(cp: ComparePoint) extends RollModifier {
    override def render: String = s"${cp.render}";
  }

  case class CriticalSuccess(cp: ComparePoint) extends RollModifier {
    override def render: String = s"cs${cp.render}";
  }

  case class CriticalFailure(cp: ComparePoint) extends RollModifier {
    override def render: String = s"cf${cp.render}";
  }

  case class FailuresRoll(cp: ComparePoint) extends RollModifier {
    override def render: String = s"f${cp.render}";
  }

  case class ExplodingRoll(cp: ComparePoint) extends RollModifier {
    override def render: String = s"!${cp.render}";
  }

  case class CompoundingRoll(cp: ComparePoint) extends RollModifier {
    override def render: String = s"!!${cp.render}";
  }

  case class PenetratingRoll(cp: ComparePoint) extends RollModifier {
    override def render: String = s"!p${cp.render}";
  }

  case class KeepHighestRoll(n: Int) extends RollModifier {
    override def render: String = s"kh$n";
  }

  case class KeepLowestRoll(n: Int) extends RollModifier {
    override def render: String = s"kl$n";
  }

  case class DropHighestRoll(n: Int) extends RollModifier {
    override def render: String = s"dh$n";
  }

  case class DropLowestRoll(n: Int) extends RollModifier {
    override def render: String = s"dl$n";
  }

  case class RerollRoll(cp: ComparePoint) extends RollModifier {
    override def render: String = s"r${cp.render}";
  }

  case class RerollOnceRoll(cp: ComparePoint) extends RollModifier {
    override def render: String = s"ro${cp.render}";
  }

  case object SortAscRoll extends RollModifier {
    override def render: String = s"sa";
  }

  case object SortDescRoll extends RollModifier {
    override def render: String = s"sd";
  }

  case class ModifierSeq(mods: Seq[RollModifier]) extends RollModifier {
    override def render: String = s"${mods.map(_.render).mkString}";
    override def ++(mod: RollModifier): RollModifier = ModifierSeq(mods :+ mod);
  }

}

case class RollField[T: Numeric](ctx: RenderingContext, attr: String, formula: RollExpression[T]) extends FieldLike[RollExpression[T]] {
  override def editable(): Boolean = false; // for now...maybe change later
  override def name: String = s"attr_${attr}";
  override def initialValue: String = formula.render;
  override def reader: Readable[RollExpression[T]] = ???; // TODO this is tricky...

  def inline: Roll = Rolls.InlineRoll(formula);
  def roll: Roll = Rolls.SimpleRoll(formula);
}

case class Button(ctx: RenderingContext, attr: String, roll: Roll) extends FieldLike[Roll] {
  override def editable(): Boolean = false; // not really relevant
  override def name: String = s"roll_${qualifiedAttr}";
  override def initialValue: String = roll.render;
  override def reader: Readable[Roll] = ???; // TODO this is tricky...
}

case class CommandButton(label: String, button: Button) extends Renderable {
  override def render: String = s"[$label](~${button.accessor})";
}

case class APIButton(label: String, command: Renderable) extends Renderable {
  override def render: String = s"[$label](!${command.render})";
}

object APIButton {
  def from(label: String, formula: RollExpression[Int]): APIButton = {
    APIButton(label, RollAsAPI(formula))
  }
}

/**
 * Note that this doesn't really work at the moment, as the template doesn't unescape things again, but fails if they are not escaped -.-
 */
case class RollAsAPI(formula: RollExpression[Int]) extends Renderable {
  override def render: String = s"&#13;/roll ${escapedFormula}";

  lazy val escapedFormula: String = {
    val fS = formula.render;
    val sb = new StringBuilder();
    fS.foreach { c =>
      c match {
        case '%' => sb.append("&#37;")
        case ')' => sb.append("&#41;")
        case '?' => sb.append("&#63;")
        case '@' => sb.append("&#64;")
        case '[' => sb.append("&#91;")
        case ']' => sb.append("&#93;")
        case _   => sb.append(c)
      }
    }
    sb.toString;
  }
}
