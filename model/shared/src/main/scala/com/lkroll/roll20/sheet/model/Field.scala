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
package com.lkroll.roll20.sheet.model

import com.lkroll.roll20.core._

sealed trait Field[T] extends FieldLike[T] {
  type F <: Field[T]

  def editable(b: Boolean): F;
  def default(d: T): F;
  def defaultValue: Option[T];
  override def initialValue: String = {
    defaultValue match {
      case Some(t) => t.toString();
      case None    => ""
    }
  }
  def fieldDefault: T;
  def resetValue: T = {
    defaultValue match {
      case Some(t) => t
      case None    => fieldDefault;
    }
  }
  def autocalc(expr: AutocalcExpression[T]): AutocalcField[T] = {
    return new AutocalcField(this, expr);
  }
}

case class FlagField(ctx: RenderingContext,
                     attr: String,
                     defaultValue: Option[Boolean] = None,
                     editable: Boolean = true
) extends Field[Boolean] {
  type F = FlagField

  override def reader = CoreImplicits.readableBoolean;
  override def fieldDefault: Boolean = false;

  override def default(b: Boolean): FlagField = FlagField(ctx, attr, Some(b), editable);
  override def editable(b: Boolean): FlagField = FlagField(ctx, attr, defaultValue, b);
}

case class TextField(ctx: RenderingContext, attr: String, defaultValue: Option[String] = None, editable: Boolean = true)
    extends Field[String] {
  type F = TextField

  override def reader = CoreImplicits.readableString;
  override def fieldDefault: String = "";

  override def default(s: String): TextField = TextField(ctx, attr, Some(s), editable);
  override def editable(b: Boolean): TextField = TextField(ctx, attr, defaultValue, b);
}

case class EnumField(ctx: RenderingContext,
                     attr: String,
                     defaultValue: Option[String],
                     options: Set[String],
                     enumeration: Option[Enumeration],
                     editable: Boolean = false
) extends Field[String] {
  type F = EnumField

  override def reader = CoreImplicits.readableString; // maybe check that value is actually a member of options?
  override def fieldDefault: String =
    if (options.isEmpty) {
      ""
    } else {
      options.head
    };

  override def default(s: String): EnumField = EnumField(ctx, attr, Some(s), options, enumeration, editable);
  def default(s: Any): EnumField =
    EnumField(ctx,
              attr,
              Some(s.toString()),
              options,
              enumeration,
              editable
    ); // this is a bit awkward but knowing the type of an Enumeration is a bit tricky
  override def editable(b: Boolean): EnumField = EnumField(ctx, attr, defaultValue, options, enumeration, b);
}

case class VoidField(ctx: RenderingContext, attr: String, defaultValue: Option[Void] = None, editable: Boolean = true)
    extends Field[Void] {

  type F = VoidField

  override def reader = CoreImplicits.readableNull;
  override def fieldDefault: Void = null;

  override def default(v: Void): VoidField = VoidField(ctx, attr, defaultValue, editable); // only because it's required
  def default(b: Boolean): FlagField = FlagField(ctx, attr, Some(b), editable);
  def default(s: String): TextField = new TextField(ctx, attr, Some(s), editable);
  def default[N](num: N)(implicit n: Numeric[N], r: Readable[N]): NumberField[N] =
    NumberField(ctx, attr, r, Some(num), editable);
  def default(cc: ChatCommand): ChatField = ChatField(ctx, attr, Some(cc), editable);
  def options(opts: String*): EnumField = EnumField(ctx, attr, None, opts.toSet, None, editable);
  def options(e: Enumeration): EnumField =
    EnumField(ctx, attr, None, e.values.map(_.toString).toSet, Some(e), editable);
  def ref[T]: FieldRef[T] = FieldRef[T](ctx, attr);
  def ref[T](fref: FieldLike[T]): FieldRefRepeating[T] = FieldRefRepeating(ctx, attr, fref);
  def expression[T]: ExpressionField[T] = ExpressionField(ctx, attr);
  override def editable(b: Boolean): VoidField = VoidField(ctx, attr, defaultValue, b);
  def autocalc(expr: AutocalcExpression[String])(implicit dummy: DummyImplicit): AutocalcField[String] = {
    return new AutocalcField(TextField(ctx, this.attr), expr);
  }
  def autocalc[T](expr: AutocalcExpression[T])(implicit n: Numeric[T], r: Readable[T]): AutocalcField[T] = {
    return new AutocalcField(NumberField[T](ctx, this.attr, r), expr);
  }
  def button[T](roll: RollExpression[T]) = Button(ctx, attr, Rolls.SimpleRoll(roll));
  def roll[T: Numeric](roll: RollExpression[T]) = RollField(ctx, attr, roll);
}

case class NumberValidity[N](min: N, max: N, step: N)

case class NumberField[N: Numeric](ctx: RenderingContext,
                                   attr: String,
                                   reader: Readable[N],
                                   defaultValue: Option[N] = None,
                                   editable: Boolean = true,
                                   valid: Option[NumberValidity[N]] = None
) extends Field[N] {

  type F = NumberField[N]

  val numericEvidence = implicitly[Numeric[N]];

  override def fieldDefault: N = numericEvidence.zero;

  override def default(n: N): NumberField[N] = NumberField(ctx, attr, reader, Some(n), editable, valid);
  override def editable(b: Boolean): NumberField[N] = NumberField(ctx, attr, reader, defaultValue, b, valid);
  def validIn(min: N, max: N, step: N) =
    NumberField(ctx, attr, reader, defaultValue, editable, Some(NumberValidity(min, max, step)));
}

case class FieldRefRepeating[T](ctx: RenderingContext,
                                attr: String,
                                ref: FieldLike[T],
                                defaultValue: Option[String] = None,
                                editable: Boolean = true
) extends Field[String] {

  type F = FieldRefRepeating[T]

  override def fieldDefault: String = "none";
  override def reader = CoreImplicits.readableString;

  override def default(s: String): FieldRefRepeating[T] = FieldRefRepeating(ctx, attr, ref, Some(s), editable);
  override def editable(b: Boolean): FieldRefRepeating[T] = FieldRefRepeating(ctx, attr, ref, defaultValue, b);

  def valueAt(id: String): String = s"@{${ref.accessor(id)}}";
  def altExpr(implicit labelFields: LabelFields): AutocalcExpression[T] =
    CoreImplicits.fieldToAutoMaybeLabel(this).as[T];
  def altArith()(implicit n: Numeric[T], labelFields: LabelFields): ArithmeticExpression[T] =
    CoreImplicits.autoToArith(this.altExpr);
}

case class FieldRef[T](ctx: RenderingContext,
                       attr: String,
                       defaultValue: Option[String] = None,
                       editable: Boolean = true
) extends Field[String] {

  type F = FieldRef[T]

  override def fieldDefault: String = "none";
  override def reader = CoreImplicits.readableString;

  override def default(s: String): FieldRef[T] = FieldRef[T](ctx, attr, Some(s), editable);
  def default(s: FieldLike[T]): FieldRef[T] = FieldRef[T](ctx, attr, Some(valueFrom(s)), editable);
  override def editable(b: Boolean): FieldRef[T] = FieldRef[T](ctx, attr, defaultValue, b);

  def valueFrom(ref: FieldLike[T]): String = s"@{${ref.accessor}}";
  def valueFrom(ref: FieldLike[T], id: String): String = s"@{${ref.accessor(id)}}";
  def altExpr(implicit labelFields: LabelFields): AutocalcExpression[T] =
    CoreImplicits.fieldToAutoMaybeLabel(this).as[T];
  def altArith()(implicit n: Numeric[T], labelFields: LabelFields): ArithmeticExpression[T] =
    CoreImplicits.autoToArith(this.altExpr);
}

case class ExpressionField[T](ctx: RenderingContext,
                              attr: String,
                              defaultValue: Option[String] = None,
                              editable: Boolean = true
) extends Field[String] {

  type F = ExpressionField[T]

  override def fieldDefault: String = "none";
  override def reader = CoreImplicits.readableString;

  override def default(s: String): ExpressionField[T] = ExpressionField[T](ctx, attr, Some(s), editable);
  def default(expr: RollExpression[T]): ExpressionField[T] = ExpressionField[T](ctx, attr, Some(expr.render), editable);
  def default(expr: ArithmeticExpression[T]): ExpressionField[T] =
    ExpressionField[T](ctx, attr, Some(expr.render), editable);
  def default(expr: DiceExpression): ExpressionField[Int] =
    ExpressionField[Int](ctx, attr, Some(expr.render), editable);
  override def editable(b: Boolean): ExpressionField[T] = ExpressionField[T](ctx, attr, defaultValue, b);

  def valueFrom(r: Renderable): String = r.render;
  def altExpr(implicit labelFields: LabelFields): AutocalcExpression[T] =
    CoreImplicits.fieldToAutoMaybeLabel(this).as[T];
  def altArith()(implicit n: Numeric[T], labelFields: LabelFields): ArithmeticExpression[T] =
    CoreImplicits.autoToArith(this.altExpr);
}

case class ChatField(ctx: RenderingContext,
                     attr: String,
                     defaultValue: Option[ChatCommand] = None,
                     editable: Boolean = true
) extends Field[ChatCommand] {

  type F = ChatField

  override def fieldDefault: ChatCommand = Chat.Default;
  override def initialValue: String = {
    defaultValue match {
      case Some(t) => t.render;
      case None    => ""
    }
  }
  override def reader = CoreImplicits.readableChat;

  override def default(cc: ChatCommand): ChatField = ChatField(ctx, attr, Some(cc), editable);
  override def editable(b: Boolean): ChatField = ChatField(ctx, attr, defaultValue, b);

}

trait Fields extends RenderingContext {

  val renderingContext: RenderingContext = this;

  def flag(attr: String) = FlagField(this, attr);
  def text(attr: String) = TextField(this, attr);
  def number[T](attr: String)(implicit n: Numeric[T], r: Readable[T]) = NumberField[T](this, attr, r);
  def fieldRef[T](attr: String): FieldRef[T] = FieldRef(this, attr);
  def fieldRef[T](attr: String, ref: FieldLike[T]): FieldRefRepeating[T] = FieldRefRepeating(this, attr, ref);
  def field(attr: String) = VoidField(this, attr);
  def exprField[T](attr: String) = ExpressionField[T](this, attr);
  def button[T](attr: String, roll: RollExpression[T]) = Button(this, attr, Rolls.SimpleRoll(roll));
  def roll[T: Numeric](attr: String, roll: RollExpression[T]) = RollField(this, attr, roll);
  def chat(attr: String) = ChatField(this, attr);
}
