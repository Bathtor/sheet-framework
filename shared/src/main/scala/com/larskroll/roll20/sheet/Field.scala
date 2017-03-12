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

trait FieldLike[T] {
  def editable(): Boolean;
  def ctx: RenderingContext;
  def attr: String;
  def name: String = s"attr_${attr}";
  def accessor: String = ctx.mapAccess(attr);
  def accessor(rowId: String): String = ctx.mapAccess(rowId, attr);
  def selector: String = ctx.mapSelect(attr);
  def initialValue: String;

  def reader: Readable[T];
  def read(s: String): Option[T] = reader.read(s);

  def expr: AutocalcExpression[T] = FieldImplicits.fieldToAuto(this);
  def arith()(implicit n: Numeric[T]): ArithmeticExpression[T] = FieldImplicits.autoToArith(this.expr);

  /*
   * Compare fields by attr
   */
  def canEqual(that: Any) = that.isInstanceOf[FieldLike[_]];
  override def hashCode(): Int = attr.hashCode();
  override def equals(that: Any): Boolean = canEqual(that) && (that.asInstanceOf[FieldLike[_]].attr == this.attr);
}

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

case class FlagField(ctx: RenderingContext, attr: String, defaultValue: Option[Boolean] = None, editable: Boolean = true) extends Field[Boolean] {
  type F = FlagField

  override def reader = FieldImplicits.readableBoolean;
  override def fieldDefault: Boolean = false;

  override def default(b: Boolean): FlagField = FlagField(ctx, attr, Some(b), editable);
  override def editable(b: Boolean): FlagField = FlagField(ctx, attr, defaultValue, b);
}

case class TextField(ctx: RenderingContext, attr: String, defaultValue: Option[String] = None, editable: Boolean = true) extends Field[String] {
  type F = TextField

  override def reader = FieldImplicits.readableString;
  override def fieldDefault: String = "";

  override def default(s: String): TextField = TextField(ctx, attr, Some(s), editable);
  override def editable(b: Boolean): TextField = TextField(ctx, attr, defaultValue, b);
}

case class EnumField(ctx: RenderingContext, attr: String, defaultValue: Option[String], options: Set[String], editable: Boolean = false) extends Field[String] {
  type F = EnumField

  override def reader = FieldImplicits.readableString; // maybe check that value is actually a member of options?
  override def fieldDefault: String = if (options.isEmpty) { "" } else { options.head };

  override def default(s: String): EnumField = EnumField(ctx, attr, Some(s), options, editable);
  def default(s: Any): EnumField = EnumField(ctx, attr, Some(s.toString()), options, editable); // this is a bit awkward but knowing the type of an Enumeration is a bit tricky
  override def editable(b: Boolean): EnumField = EnumField(ctx, attr, defaultValue, options, b);
}

case class VoidField(ctx: RenderingContext, attr: String, defaultValue: Option[Void] = None, editable: Boolean = true) extends Field[Void] {

  type F = VoidField

  override def reader = FieldImplicits.readableNull;
  override def fieldDefault: Void = null;

  override def default(v: Void): VoidField = VoidField(ctx, attr, defaultValue, editable); // only because it's required
  def default(b: Boolean): FlagField = FlagField(ctx, attr, Some(b), editable);
  def default(s: String): TextField = new TextField(ctx, attr, Some(s), editable);
  def default[N](num: N)(implicit n: Numeric[N], r: Readable[N]): NumberField[N] = NumberField(ctx, attr, r, Some(num), editable);
  def options(opts: String*): EnumField = EnumField(ctx, attr, None, opts.toSet, editable);
  def options(e: Enumeration): EnumField = EnumField(ctx, attr, None, e.values.map(_.toString).toSet, editable);
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

case class NumberField[N: Numeric](ctx: RenderingContext, attr: String, reader: Readable[N], defaultValue: Option[N] = None, editable: Boolean = true) extends Field[N] {

  type F = NumberField[N]

  override def fieldDefault: N = implicitly[Numeric[N]].zero;

  override def default(n: N): NumberField[N] = NumberField(ctx, attr, reader, Some(n), editable);
  override def editable(b: Boolean): NumberField[N] = NumberField(ctx, attr, reader, defaultValue, b);
}

trait Fields extends RenderingContext {

  val renderingContext: RenderingContext = this;

  def flag(attr: String) = FlagField(this, attr);
  def text(attr: String) = TextField(this, attr);
  def number[T](attr: String)(implicit n: Numeric[T], r: Readable[T]) = NumberField[T](this, attr, r);
  def field(attr: String) = VoidField(this, attr);
  def button[T](attr: String, roll: RollExpression[T]) = Button(this, attr, Rolls.SimpleRoll(roll));
  def roll[T: Numeric](attr: String, roll: RollExpression[T]) = RollField(this, attr, roll);
}
