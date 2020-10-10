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

trait RepeatingSection extends Fields {
  def name: String;

  lazy val cls: String = {
    assert(!name.contains("_"), "Repeating section names may not contain underscores in Roll20!");
    s"${RepeatingSection.prefix}${name}";
  }
  override def qualifier: Option[String] = Some(name);
  override def mapAccess(rowId: String, s: String): String = s"${RepeatingSection.prefix}${name}_${rowId}_$s";
  override def mapAccess(s: String): String = s"${RepeatingSection.prefix}${name}_$s";
  override def mapSelect(s: String): String = s"${RepeatingSection.prefix}${name}:$s";
  override def mapMatcher(s: String): String => Boolean = {
    val pattern = s"""${RepeatingSection.prefix}${name}_[-a-zA-Z0-9]+_$s""".r;
    (s: String) =>
      s match {
        case pattern() => true
        case _         => false
      }
  }
  override def mapMatcher(rowId: String, s: String): String => Boolean = {
    val pattern = s"""${RepeatingSection.prefix}${name}_${rowId}_$s""";
    (s: String) => s.equalsIgnoreCase(pattern)
  }

  def at[T](rowId: String, f: FieldLike[T]): FieldAtRow[T] = FieldAtRow(this, rowId, f);

  val reporder = ReporderField(this);
  lazy val selector = cls;

  override def toString: String = s"repeating_${name}";
}

object RepeatingSection {
  val prefix = "repeating_";
}

case class ReporderField(section: RepeatingSection) extends FieldLike[Array[String]] with RenderingContext {

  val _accessor = s"_reporder_${RepeatingSection.prefix}${section.name}";

  override def qualifier: Option[String] = section.qualifier;
  override def mapAccess(rowId: String, s: String): String = _accessor;
  override def mapAccess(s: String): String = _accessor;
  override def mapSelect(s: String): String = _accessor; // actually the same
  override def mapMatcher(s: String): String => Boolean = ???; // not sure
  override def mapMatcher(rowId: String, s: String): String => Boolean = ???; // not sure

  def editable(): Boolean = false;
  def ctx: RenderingContext = this;
  def attr: String = _accessor;
  def initialValue: String = "[]";

  def reader: Readable[Array[String]] = CoreImplicits.readableArrayString;

  /*
   * Compare fields at row by accessor
   */
  override def canEqual(that: Any) = that.isInstanceOf[ReporderField];
  override def hashCode(): Int = _accessor.hashCode();
  override def equals(that: Any): Boolean =
    canEqual(that) && (that.asInstanceOf[ReporderField]._accessor == this._accessor);

}

case class FieldAtRow[T](section: RepeatingSection, rowId: String, field: FieldLike[T])
    extends FieldLike[T]
    with RenderingContext {

  override def qualifier: Option[String] = section.qualifier;
  override def mapAccess(rowId: String, s: String): String = section.mapAccess(rowId, s);
  override def mapAccess(s: String): String = section.mapAccess(rowId, s);
  override def mapSelect(s: String): String = section.mapSelect(s);
  override def mapMatcher(s: String): String => Boolean = ???; // not sure
  override def mapMatcher(rowId: String, s: String): String => Boolean = ???; // not sure

  def editable(): Boolean = field.editable();
  def ctx: RenderingContext = this;
  def attr: String = field.attr;
  def initialValue: String = field.initialValue;

  def reader: Readable[T] = field.reader;

  /*
   * Compare fields at row by accessor
   */
  override def canEqual(that: Any) = that.isInstanceOf[FieldAtRow[_]];
  override def hashCode(): Int = accessor.hashCode();
  override def equals(that: Any): Boolean =
    canEqual(that) && (that.asInstanceOf[FieldAtRow[_]].accessor == this.accessor);
}
