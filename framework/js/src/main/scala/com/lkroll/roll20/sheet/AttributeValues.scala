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

import scalajs.js;
import com.lkroll.roll20.core._
import com.lkroll.roll20.sheet.model._

sealed trait AttributeValues {
  def apply(attr: String): Any;
  def apply[T](field: FieldLike[T]): Option[T];
}

case class DataAttributeValues(values: Map[String, Any]) extends AttributeValues {

  override def apply(attr: String): Any = values(attr);

  override def apply[T](field: FieldLike[T]): Option[T] = {
    if (values.contains(field.accessor)) {
      values(field.accessor) match {
        case v if field.reader.isInstanceOf(v) => Some(v.asInstanceOf[T])
        case s: String                         => field.read(s);
        case a                                 => field.read(a.toString);
      }
    } else {
      field match {
        case f: Field[T] => f.defaultValue
        case _           => None
      }
    }
  }

  override def toString: String = {
    s"DataAttributeValues(${values.mkString(",")})"
  }

}

case class RowAttributeValues(rowId: String, original: DataAttributeValues) extends AttributeValues {
  override def apply(attr: String): Any = original(attr); // maybe implement this differently?
  override def apply[T](field: FieldLike[T]): Option[T] = {
    if (original.values.contains(field.accessor(rowId))) {
      original.values(field.accessor(rowId)) match {
        case v if field.reader.isInstanceOf(v) => Some(v.asInstanceOf[T])
        case s: String                         => field.read(s);
        case a                                 => field.read(a.toString);
      }
    } else {
      field match {
        case f: Field[T] => f.defaultValue
        case _           => None
      }
    }
  }

  override def toString: String = {
    s"RowAttributeValues($rowId => $original)"
  }
}

case class ReadThroughAttributeValues(updates: Map[FieldLike[Any], Any], original: AttributeValues)
    extends AttributeValues {

  override def apply(attr: String): Any = {
    val filtered = updates.filter(t => t._1.accessor == attr);
    if (!filtered.isEmpty) {
      return filtered.head._2
    } else {
      original(attr)
    }

  }
  def apply[T](field: FieldLike[T]): Option[T] = {
    val f = field.asInstanceOf[FieldLike[Any]]; // maybe fix the type param at some point
    if (updates.contains(f)) {
      updates(f) match {
        case v if field.reader.isInstanceOf(v) => Some(v.asInstanceOf[T])
        case s: String                         => field.read(s);
        case a                                 => field.read(a.toString);
      }
    } else {
      original(field)
    }
  }

  override def toString: String = {
    val data = original match {
      case DataAttributeValues(values) =>
        values.map { case (k, v) =>
          s"$k -> $v -> ${apply(k)}"
        }
      case RowAttributeValues(rowId, original) =>
        original.values.map { case (k, v) =>
          s"$k @ $rowId -> $v -> ${apply(k)}"
        }
      case _ => Seq("Invalid")
    }
    s"ReadThroughAttributeValues(${data.mkString(",")})"
  }

  private[sheet] def replaceValues(updates: Seq[(FieldLike[Any], Any)]): ReadThroughAttributeValues = {
    ReadThroughAttributeValues(this.updates ++ updates.toMap, original)
  }
}
