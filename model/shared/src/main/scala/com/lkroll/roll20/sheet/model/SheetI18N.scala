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

trait SheetI18N {
  private var keys = List.empty[String];

  private[roll20] def allKeys = keys;

  def text(key: String): DataKey = {
    keys ::= key;
    DataKey(key)
  }

  def abbr(abbrKey: String, fullKey: String): AbbreviationKey = {
    keys ::= abbrKey;
    keys ::= fullKey;
    AbbreviationKey(abbrKey, fullKey)
  }

  def enum[T <: Enumeration](prefix: String, options: Map[T#Value, String]): OptionKey[T] = {
    val opts = options.map { case (enumval, keySuffix) =>
      enumval -> s"${prefix}-$keySuffix"
    };
    return new OptionKey(opts);
  }

}

sealed trait I18NKey;
case class DataKey(key: String) extends I18NKey {
  def dynamic: DynamicLabel = DynamicLabel(this);
}
case class AbbreviationKey(abbrKey: String, fullKey: String) extends I18NKey;
case class OptionKey[T <: Enumeration](options: Map[T#Value, String]) extends I18NKey;

case class DynamicLabel(key: DataKey) extends Renderable {
  override def render: String = s"^{${key.key}}";
}
