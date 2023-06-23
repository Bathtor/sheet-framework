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
package com.lkroll.roll20.sheet.utils

import scalajs.js
import com.lkroll.roll20.facade.Roll20
import com.lkroll.roll20.util.ListMultiMap
import collection.mutable

object SubscriptionMap {
  def create: SubscriptionMap = new SubscriptionMap();
}

class SubscriptionMap extends ListMultiMap[String, Function1[Roll20.EventInfo, Unit]] {

  private val underlying =
    new mutable.HashMap[String, mutable.ArrayDeque[Function1[Roll20.EventInfo, Unit]]];

  override def addOne(
      elem: (String, mutable.ArrayDeque[Function1[Roll20.EventInfo, Unit]])
  ): SubscriptionMap.this.type = {
    underlying.addOne(elem);
    this
  };

  def iterator: Iterator[(String, mutable.ArrayDeque[Function1[Roll20.EventInfo, Unit]])] =
    underlying.iterator;

  def get(key: String): Option[mutable.ArrayDeque[Function1[Roll20.EventInfo, Unit]]] =
    underlying.get(key);

  def subtractOne(elem: String): SubscriptionMap.this.type = {
    underlying.subtractOne(elem);
    this
  }

}
