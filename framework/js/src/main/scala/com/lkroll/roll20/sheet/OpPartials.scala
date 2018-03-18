/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2017 Lars Kroll <bathtor@googlemail.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of sheet software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and sheet permission notice shall be included in all
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

import com.lkroll.roll20.core._

trait OpPartials {

  def sheet: SheetWorker;

  def nop: FieldOps[Unit] = NoOp(sheet);

  def op[T](f: FieldLike[T]): FieldOpsWithFields[T] = {
    val mapper = (attrs: AttributeValues) => attrs(f);
    val ctx = new SheetWorkerOpPartial(Tuple1(f), mapper, sheet);
    ctx
  }

  def op[T1, T2](f1: FieldLike[T1], f2: FieldLike[T2]): FieldOpsWithFields[(T1, T2)] = {
    val mapper = (attrs: AttributeValues) => for {
      t1 <- attrs(f1);
      t2 <- attrs(f2)
    } yield (t1, t2)
    val ctx = new SheetWorkerOpPartial((f1, f2), mapper, sheet);
    ctx
  }

  def op[T1, T2, T3](f1: FieldLike[T1], f2: FieldLike[T2], f3: FieldLike[T3]): FieldOpsWithFields[(T1, T2, T3)] = {
    val mapper = (attrs: AttributeValues) => for {
      t1 <- attrs(f1);
      t2 <- attrs(f2);
      t3 <- attrs(f3)
    } yield (t1, t2, t3)
    val ctx = new SheetWorkerOpPartial((f1, f2, f3), mapper, sheet);
    ctx
  }

  def op[T1, T2, T3, T4](f1: FieldLike[T1], f2: FieldLike[T2], f3: FieldLike[T3], f4: FieldLike[T4]): FieldOpsWithFields[(T1, T2, T3, T4)] = {
    val mapper = (attrs: AttributeValues) => for {
      t1 <- attrs(f1);
      t2 <- attrs(f2);
      t3 <- attrs(f3);
      t4 <- attrs(f4)
    } yield (t1, t2, t3, t4)
    val ctx = new SheetWorkerOpPartial((f1, f2, f3, f4), mapper, sheet);
    ctx
  }

  def op[T1, T2, T3, T4, T5](f1: FieldLike[T1], f2: FieldLike[T2], f3: FieldLike[T3], f4: FieldLike[T4],
                             f5: FieldLike[T5]): FieldOpsWithFields[(T1, T2, T3, T4, T5)] = {
    val mapper = (attrs: AttributeValues) => for {
      t1 <- attrs(f1);
      t2 <- attrs(f2);
      t3 <- attrs(f3);
      t4 <- attrs(f4);
      t5 <- attrs(f5)
    } yield (t1, t2, t3, t4, t5)
    val ctx = new SheetWorkerOpPartial((f1, f2, f3, f4, f5), mapper, sheet);
    ctx
  }

  def op[T1, T2, T3, T4, T5, T6](
    f1: FieldLike[T1], f2: FieldLike[T2], f3: FieldLike[T3], f4: FieldLike[T4],
    f5: FieldLike[T5], f6: FieldLike[T6]): FieldOpsWithFields[(T1, T2, T3, T4, T5, T6)] = {
    val mapper = (attrs: AttributeValues) => for {
      t1 <- attrs(f1);
      t2 <- attrs(f2);
      t3 <- attrs(f3);
      t4 <- attrs(f4);
      t5 <- attrs(f5);
      t6 <- attrs(f6)
    } yield (t1, t2, t3, t4, t5, t6)
    val ctx = new SheetWorkerOpPartial((f1, f2, f3, f4, f5, f6), mapper, sheet);
    ctx
  }

  def op[T1, T2, T3, T4, T5, T6, T7](
    f1: FieldLike[T1], f2: FieldLike[T2], f3: FieldLike[T3], f4: FieldLike[T4],
    f5: FieldLike[T5], f6: FieldLike[T6], f7: FieldLike[T7]): FieldOpsWithFields[(T1, T2, T3, T4, T5, T6, T7)] = {
    val mapper = (attrs: AttributeValues) => for {
      t1 <- attrs(f1);
      t2 <- attrs(f2);
      t3 <- attrs(f3);
      t4 <- attrs(f4);
      t5 <- attrs(f5);
      t6 <- attrs(f6);
      t7 <- attrs(f7)
    } yield (t1, t2, t3, t4, t5, t6, t7)
    val ctx = new SheetWorkerOpPartial((f1, f2, f3, f4, f5, f6, f7), mapper, sheet);
    ctx
  }

  def op[T1, T2, T3, T4, T5, T6, T7, T8](
    f1: FieldLike[T1], f2: FieldLike[T2], f3: FieldLike[T3], f4: FieldLike[T4],
    f5: FieldLike[T5], f6: FieldLike[T6], f7: FieldLike[T7], f8: FieldLike[T8]): FieldOpsWithFields[(T1, T2, T3, T4, T5, T6, T7, T8)] = {
    val mapper = (attrs: AttributeValues) => for {
      t1 <- attrs(f1);
      t2 <- attrs(f2);
      t3 <- attrs(f3);
      t4 <- attrs(f4);
      t5 <- attrs(f5);
      t6 <- attrs(f6);
      t7 <- attrs(f7);
      t8 <- attrs(f8)
    } yield (t1, t2, t3, t4, t5, t6, t7, t8)
    val ctx = new SheetWorkerOpPartial((f1, f2, f3, f4, f5, f6, f7, f8), mapper, sheet);
    ctx
  }

  def op[T1, T2, T3, T4, T5, T6, T7, T8, T9](
    f1: FieldLike[T1], f2: FieldLike[T2], f3: FieldLike[T3], f4: FieldLike[T4],
    f5: FieldLike[T5], f6: FieldLike[T6], f7: FieldLike[T7], f8: FieldLike[T8],
    f9: FieldLike[T9]): FieldOpsWithFields[(T1, T2, T3, T4, T5, T6, T7, T8, T9)] = {
    val mapper = (attrs: AttributeValues) => for {
      t1 <- attrs(f1);
      t2 <- attrs(f2);
      t3 <- attrs(f3);
      t4 <- attrs(f4);
      t5 <- attrs(f5);
      t6 <- attrs(f6);
      t7 <- attrs(f7);
      t8 <- attrs(f8);
      t9 <- attrs(f9)
    } yield (t1, t2, t3, t4, t5, t6, t7, t8, t9)
    val ctx = new SheetWorkerOpPartial((f1, f2, f3, f4, f5, f6, f7, f8, f9), mapper, sheet);
    ctx
  }

  def op[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](
    f1: FieldLike[T1], f2: FieldLike[T2], f3: FieldLike[T3], f4: FieldLike[T4],
    f5: FieldLike[T5], f6: FieldLike[T6], f7: FieldLike[T7], f8: FieldLike[T8],
    f9: FieldLike[T9], f10: FieldLike[T10]): FieldOpsWithFields[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10)] = {
    val mapper = (attrs: AttributeValues) => for {
      t1 <- attrs(f1);
      t2 <- attrs(f2);
      t3 <- attrs(f3);
      t4 <- attrs(f4);
      t5 <- attrs(f5);
      t6 <- attrs(f6);
      t7 <- attrs(f7);
      t8 <- attrs(f8);
      t9 <- attrs(f9);
      t10 <- attrs(f10)
    } yield (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10)
    val ctx = new SheetWorkerOpPartial((f1, f2, f3, f4, f5, f6, f7, f8, f9, f10), mapper, sheet);
    ctx
  }

  def op[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](
    f1: FieldLike[T1], f2: FieldLike[T2], f3: FieldLike[T3], f4: FieldLike[T4],
    f5: FieldLike[T5], f6: FieldLike[T6], f7: FieldLike[T7], f8: FieldLike[T8],
    f9: FieldLike[T9], f10: FieldLike[T10], f11: FieldLike[T11]): FieldOpsWithFields[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11)] = {
    val mapper = (attrs: AttributeValues) => for {
      t1 <- attrs(f1);
      t2 <- attrs(f2);
      t3 <- attrs(f3);
      t4 <- attrs(f4);
      t5 <- attrs(f5);
      t6 <- attrs(f6);
      t7 <- attrs(f7);
      t8 <- attrs(f8);
      t9 <- attrs(f9);
      t10 <- attrs(f10);
      t11 <- attrs(f11)
    } yield (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11)
    val ctx = new SheetWorkerOpPartial((f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11), mapper, sheet);
    ctx
  }

  def op[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12](
    f1: FieldLike[T1], f2: FieldLike[T2], f3: FieldLike[T3], f4: FieldLike[T4],
    f5: FieldLike[T5], f6: FieldLike[T6], f7: FieldLike[T7], f8: FieldLike[T8],
    f9: FieldLike[T9], f10: FieldLike[T10], f11: FieldLike[T11], f12: FieldLike[T12]): FieldOpsWithFields[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12)] = {
    val mapper = (attrs: AttributeValues) => for {
      t1 <- attrs(f1);
      t2 <- attrs(f2);
      t3 <- attrs(f3);
      t4 <- attrs(f4);
      t5 <- attrs(f5);
      t6 <- attrs(f6);
      t7 <- attrs(f7);
      t8 <- attrs(f8);
      t9 <- attrs(f9);
      t10 <- attrs(f10);
      t11 <- attrs(f11);
      t12 <- attrs(f12)
    } yield (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12)
    val ctx = new SheetWorkerOpPartial((f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12), mapper, sheet);
    ctx
  }

  def op[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13](
    f1: FieldLike[T1], f2: FieldLike[T2], f3: FieldLike[T3], f4: FieldLike[T4],
    f5: FieldLike[T5], f6: FieldLike[T6], f7: FieldLike[T7], f8: FieldLike[T8],
    f9: FieldLike[T9], f10: FieldLike[T10], f11: FieldLike[T11], f12: FieldLike[T12],
    f13: FieldLike[T13]): FieldOpsWithFields[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13)] = {
    val mapper = (attrs: AttributeValues) => for {
      t1 <- attrs(f1);
      t2 <- attrs(f2);
      t3 <- attrs(f3);
      t4 <- attrs(f4);
      t5 <- attrs(f5);
      t6 <- attrs(f6);
      t7 <- attrs(f7);
      t8 <- attrs(f8);
      t9 <- attrs(f9);
      t10 <- attrs(f10);
      t11 <- attrs(f11);
      t12 <- attrs(f12);
      t13 <- attrs(f13)
    } yield (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13)
    val ctx = new SheetWorkerOpPartial((f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13), mapper, sheet);
    ctx
  }

  def op[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14](
    f1: FieldLike[T1], f2: FieldLike[T2], f3: FieldLike[T3], f4: FieldLike[T4],
    f5: FieldLike[T5], f6: FieldLike[T6], f7: FieldLike[T7], f8: FieldLike[T8],
    f9: FieldLike[T9], f10: FieldLike[T10], f11: FieldLike[T11], f12: FieldLike[T12],
    f13: FieldLike[T13], f14: FieldLike[T14]): FieldOpsWithFields[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14)] = {
    val mapper = (attrs: AttributeValues) => for {
      t1 <- attrs(f1);
      t2 <- attrs(f2);
      t3 <- attrs(f3);
      t4 <- attrs(f4);
      t5 <- attrs(f5);
      t6 <- attrs(f6);
      t7 <- attrs(f7);
      t8 <- attrs(f8);
      t9 <- attrs(f9);
      t10 <- attrs(f10);
      t11 <- attrs(f11);
      t12 <- attrs(f12);
      t13 <- attrs(f13);
      t14 <- attrs(f14)
    } yield (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14)
    val ctx = new SheetWorkerOpPartial((f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14), mapper, sheet);
    ctx
  }

  def op[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15](
    f1: FieldLike[T1], f2: FieldLike[T2], f3: FieldLike[T3], f4: FieldLike[T4],
    f5: FieldLike[T5], f6: FieldLike[T6], f7: FieldLike[T7], f8: FieldLike[T8],
    f9: FieldLike[T9], f10: FieldLike[T10], f11: FieldLike[T11], f12: FieldLike[T12],
    f13: FieldLike[T13], f14: FieldLike[T14], f15: FieldLike[T15]): FieldOpsWithFields[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15)] = {
    val mapper = (attrs: AttributeValues) => for {
      t1 <- attrs(f1);
      t2 <- attrs(f2);
      t3 <- attrs(f3);
      t4 <- attrs(f4);
      t5 <- attrs(f5);
      t6 <- attrs(f6);
      t7 <- attrs(f7);
      t8 <- attrs(f8);
      t9 <- attrs(f9);
      t10 <- attrs(f10);
      t11 <- attrs(f11);
      t12 <- attrs(f12);
      t13 <- attrs(f13);
      t14 <- attrs(f14);
      t15 <- attrs(f15)
    } yield (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15)
    val ctx = new SheetWorkerOpPartial((f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15), mapper, sheet);
    ctx
  }

  // TODO either do more or figure out the templating thingy
}
