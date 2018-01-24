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

import org.scalatest._
import com.lkroll.roll20.sheet.model._
import com.lkroll.roll20.core._

object TestSheetModel extends SheetModel {
  import CoreImplicits._
  import FieldImplicits._

  override def version = "0.0.0";

  implicit val ctx = this.renderingContext;

  val test1 = "test1".default(5);
  val test2 = "test2".default(20);
  val testIntermediate = number[Int]("test_intermediate");
  val testRes = number[Int]("test_res");
}

object TestWorkers extends SheetWorker {
  import TestSheetModel._

  private val ttt: Tuple2[Int, Int] => Seq[(FieldLike[Any], Any)] = {
    case (t1, t2) => Seq(testIntermediate <<= t1 + t2);
  }

  val interOp = op(testIntermediate) update {
    case i => Seq(testRes <<= i + 5)
  }

  val sideOp = op(testIntermediate) { (o: Option[Int]) =>
    o match {
      case Some(i) => println(s"Lala $i");
      case None    => println("Nothing ;(");
    }
  }

  val op1 = bind(op(test1, test2)) update (ttt, interOp);
  val op2 = op1 andThen sideOp;
}

class OpTest extends FunSuite with Matchers {
  test("MergedOp creation") {
    //println(s"Testing op: ${TestWorkers.op1}");
    TestWorkers.op1 shouldBe a[MergedOpChain];
    val moc = TestWorkers.op1.asInstanceOf[MergedOpChain];
    moc.operations should not be empty;
  }

  test("ChainedOp creation") {
    //println(s"Testing op: ${TestWorkers.op2}");
    TestWorkers.op2 shouldBe a[ChainedOpChain];
    val coc = TestWorkers.op2.asInstanceOf[ChainedOpChain];
    coc.operations should not be empty;
    coc.operations should have length 2;
  }
}
