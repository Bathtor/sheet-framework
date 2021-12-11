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

package com.lkroll.roll20.testsheet

import com.lkroll.roll20.facade.Roll20;
import com.lkroll.roll20.facade.Roll20.EventInfo;
import com.lkroll.roll20.sheet._
import com.lkroll.roll20.sheet.model._
import com.lkroll.roll20.core._
import com.lkroll.roll20.testmodel._
import SheetWorkerTypeShorthands._
import util.{Failure, Success}
import concurrent.{ExecutionContext, Future, Promise}
import scala.scalajs.js
import scala.scalajs.js.annotation._

@JSExportTopLevel("TestSheetWorkers")
object TestSheetWorkers extends SheetWorkerRoot {

  override def children: Seq[SheetWorker] = Seq(tabbedWorker, TestUpdates);

  lazy val tabbedWorker = TabbedWorker(TestCharModel, TestUpdates);

  import TestCharModel._

  register[ChatCommand](ChatSer);
  register[Boolean](ToggleSer);

  onOpen {
    log("TestSheet: Sheet workers loading...");
    //versionLoadOp();
    ()
  };

}
