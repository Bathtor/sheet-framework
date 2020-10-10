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

import scalajs.js
import com.lkroll.roll20.sheet.model._
import com.lkroll.roll20.core._
import concurrent.{ExecutionContext, Future, Promise}
import util.{Failure, Success, Try}

case class TabbedWorker(model: SheetModel, manager: UpdateManager) extends SheetWorker {

  //register(model.showOverlay, ToggleSer);

  val activateOverlay = nop { _: Option[Unit] =>
    setAttrs(Map(model.showOverlay <<= true))
  }
  val deactivateOverlay = nop { _: Option[Unit] =>
    setAttrs(Map(model.showOverlay <<= false))
  }

  val closeOverlay = bind(op(model.closeOverlay)) update { _ =>
    Seq(model.closeOverlay <<= false, model.showOverlay <<= false)
  }

  def setProcessing(count: Int) = nop { _: Option[Unit] =>
    debug(s"Setting processing count=$count");
    setAttrs(Map(model.processingCount <<= count))
  }

  val decrementProcessing = op(model.processingCount) { oI: Option[Int] =>
    //    debug("Wasting some time.");
    //    val r = 0.to(100000000).product; // waste some time
    //    debug(s"Done wasting some time $r.");

    oI match {
      case Some(count) => {
        debug(s"Decrementing count=$count");
        setAttrs(Map(model.processingCount <<= count - 1))
      }
      case None => {
        debug("Could not read count, setting to 0");
        setAttrs(Map(model.processingCount <<= 0))
      }
    }
  }

  onOpen {
    log("TabbedSheet: Sheet workers loading...");
    versionLoadOp();
    ()
  };

  val versionLoadOp = op(model.versionField) { o: Option[String] =>
    o match {
      case Some(v) if !v.isEmpty() => {
        if (v == model.version) {
          log(s"Loaded sheet with version $v");
          //Promise[Unit]().success(()).future
          Future.successful(())
        } else {
          log(s"Loaded sheet with version $v < ${model.version}");
          val f = for {
            _ <- activateOverlay();
            _ <- {
              val updates = manager.update(v, model.version);
              val count = updates.size;
              debug(s"Got $count updates to process.");
              val updatesProcessing = setProcessing(count) :: updates.flatMap(op => List(op, decrementProcessing));
              debug(s"Got update processing: ${updatesProcessing}");
              val op = SheetWorkerOpChain(updatesProcessing);
              debug(s"Got update ops: ${op}");
              op()
            };
            _ <- deactivateOverlay()
          } yield ();
          f.onComplete {
            case Success(_) => debug("Update completed fine.");
            case Failure(e) => {
              debug(s"Updated failed with ${e.getMessage}:");
              error(e.getStackTrace.mkString("\n"));
            }
          }
          f
        }
      }
      case _ => {
        log(s"Loaded unversioned sheet!");
        for {
          _ <- activateOverlay();
          _ <- SheetWorkerOpChain(manager.updateUnversioned(model.version))();
          _ <- deactivateOverlay()
        } yield ()
        //setAttrs(Map(versionField <<= version, characterSheet <<= s"$sheetName v$version"))
      }
    }
  };
}
