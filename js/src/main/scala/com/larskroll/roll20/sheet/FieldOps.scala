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

import concurrent.{ Future, Promise, ExecutionContext }
import collection.mutable;
import util.{ Try, Success, Failure }
import annotation.tailrec

sealed trait FieldOps[T] {

  def apply(f: Option[T] => Unit)(implicit ec: ExecutionContext): SheetWorkerOp;

  def sheet: SheetWorker;

  def update(f: T => Seq[(FieldLike[Any], Any)])(implicit ec: ExecutionContext): SheetWorkerOp;
}

sealed trait FieldOpsWithCompleter[T] extends FieldOps[T] {
  def apply(f: Option[T] => Unit, onComplete: Try[Unit] => Unit)(implicit ec: ExecutionContext): SheetWorkerOp;

  def update(f: T => Seq[(FieldLike[Any], Any)], onComplete: Try[Unit] => Unit)(implicit ec: ExecutionContext): SheetWorkerOp;
}

sealed trait FieldOpsWithChain[T] extends FieldOps[T] {
  def apply(f: Option[T] => Unit, op: SheetWorkerOp)(implicit ec: ExecutionContext): SheetWorkerOp;

  def update(f: T => Seq[(FieldLike[Any], Any)], nextOp: SheetWorkerOp)(implicit ec: ExecutionContext): SheetWorkerOp;
}

class SheetWorkerOpPartial[T](private val t: Product, val mapper: AttributeValues => Option[T], val sheet: SheetWorker) extends FieldOps[T] {

  val getFields = t.productIterator.map {
    case f: FieldLike[Any] @unchecked => f
    case x                            => throw new IllegalArgumentException(s"Expected FieldLike, got $x");
  }.toSeq;

  override def apply(f: Option[T] => Unit)(implicit ec: ExecutionContext): SheetWorkerOp = {
    new SideEffectingSheetWorkerOp(this, f);
  }

  override def update(f: T => Seq[(FieldLike[Any], Any)])(implicit ec: ExecutionContext): SheetWorkerOp = {
    new WritingSheetWorkerOp(this, f);
  }
}

class SheetWorkerBinding[T](partial: SheetWorkerOpPartial[T]) extends FieldOpsWithCompleter[T] with FieldOpsWithChain[T] {
  import partial.getFields

  def sheet = partial.sheet;

  val trigger: String = getFields.map(f => s"change:${f.selector}").mkString(" ");

  override def apply(f: Option[T] => Unit)(implicit ec: ExecutionContext): SheetWorkerOp = {
    val op = new SideEffectingSheetWorkerOp(partial, f);
    bind(op);
    op
  }

  override def apply(f: Option[T] => Unit, onComplete: Try[Unit] => Unit)(implicit ec: ExecutionContext): SheetWorkerOp = {
    val op = new SideEffectingSheetWorkerOp(partial, f);
    bind(op, onComplete);
    op
  }

  override def apply(f: Option[T] => Unit, nextOp: SheetWorkerOp)(implicit ec: ExecutionContext): SheetWorkerOp = {
    val op = new SideEffectingSheetWorkerOp(partial, f) andThen nextOp;
    bind(op);
    op
  }

  private def bind(op: SheetWorkerOp)(implicit ec: ExecutionContext) {
    sheet.on(trigger, (e) => {
      sheet.log(s"Op triggered for: ${trigger}");
      op().onFailure {
        case e: Throwable => sheet.error(e)
      };
    });
  }

  private def bind(op: SheetWorkerOp, onComplete: Try[Unit] => Unit)(implicit ec: ExecutionContext) {
    sheet.on(trigger, (e) => {
      sheet.log(s"Op triggered for: ${trigger}");
      op().onComplete(onComplete);
    });
  }

  def update(f: T => Seq[(FieldLike[Any], Any)])(implicit ec: ExecutionContext): SheetWorkerOp = {
    val op = new WritingSheetWorkerOp(partial, f);
    bind(op);
    op
  }

  def update(f: T => Seq[(FieldLike[Any], Any)], onComplete: Try[Unit] => Unit)(implicit ec: ExecutionContext): SheetWorkerOp = {
    val op = new WritingSheetWorkerOp(partial, f);
    bind(op, onComplete);
    op
  }

  def update(f: T => Seq[(FieldLike[Any], Any)], nextOp: SheetWorkerOp)(implicit ec: ExecutionContext): SheetWorkerOp = {
    val op = new WritingSheetWorkerOp(partial, f) andThen nextOp;
    bind(op);
    op
  }
}

sealed trait SheetWorkerOp {
  def sheet: SheetWorker;
  def inputFields: Set[FieldLike[_]];
  def computeOutput(attrs: AttributeValues): Seq[(FieldLike[Any], Any)];
  def apply()(implicit ec: ExecutionContext): Future[Unit] = {
    sheet.log(s"Executing op:\n${this}");
    val p = Promise[Unit]();
    sheet.log(s"Getting values:\n${inputFields.map(_.accessor).mkString(",")}");
    val setF = for {
      attrs <- sheet.getAttrs(inputFields);
      set <- setIfNecessary(computeOutput(attrs))
    } yield set;
    p.completeWith(setF);
    p.future
  }

  def ++(ops: List[SheetWorkerOp]): SheetWorkerOp = SheetWorkerOpChain(this :: ops);
  def andThen(op: SheetWorkerOp): SheetWorkerOp = this ++ List(op);

  protected def setIfNecessary(output: Seq[(FieldLike[Any], Any)]): Future[Unit] = {
    if (output.isEmpty) {
      sheet.log("Not setting any values as none were returned.");
      Future.successful(())
    } else {
      sheet.log(s"Setting values:\n${output.map(t => t._1.accessor + " -> " + t._2).mkString(",")}");
      sheet.setAttrs(output.toMap)
    }
  }
}

case class SideEffectingSheetWorkerOp[T](partial: SheetWorkerOpPartial[T], application: Option[T] => Unit) extends SheetWorkerOp {
  override def sheet: SheetWorker = partial.sheet;
  override def inputFields: Set[FieldLike[_]] = partial.getFields.toSet;
  override def computeOutput(attrs: AttributeValues): Seq[(FieldLike[Any], Any)] = {
    (partial.mapper andThen application)(attrs);
    Seq()
  }
}

case class WritingSheetWorkerOp[T](partial: SheetWorkerOpPartial[T], application: T => Seq[(FieldLike[Any], Any)]) extends SheetWorkerOp {
  override def sheet: SheetWorker = partial.sheet;
  override def inputFields: Set[FieldLike[_]] = partial.getFields.toSet;
  override def computeOutput(attrs: AttributeValues): Seq[(FieldLike[Any], Any)] = {
    partial.mapper(attrs) match {
      case Some(t) => application(t)
      case None    => sheet.error(s"Could not get attributes for ${partial.getFields.mkString(",")}"); Seq()
    }
  }
}

//class SheetWorkerOp[T](partial: SheetWorkerOpPartial[T], application: Option[T] => Unit) {
//  import partial._
//
//  def apply()(implicit ec: ExecutionContext): Future[Unit] = {
//    val p = Promise[Unit]();
//    val getF = sheet.getAttrs(getFields);
//    val setF = getF.map(mapper).map(application);
//    p.completeWith(setF);
//    p.future
//  }
//}

object SheetWorkerOpChain {
  def apply(operations: List[SheetWorkerOp]): SheetWorkerOp = {
    assert(!operations.isEmpty);
    val head = operations.head;
    val sheet = head.sheet;
    //sheet.log(s"Creating chain from ops:\n${operations.mkString(",")}");
    if (operations.length == 1) {
      return head;
    }
    val ops = leftDescend(sheet, List(), operations);
    //sheet.log(s"Chain of ops:\n${ops.mkString(",")}");
    assert(!ops.isEmpty);
    if (ops.length == 1) {
      ops.head match {
        case Right(moc) => moc
        case Left(_)    => ??? // this chase shouldn't happen (would be easy to recover from, but I prefer to find the bug)
      }
    } else {
      ChainedOpChain(sheet, ops)
    }
  }

  @tailrec private def leftDescend(sheet: SheetWorker,
                                   acc: List[Either[SideEffectingSheetWorkerOp[_], MergedOpChain]],
                                   rest: List[SheetWorkerOp]): List[Either[SideEffectingSheetWorkerOp[_], MergedOpChain]] = {
    rest match {
      case h :: r => h match {
        case se: SideEffectingSheetWorkerOp[_] => leftDescend(sheet, Left(se) :: acc, r)
        case we: WritingSheetWorkerOp[_]       => rightDescend(sheet, List(we), acc, r)
        case MergedOpChain(_, otherOps)        => rightDescend(sheet, otherOps.reverse, acc, r)
        case ChainedOpChain(_, otherOps)       => leftDescend(sheet, acc, otherOps.map(e => e.fold(identity, identity)) ++ r)
      }
      case Nil => acc.reverse
    }
  }

  @tailrec private def rightDescend(sheet: SheetWorker,
                                    wacc: List[WritingSheetWorkerOp[_]],
                                    acc: List[Either[SideEffectingSheetWorkerOp[_], MergedOpChain]],
                                    rest: List[SheetWorkerOp]): List[Either[SideEffectingSheetWorkerOp[_], MergedOpChain]] = {
    rest match {
      case h :: r => h match {
        case se: SideEffectingSheetWorkerOp[_] => leftDescend(sheet, Left(se) :: Right(MergedOpChain(sheet, wacc.reverse)) :: acc, r)
        case we: WritingSheetWorkerOp[_]       => rightDescend(sheet, we :: wacc, acc, r)
        case MergedOpChain(_, otherOps)        => rightDescend(sheet, otherOps.reverse ++ wacc, acc, r)
        case ChainedOpChain(_, otherOps)       => rightDescend(sheet, wacc, acc, otherOps.map(e => e.fold(identity, identity)) ++ r)
      }
      case Nil => (Right(MergedOpChain(sheet, wacc.reverse)) :: acc).reverse
    }
  }

}

case class MergedOpChain(sheet: SheetWorker, operations: List[WritingSheetWorkerOp[_]]) extends SheetWorkerOp {

  private lazy val _inputFields: Set[FieldLike[_]] = {
    operations.map(_.inputFields).fold(Set.empty)((s1, s2) => s1 | s2)
  }

  override def inputFields: Set[FieldLike[_]] = _inputFields;

  override def computeOutput(attrs: AttributeValues): Seq[(FieldLike[Any], Any)] = {
    val finalAttrs = operations.foldLeft(ReadThroughAttributeValues(Map.empty[FieldLike[Any], Any], attrs))((acc, op) => {
      val updates = op.computeOutput(acc);
      val newAttrs = acc.replaceValues(updates);
      newAttrs
    });
    finalAttrs.updates.toSeq
  }

  override def apply()(implicit ec: ExecutionContext): Future[Unit] = {
    sheet.log(s"Executing op:\n${this}");
    super.apply()
  }

  override def ++(ops: List[SheetWorkerOp]): SheetWorkerOp = this andThen SheetWorkerOpChain(ops);
  override def andThen(op: SheetWorkerOp): SheetWorkerOp = op match {
    case se: SideEffectingSheetWorkerOp[_] => ChainedOpChain(sheet, List(Right(this), Left(se)))
    case we: WritingSheetWorkerOp[_]       => MergedOpChain(sheet, operations :+ we);
    case MergedOpChain(_, otherOps)        => MergedOpChain(sheet, operations ++ otherOps);
    case ChainedOpChain(_, otherOps) => otherOps.head match {
      case Left(_) => ChainedOpChain(sheet, Right(this) :: otherOps)
      case Right(moc) => {
        val merged = MergedOpChain(sheet, operations ++ moc.operations);
        ChainedOpChain(sheet, Right(merged) :: otherOps.tail)
      }
    }
  }
}

case class ChainedOpChain(sheet: SheetWorker, operations: List[Either[SideEffectingSheetWorkerOp[_], MergedOpChain]]) extends SheetWorkerOp {

  override def inputFields: Set[FieldLike[_]] = ???; // don't use here

  override def computeOutput(attrs: AttributeValues): Seq[(FieldLike[Any], Any)] = ???;

  override def apply()(implicit ec: ExecutionContext): Future[Unit] = {
    sheet.log(s"Executing op:\n${this}");
    operations.foldLeft(Future.successful(())) {
      case (f, op) => f andThen {
        case Success(_) => op.fold(_.apply(), _.apply())
        case Failure(e) => sheet.error(e)
      }
    }
  }

  override def ++(ops: List[SheetWorkerOp]): SheetWorkerOp = this andThen SheetWorkerOpChain(ops);
  override def andThen(op: SheetWorkerOp): SheetWorkerOp = op match {
    case se: SideEffectingSheetWorkerOp[_] => ChainedOpChain(sheet, operations :+ Left(se))
    case we: WritingSheetWorkerOp[_]       => ChainedOpChain(sheet, operations :+ Right(MergedOpChain(sheet, List(we))))
    case moc: MergedOpChain                => ChainedOpChain(sheet, operations :+ Right(moc))
    case ChainedOpChain(_, otherOps) => (operations.last, otherOps.head) match {
      case (Right(moc1), Right(moc2)) => {
        val merged = MergedOpChain(sheet, moc1.operations ++ moc2.operations);
        ChainedOpChain(sheet, operations.take(operations.length - 1) ++ (Right(merged) :: otherOps.tail))
      }
      case _ => ChainedOpChain(sheet, operations ++ otherOps)
    }
  }

  //  def runChained()(implicit ec: ExecutionContext): Future[Unit] = {
  //    ops.foldLeft(Future.successful(())) {
  //      case (f, op) => f andThen {
  //        case Success(_) => op()
  //        case Failure(e) => e.printStackTrace(Console.err);
  //      }
  //    }
  //  }
  //
  //  def runMerged()(implicit ec: ExecutionContext): Future[Unit] = {
  //    // TODO
  //    ???
  //  }

}
