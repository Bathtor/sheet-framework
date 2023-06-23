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

import concurrent.{ExecutionContext, Future, Promise}
import collection.mutable;
import util.{Failure, Success, Try}
import annotation.tailrec
import com.lkroll.roll20.core._
import com.lkroll.roll20.sheet.model._

object SheetWorkerTypeShorthands {
  type Updates = Seq[(FieldLike[Any], Any)];
  val emptyUpdates = Seq.empty[(FieldLike[Any], Any)];
  type UpdateDecision = (Updates, ChainingDecision);
}

import SheetWorkerTypeShorthands._
import com.lkroll.roll20.sheet.model.RepeatingSection

sealed trait FieldOps[T] {

  protected def defaultChainingDecision(
      f: Option[T] => Future[Unit]
  )(implicit ec: ExecutionContext): Option[T] => Future[ChainingDecision] = { (ot: Option[T]) =>
    f(ot).map(_ => ExecuteChain)
  }

  protected def defaultUpdateChainingDecision(f: T => Updates)(implicit
      ec: ExecutionContext): T => UpdateDecision = { (t: T) =>
    (f(t), ExecuteChain)
  }

  def apply(
      f: Option[T] => Unit)(implicit ec: ExecutionContext, dummy: DummyImplicit): SheetWorkerOp = {
    val f2 = (ot: Option[T]) => {
      val p = Promise[Unit]();
      p.success(f(ot));
      p.future
    };
    apply(f2)
  };
  def apply(f: Option[T] => Future[Unit])(implicit ec: ExecutionContext): SheetWorkerOp;

  def sheet: SheetWorker;

  def update(f: T => Updates)(implicit ec: ExecutionContext): SheetWorkerOp;
}

sealed trait FieldOpsWithCompleter[T] extends FieldOps[T] {
  def apply(f: Option[T] => Unit, onComplete: Try[Unit] => Unit)(implicit
      ec: ExecutionContext,
      dummy: DummyImplicit
  ): SheetWorkerOp = {
    val f2 = (ot: Option[T]) => {
      val p = Promise[Unit]();
      p.success(f(ot));
      p.future
    };
    apply(f2, onComplete)
  };
  def apply(f: Option[T] => Future[Unit], onComplete: Try[Unit] => Unit)(implicit
      ec: ExecutionContext): SheetWorkerOp;

  def update(f: T => Updates, onComplete: Try[Unit] => Unit)(implicit
      ec: ExecutionContext): SheetWorkerOp;
}

sealed trait FieldOpsWithChain[T] extends FieldOps[T] {
  def apply(
      f: Option[T] => ChainingDecision,
      op: SheetWorkerOp
  )(implicit
      ec: ExecutionContext,
      d1: DummyImplicit,
      d2: DummyImplicit,
      d3: DummyImplicit): SheetWorkerOp = {
    val f2 = (ot: Option[T]) => {
      val p = Promise[ChainingDecision]();
      p.success(f(ot));
      p.future
    };
    apply(f2, op)
  };

  def apply(f: Option[T] => Unit, op: SheetWorkerOp)(implicit
      ec: ExecutionContext,
      d1: DummyImplicit,
      d2: DummyImplicit): SheetWorkerOp = {
    val f2 = (ot: Option[T]) => {
      val p = Promise[ChainingDecision]();
      f(ot);
      p.success(ExecuteChain);
      p.future
    };
    apply(f2, op)
  };

  def apply(f: Option[T] => Future[Unit], op: SheetWorkerOp)(implicit
      ec: ExecutionContext,
      d1: DummyImplicit
  ): SheetWorkerOp = {
    apply(defaultChainingDecision(f), op)
  }

  def apply(f: Option[T] => Future[ChainingDecision], op: SheetWorkerOp)(implicit
      ec: ExecutionContext): SheetWorkerOp;

  def update(f: T => Updates, nextOp: SheetWorkerOp)(implicit ec: ExecutionContext): SheetWorkerOp;

  def update(f: T => UpdateDecision, nextOp: SheetWorkerOp)(implicit
      ec: ExecutionContext,
      d1: DummyImplicit
  ): SheetWorkerOp;
}

sealed trait FieldOpsWithFields[T] extends FieldOps[T] {
  def getFields: Seq[FieldLike[Any]];
  def mapper: AttributeValues => Option[T];

  def fold[Acc](section: RepeatingSection, initialValue: Acc)(
      f: (Acc, (String, T)) => Acc
  )(r: Acc => Updates)(implicit ec: ExecutionContext): SheetWorkerOp = {
    NoOp(sheet) { _: Option[Unit] =>
      sheet.foldRows(section, this, initialValue, f, r).map(_ => ())
    }

  }

  def fold(
      section: RepeatingSection
  )(f: (Updates, (String, T)) => Updates)(implicit ec: ExecutionContext): SheetWorkerOp = {
    NoOp(sheet) { _: Option[Unit] =>
      sheet.foldRows(section, this, emptyUpdates, f, identity[Updates]).map(_ => ())
    }
  }

  def sum[N: Numeric](section: RepeatingSection, outputField: FieldLike[N])(
      toNum: T => N
  )(implicit ec: ExecutionContext): SheetWorkerOp = {
    NoOp(sheet) { _: Option[Unit] =>
      val nev = implicitly[Numeric[N]];
      val f: (N, (String, T)) => N = (acc: N, v: (String, T)) =>
        v match {
          case (_, t) => nev.plus(acc, toNum(t))
        };
      val r: N => Updates =
        (acc: N) => Seq((outputField -> acc).asInstanceOf[(FieldLike[Any], Any)]);
      sheet.foldRows(section, this, nev.zero, f, r).map(_ => ())
    }
  }
}

case class NoOp(sheet: SheetWorker) extends FieldOpsWithFields[Unit] {

  def getFields: Seq[FieldLike[Any]] = Seq.empty;
  def mapper: AttributeValues => Option[Unit] = (_: AttributeValues) => Some(());

  override def apply(f: Option[Unit] => Future[Unit])(implicit
      ec: ExecutionContext): SheetWorkerOp = {
    new SideEffectingSheetWorkerOp(this, defaultChainingDecision(f));
  }

  override def update(f: Unit => Updates)(implicit ec: ExecutionContext): SheetWorkerOp = {
    new WritingSheetWorkerOp(this, f);
  }
}

class SheetWorkerOpPartial[T](
    private val t: Product,
    val mapper: AttributeValues => Option[T],
    val sheet: SheetWorker)
  extends FieldOpsWithFields[T] {

  val getFields = t.productIterator.map {
    case f: FieldLike[Any] @unchecked => f
    case x => throw new IllegalArgumentException(s"Expected FieldLike, got $x");
  }.toSeq;

  override def apply(f: Option[T] => Future[Unit])(implicit ec: ExecutionContext): SheetWorkerOp = {
    new SideEffectingSheetWorkerOp(this, defaultChainingDecision(f));
  }

  override def update(f: T => Updates)(implicit ec: ExecutionContext): SheetWorkerOp = {
    new WritingSheetWorkerOp(this, f);
  }
}

class SheetWorkerBinding[T](partial: SheetWorkerOpPartial[T])
  extends FieldOpsWithCompleter[T]
  with FieldOpsWithChain[T] {
  import partial.getFields

  def sheet = partial.sheet;

  val trigger: String = getFields.map(f => s"change:${f.selector}").mkString(" ");

  override def apply(f: Option[T] => Future[Unit])(implicit ec: ExecutionContext): SheetWorkerOp = {
    val op = new SideEffectingSheetWorkerOp(partial, defaultChainingDecision(f));
    bind(op);
    op
  }

  override def apply(f: Option[T] => Future[Unit], onComplete: Try[Unit] => Unit)(implicit
      ec: ExecutionContext
  ): SheetWorkerOp = {
    val op = new SideEffectingSheetWorkerOp(partial, defaultChainingDecision(f));
    bind(op, onComplete);
    op
  }

  override def apply(f: Option[T] => Future[ChainingDecision], nextOp: SheetWorkerOp)(implicit
      ec: ExecutionContext
  ): SheetWorkerOp = {
    val op = new SideEffectingSheetWorkerOp(partial, f) andThen nextOp;
    bind(op);
    op
  }

  private def bind(op: SheetWorkerOp)(implicit ec: ExecutionContext): Unit = {
    sheet.on(
      trigger,
      (e) => {
        sheet.log(s"Op triggered for: ${trigger}");
        op().onComplete {
          case Success(_) => () // ignore
          case Failure(e) => sheet.error(e)
        };
      });
  }

  private def bind(op: SheetWorkerOp, onComplete: Try[Unit] => Unit)(implicit
      ec: ExecutionContext): Unit = {
    sheet.on(
      trigger,
      (e) => {
        sheet.log(s"Op triggered for: ${trigger}");
        op().onComplete(x => onComplete(x.map(_ => ())));
      });
  }

  override def update(f: T => Updates)(implicit ec: ExecutionContext): SheetWorkerOp = {
    val op = new WritingSheetWorkerOp(partial, f);
    bind(op);
    op
  }

  override def update(f: T => Updates, onComplete: Try[Unit] => Unit)(implicit
      ec: ExecutionContext): SheetWorkerOp = {
    val op = new WritingSheetWorkerOp(partial, f);
    bind(op, onComplete);
    op
  }

  override def update(f: T => Updates, nextOp: SheetWorkerOp)(implicit
      ec: ExecutionContext): SheetWorkerOp = {
    val op = new WritingSheetWorkerOp(partial, f) andThen nextOp;
    bind(op);
    op
  }

  override def update(f: T => UpdateDecision, nextOp: SheetWorkerOp)(implicit
      ec: ExecutionContext,
      d1: DummyImplicit
  ): SheetWorkerOp = {
    val op = new WritingNoMergeSheetWorkerOp(partial, f) andThen nextOp;
    bind(op);
    op
  }
}

sealed trait SheetWorkerOp {

  def sheet: SheetWorker;
  def inputFields: Set[FieldLike[_]];
  def computeOutput(attrs: AttributeValues)(implicit ec: ExecutionContext): Future[UpdateDecision];
  def apply()(implicit ec: ExecutionContext): Future[ChainingDecision] = {
    sheet.log(s"Executing op:\n${this}");
    val p = Promise[ChainingDecision]();
    sheet.log(s"Getting values:\n${inputFields.map(_.accessor).mkString(",")}");
    val setF = for {
      attrs <- getIfNecessary(inputFields);
      (data, chainD) <- computeOutput(attrs);
      set <- setIfNecessary(data).map(_ => chainD)
    } yield set;
    p.completeWith(setF);
    p.future
  }

  def ++(ops: List[SheetWorkerOp]): SheetWorkerOp = SheetWorkerOpChain(this :: ops);
  def andThen(op: SheetWorkerOp): SheetWorkerOp = this ++ List(op);
  def all(section: RepeatingSection)(implicit ec: ExecutionContext): SheetWorkerOp = {
    NoOp(sheet) { _: Option[Unit] =>
      sheet.forAllRows(section, this).map(_ => ())
    }
  }

  protected def getIfNecessary(input: Set[FieldLike[_]]): Future[AttributeValues] = {
    if (input.isEmpty) {
      sheet.log("Not getting any values as input fields were empty.");
      Promise[AttributeValues]().success(null).future
    } else {
      sheet.log(s"Getting values:\n${input.map(f => f.accessor).mkString(",")}")
      sheet.getAttrs(input);
    }
  }

  protected def setIfNecessary(output: Seq[(FieldLike[Any], Any)]): Future[Unit] = {
    if (output.isEmpty) {
      sheet.log("Not setting any values as none were returned.");
      Future.successful(())
    } else {
      sheet.log(
        s"Setting values:\n${output.map(t => t._1.accessor + " -> " + t._2).mkString(",")}");
      sheet.setAttrs(output.toMap)
    }
  }
}

case class SideEffectingSheetWorkerOp[T](
    partial: FieldOpsWithFields[T],
    application: Option[T] => Future[ChainingDecision])
  extends SheetWorkerOp {
  override def sheet: SheetWorker = partial.sheet;
  override def inputFields: Set[FieldLike[_]] = partial.getFields.toSet;
  override def computeOutput(attrs: AttributeValues)(implicit
      ec: ExecutionContext): Future[UpdateDecision] = {
    val f = (partial.mapper andThen application)(attrs);
    f.map(d => (emptyUpdates, d))
  }
}

case class WritingSheetWorkerOp[T](partial: FieldOpsWithFields[T], application: T => Updates)
  extends SheetWorkerOp {
  override def sheet: SheetWorker = partial.sheet;
  override def inputFields: Set[FieldLike[_]] = partial.getFields.toSet;
  override def computeOutput(attrs: AttributeValues)(implicit
      ec: ExecutionContext): Future[UpdateDecision] = {
    partial.mapper(attrs) match {
      case Some(t) => Future.successful((application(t), ExecuteChain))
      case None => {
        val missing = inputFields.map(f => f -> attrs(f)).filter(t => t._2.isEmpty);
        sheet.error(s"Could not get attributes for:\n${missing.mkString(",")}\nGot:\n${attrs}");
        Future.failed(new RuntimeException("Some attributes could not found!"))
      }
    }
  }
}

case class WritingNoMergeSheetWorkerOp[T](
    partial: FieldOpsWithFields[T],
    application: T => UpdateDecision)
  extends SheetWorkerOp {
  override def sheet: SheetWorker = partial.sheet;
  override def inputFields: Set[FieldLike[_]] = partial.getFields.toSet;
  override def computeOutput(attrs: AttributeValues)(implicit
      ec: ExecutionContext): Future[UpdateDecision] = {
    partial.mapper(attrs) match {
      case Some(t) => Future.successful(application(t))
      case None => {
        val missing = inputFields.map(f => f -> attrs(f)).filter(t => t._2.isEmpty);
        sheet.error(s"Could not get attributes for:\n${missing.mkString(",")}\nGot:\n${attrs}");
        Future.failed(new RuntimeException("Some attributes could not found!"))
      }
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

sealed trait ChainingDecision {
  def &(other: ChainingDecision): ChainingDecision = (this, other) match {
    case (ExecuteChain, ExecuteChain) => ExecuteChain
    case _                            => SkipChain
  }
  def |(other: ChainingDecision): ChainingDecision = (this, other) match {
    case (SkipChain, SkipChain) => SkipChain
    case _                      => ExecuteChain
  }
}
case object ExecuteChain extends ChainingDecision
case object SkipChain extends ChainingDecision

sealed private[sheet] trait ChainOperation {
  def apply()(implicit ec: ExecutionContext): Future[ChainingDecision];
  def lift(): SheetWorkerOp;
}
private[sheet] case class SideEffecting(op: SideEffectingSheetWorkerOp[_]) extends ChainOperation {
  def apply()(implicit ec: ExecutionContext): Future[ChainingDecision] = op();
  def lift(): SheetWorkerOp = op;
}
private[sheet] case class Merged(op: MergedOpChain) extends ChainOperation {
  def apply()(implicit ec: ExecutionContext): Future[ChainingDecision] = op();
  def lift(): SheetWorkerOp = op;
}
private[sheet] case class Unmerged(op: WritingNoMergeSheetWorkerOp[_]) extends ChainOperation {
  def apply()(implicit ec: ExecutionContext): Future[ChainingDecision] = op();
  def lift(): SheetWorkerOp = op;
}

object SheetWorkerOpChain {
  def apply(operations: List[SheetWorkerOp]): SheetWorkerOp = {
    assert(!operations.isEmpty);
    val head = operations.head;
    val sheet = head.sheet;
    // sheet.log(s"Creating chain from ops:\n${operations.mkString(",")}");
    if (operations.length == 1) {
      return head;
    }
    val ops = leftDescend(sheet, List(), operations);
    // sheet.log(s"Chain of ops:\n${ops.mkString(",")}");
    assert(!ops.isEmpty);
    if (ops.length == 1) {
      ops.head match {
        case Merged(moc)  => moc
        case Unmerged(wo) => wo
        case SideEffecting(_) =>
          ??? // this case shouldn't happen (would be easy to recover from, but I prefer to find the bug)
      }
    } else {
      ChainedOpChain(sheet, ops)
    }
  }

  @tailrec private def leftDescend(
      sheet: SheetWorker,
      acc: List[ChainOperation],
      rest: List[SheetWorkerOp]): List[ChainOperation] = {
    rest match {
      case h :: r =>
        h match {
          case se: SideEffectingSheetWorkerOp[_] => leftDescend(sheet, SideEffecting(se) :: acc, r)
          case we: WritingSheetWorkerOp[_]       => rightDescend(sheet, List(we), acc, r)
          case wenm: WritingNoMergeSheetWorkerOp[_] => leftDescend(sheet, Unmerged(wenm) :: acc, r)
          case MergedOpChain(_, otherOps)           => rightDescend(sheet, otherOps.reverse, acc, r)
          case ChainedOpChain(_, otherOps) =>
            leftDescend(sheet, acc, otherOps.map(e => e.lift()) ++ r)
        }
      case Nil => acc.reverse
    }
  }

  @tailrec private def rightDescend(
      sheet: SheetWorker,
      wacc: List[WritingSheetWorkerOp[_]],
      acc: List[ChainOperation],
      rest: List[SheetWorkerOp]): List[ChainOperation] = {
    rest match {
      case h :: r =>
        h match {
          case se: SideEffectingSheetWorkerOp[_] =>
            leftDescend(
              sheet,
              SideEffecting(se) :: Merged(MergedOpChain(sheet, wacc.reverse)) :: acc,
              r)
          case we: WritingSheetWorkerOp[_] => rightDescend(sheet, we :: wacc, acc, r)
          case wenm: WritingNoMergeSheetWorkerOp[_] =>
            leftDescend(
              sheet,
              Unmerged(wenm) :: Merged(MergedOpChain(sheet, wacc.reverse)) :: acc,
              r)
          case MergedOpChain(_, otherOps) => rightDescend(sheet, otherOps.reverse ++ wacc, acc, r)
          case ChainedOpChain(_, otherOps) =>
            rightDescend(sheet, wacc, acc, otherOps.map(e => e.lift()) ++ r)
        }
      case Nil => (Merged(MergedOpChain(sheet, wacc.reverse)) :: acc).reverse
    }
  }

}

case class MergedOpChain(sheet: SheetWorker, operations: List[WritingSheetWorkerOp[_]])
  extends SheetWorkerOp {

  private lazy val _inputFields: Set[FieldLike[_]] = {
    operations.map(_.inputFields).fold(Set.empty)((s1, s2) => s1 | s2)
  }

  override def inputFields: Set[FieldLike[_]] = _inputFields;

  override def computeOutput(attrs: AttributeValues)(implicit
      ec: ExecutionContext): Future[UpdateDecision] = {
    val emptyRTAV = ReadThroughAttributeValues(Map.empty[FieldLike[Any], Any], attrs);
    val emptyAcc: Future[(ReadThroughAttributeValues, ChainingDecision)] =
      Future.successful((emptyRTAV, SkipChain));
    val finalF = operations.foldLeft(emptyAcc)((accF, op) => {
      for {
        (updatesAcc, chainDAcc) <- accF;
        (updates, chainD) <- op.computeOutput(updatesAcc)
      } yield {
        val newAttrs = updatesAcc.replaceValues(updates);
        (newAttrs, chainDAcc | chainD)
      }
    });
    for {
      (finalAttrs, finalChainD) <- finalF
    } yield (finalAttrs.updates.toSeq, finalChainD)
  }

  override def apply()(implicit ec: ExecutionContext): Future[ChainingDecision] = {
    sheet.log(s"Executing op:\n${this}");
    super.apply()
  }

  override def ++(ops: List[SheetWorkerOp]): SheetWorkerOp = this andThen SheetWorkerOpChain(ops);
  override def andThen(op: SheetWorkerOp): SheetWorkerOp = op match {
    case se: SideEffectingSheetWorkerOp[_] =>
      ChainedOpChain(sheet, List(Merged(this), SideEffecting(se)))
    case we: WritingSheetWorkerOp[_] => MergedOpChain(sheet, operations :+ we);
    case wenm: WritingNoMergeSheetWorkerOp[_] =>
      ChainedOpChain(sheet, List(Merged(this), Unmerged(wenm)))
    case MergedOpChain(_, otherOps) => MergedOpChain(sheet, operations ++ otherOps);
    case ChainedOpChain(_, otherOps) =>
      otherOps.head match {
        case SideEffecting(_) | Unmerged(_) => ChainedOpChain(sheet, Merged(this) :: otherOps)
        case Merged(moc) => {
          val merged = MergedOpChain(sheet, operations ++ moc.operations);
          ChainedOpChain(sheet, Merged(merged) :: otherOps.tail)
        }
      }
  }
}

case class ChainedOpChain(sheet: SheetWorker, operations: List[ChainOperation])
  extends SheetWorkerOp {

  override def inputFields: Set[FieldLike[_]] = ???; // don't use here

  override def computeOutput(attrs: AttributeValues)(implicit
      ec: ExecutionContext): Future[UpdateDecision] = ???;

  override def apply()(implicit ec: ExecutionContext): Future[ChainingDecision] = {
    sheet.log(s"Executing op:\n${this}");
    val emptyAcc: Future[ChainingDecision] = Future.successful(ExecuteChain);
    operations.foldLeft(emptyAcc) { case (f, op) =>
      f flatMap {
        case ExecuteChain => op.apply()
        case SkipChain    => Future.successful(SkipChain)
      }
    }
  }

  override def ++(ops: List[SheetWorkerOp]): SheetWorkerOp = this andThen SheetWorkerOpChain(ops);
  override def andThen(op: SheetWorkerOp): SheetWorkerOp = op match {
    case se: SideEffectingSheetWorkerOp[_] => ChainedOpChain(sheet, operations :+ SideEffecting(se))
    case we: WritingSheetWorkerOp[_] =>
      ChainedOpChain(sheet, operations :+ Merged(MergedOpChain(sheet, List(we))))
    case wenm: WritingNoMergeSheetWorkerOp[_] => ChainedOpChain(sheet, operations :+ Unmerged(wenm))
    case moc: MergedOpChain                   => ChainedOpChain(sheet, operations :+ Merged(moc))
    case ChainedOpChain(_, otherOps) =>
      (operations.last, otherOps.head) match {
        case (Merged(moc1), Merged(moc2)) => {
          val merged = MergedOpChain(sheet, moc1.operations ++ moc2.operations);
          ChainedOpChain(
            sheet,
            operations.take(operations.length - 1) ++ (Merged(merged) :: otherOps.tail))
        }
        case _ => ChainedOpChain(sheet, operations ++ otherOps)
      }
  }

}
