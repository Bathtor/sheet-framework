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

import js.annotation._
import scala.scalajs.js.Dynamic.{ global => dynGlobal, literal => dynLiteral }
import scala.concurrent.{ Future, Promise, ExecutionContext }
import collection.mutable
import scala.util.{ Success, Failure }
import com.lkroll.roll20.core._
import com.lkroll.roll20.sheet.model._
import com.lkroll.roll20.util.ListMultiMap
import com.lkroll.roll20.facade.Roll20;

case class SheetWorkerAPIException(msg: String) extends Throwable {
  override def getMessage(): String = s"SheetWorkerAPIException($msg)";
}

@JSExportDescendentObjects
trait SheetWorkerRoot extends SheetWorker {

  def children: Seq[SheetWorker] = Seq.empty;

  @JSExport
  def load() {
    val aggFieldS = children.foldLeft(this.fieldSerialisers)((acc, child) => acc ++ child.fieldSerialisers);
    val aggTypeS = children.foldLeft(this.typeSerialisers)((acc, child) => acc ++ child.typeSerialisers);
    this.fieldSerialisers = aggFieldS;
    this.typeSerialisers = aggTypeS;
    children.foreach { child =>
      child.fieldSerialisers = aggFieldS;
      child.typeSerialisers = aggTypeS;
    }
    subscriptions.foreach { t: (String, Seq[Function1[Roll20.EventInfo, Unit]]) =>
      {
        val (k, callbacks) = t;
        val f = (e: Roll20.EventInfo) => {
          callbacks.foreach { c => c(e) }
        }
        debug(s"${this.getClass.getName}: subscribing sheetworker on trigger: ${k}.");
        Roll20.on(k, f);
      }
    }
    children.foreach(_.internal_load());
    debug("------ Registered Serialisers -------");
    fieldSerialisers.foreach {
      case (f, s) => debug(s"${f} -> ${s.getClass.getName}")
    }
    typeSerialisers.foreach {
      case (t, s) => debug(s"${t} -> ${s.getClass.getName}")
    }
  }
}

trait SheetWorker extends SheetWorkerLogging with OpPartials {
  import js.JSConverters._
  import SheetWorkerTypeShorthands._
  //import scala.concurrent.ExecutionContext.Implicits.global
  //import scala.scalajs.concurrent.JSExecutionContext.Implicits.runNow;

  implicit val ec: ExecutionContext = scala.scalajs.concurrent.JSExecutionContext.runNow;

  val subscriptions = new mutable.HashMap[String, mutable.MutableList[Function1[Roll20.EventInfo, Unit]]] with ListMultiMap[String, Function1[Roll20.EventInfo, Unit]];

  private[sheet] var fieldSerialisers = Map.empty[String, JSSerialiser[Any]];
  private[sheet] var typeSerialisers = List.empty[(Class[Any], JSSerialiser[Any])];
  val defaultSerialiser = JSDefaultSerialiser;

  private[sheet] def internal_load() {
    subscriptions.foreach { t: (String, Seq[Function1[Roll20.EventInfo, Unit]]) =>
      {
        val (k, callbacks) = t;
        val f = (e: Roll20.EventInfo) => {
          callbacks.foreach { c => c(e) }
        }
        debug(s"${this.getClass.getName}: subscribing sheetworker on trigger: ${k}.");
        Roll20.on(k, f);
      }
    }
  }

  override def sheet: SheetWorker = this;

  def register[T](f: FieldLike[T], s: JSSerialiser[T]) {
    fieldSerialisers += (f.accessor -> s.asInstanceOf[JSSerialiser[Any]]); // just throw away the type info
  }

  def register[T: reflect.ClassTag](s: JSSerialiser[T]) {
    val staticClass: Class[Any] = reflect.classTag[T].runtimeClass.asInstanceOf[Class[Any]];
    typeSerialisers ::= (staticClass -> s.asInstanceOf[JSSerialiser[Any]])
  }

  private def checkTypeHierarchies(cls: Class[_]): Option[JSSerialiser[Any]] = {
    typeSerialisers.find({
      case (targetCls, ser) => targetCls.isAssignableFrom(cls)
    }).map(_._2)
  }

  def serialise[T](f: FieldLike[T], v: T): js.Any = {
    fieldSerialisers.get(f.accessor) match {
      case Some(s) => s.serialise(v)
      case None => checkTypeHierarchies(v.getClass()) match {
        case Some(s) => s.serialise(v)
        case None    => defaultSerialiser.serialise(v)
      }
    }
  }

  def extractSimpleRowId(id: String): String = id.split('_').last;

  def getRowAttrs(section: RepeatingSection, fields: Seq[FieldLike[_]]): Future[Map[String, RowAttributeValues]] = {
    val p = Promise[Map[String, RowAttributeValues]]();
    Roll20.getSectionIDs(section.cls, (ids: js.Array[String]) => {
      val attrNames = ids.map(id => fields.map(f => f.accessor(id))).flatten.toJSArray;
      Roll20.getAttrs(attrNames, (values: js.Dictionary[Any]) => {
        val data = DataAttributeValues(values.toMap);
        val attrs = ids.map(id => (id -> RowAttributeValues(id, data))).toMap;
        p.success(attrs); ()
      });
    });
    p.future
  }

  def foldRows[Acc, T](section: RepeatingSection, fields: FieldOpsWithFields[T],
                       initialValue: Acc, f: (Acc, (String, T)) => Acc,
                       r: Acc => Updates): Future[ChainingDecision] = {
    val resF = for {
      rows <- getRowAttrs(section, fields.getFields)
    } yield {
      val out = rows.foldLeft(initialValue) {
        case (acc, (id, attrs)) => {
          fields.mapper(attrs) match {
            case Some(t) => f(acc, (id, t))
            case None    => acc
          }
        }
      };
      val data = r(out);
      if (data.isEmpty) {
        debug(s"No updates from fold. Skipping write and chain.");
        Future.successful(SkipChain)
      } else {
        setAttrs(data.toMap).map(_ => ExecuteChain)
      }
    };
    resF flatMap identity
  }

  def forAllRows(section: RepeatingSection, op: SheetWorkerOp): Future[ChainingDecision] = {
    val p = Promise[ChainingDecision]();
    Roll20.getSectionIDs(section.cls, (ids: js.Array[String]) => {
      op match {
        case _: SideEffectingSheetWorkerOp[_] | _: WritingSheetWorkerOp[_] | _: MergedOpChain | _: WritingNoMergeSheetWorkerOp[_] => {
          val attrNames = ids.map(id => op.inputFields.map(f => f.accessor(id))).flatten.toJSArray;
          Roll20.getAttrs(attrNames, (values: js.Dictionary[Any]) => {
            val data = DataAttributeValues(values.toMap);
            val attrs = ids.map(id => (id -> RowAttributeValues(id, data))).toMap;
            val output = attrs.mapValues(attrs => op.computeOutput(attrs));
            val outputDataFs = output.map {
              case (id, output) => output.map {
                case (values, cd) => (values.map { case (f, v) => f.accessor(id) -> v.asInstanceOf[js.Any] }, cd)
              }
              //
            };
            val outputDataF = Future.sequence(outputDataFs).map { outputData =>
              val emptyAcc: (Map[String, js.Any], ChainingDecision) = (Map.empty[String, js.Any], SkipChain);
              outputData.foldLeft(emptyAcc) { (acc, dataCD) =>
                val (mapAcc, cdAcc) = acc;
                val (data, cd) = dataCD;
                (mapAcc ++ data, cdAcc | cd)
              }
            }
            //.flatten.toMap.toJSDictionary;
            outputDataF map {
              case (outputData, cd) =>
                Roll20.setAttrs(outputData.toJSDictionary, SetterOptions.silent(true), () => {
                  p.success(cd); ()
                })
            };
            ()
          });
        }
        case coc: ChainedOpChain => {
          val emptyAcc: Future[ChainingDecision] = Future.successful(ExecuteChain);
          val f = coc.operations.foldLeft(emptyAcc) {
            case (f, op) => f flatMap {
              case ExecuteChain => forAllRows(section, op.lift())
              case SkipChain    => Future.successful(SkipChain)
            }
          };
          p.completeWith(f);
        }
      };
      ()
    });
    p.future
  }

  def getAttrs(fields: Set[FieldLike[_]]): Future[AttributeValues] = {
    val attrNames: js.Array[String] = fields.map(_.accessor).toJSArray;
    val p = Promise[AttributeValues]();
    Roll20.getAttrs(attrNames, (values: js.Dictionary[Any]) => {
      p.success(DataAttributeValues(values.toMap)); ()
    });
    p.future
  }

  def getAttr[T](field: FieldLike[T]): Future[Option[T]] = {
    getAttrs(Set(field)).map(attr => attr(field))
  }

  def setAttr[T](field: FieldLike[T], value: T, silent: Boolean = true): Future[Unit] = {
    setAttrs(Map(field.asInstanceOf[FieldLike[Any]] -> value), silent)
  }

  def setAttrs(values: Map[FieldLike[Any], Any], silent: Boolean = true): Future[Unit] = {
    val valuesWithNames = values.map({
      case (f, v) => (f.accessor -> serialise(f, v))
    }).toJSDictionary;
    val p = Promise[Unit]();
    log(s"Setting attrs: ${valuesWithNames.mkString}");
    Roll20.setAttrs(valuesWithNames, SetterOptions.silent(silent), () => {
      p.success (); ()
    });
    //Roll20.setAttrs(valuesWithNames);
    p.future
  }

  def on(trigger: String, callback: Function1[Roll20.EventInfo, Unit]): Unit = {
    subscriptions.addBinding(trigger, callback);
  }

  def onChange[T](field: FieldLike[T], callback: Function1[Roll20.EventInfo, Unit]): Unit = {
    on(s"change:${field.selector}", callback);
  }

  def onChange(section: RepeatingSection, callback: Function1[Roll20.EventInfo, Unit]): Unit = {
    on(s"change:${section.selector}", callback);
  }

  def onRemove(section: RepeatingSection, callback: Function1[Roll20.EventInfo, Unit]): Unit = {
    on(s"remove:${section.selector}", callback);
  }

  def updateOnChange[T](field: FieldLike[T], mapper: T => T): Unit = {
    val callback = (info: Roll20.EventInfo) => {
      val newValOF = getAttr(field).map(oldValO => oldValO.map(mapper));
      newValOF.onComplete {
        case Success(Some(newVal)) => setAttr(field, newVal);
        case _                     => log(s"There was an issue updating ${field.attr}!")
      }
    };
    onChange(field, callback);
  }

  def onOpen(callback: Function1[Roll20.EventInfo, Unit]): Unit = {
    on("sheet:opened", callback);
  }

  def onOpen(callback: => Unit): Unit = {
    val f = callback _;
    onOpen(_ => f());
  }

  implicit class FieldAssignable[T](f: FieldLike[T]) {
    def <<=(v: T): (FieldLike[Any], Any) = (f -> v).asInstanceOf[(FieldLike[Any], Any)];
  }

  def bind[T](partialOp: FieldOps[T]): FieldOpsWithCompleter[T] with FieldOpsWithChain[T] = {
    partialOp match {
      case partial: SheetWorkerOpPartial[T] => new SheetWorkerBinding(partial)
      case x                                => throw new java.lang.IllegalArgumentException(x.toString());
    }
  }

  // override things Roll20 dereferences for sheet workers
  @deprecated("Roll20 does not allow this in sheet workers!", "0.1")
  class Worker {
    throw new java.lang.SecurityException("Roll20 does not allow this in sheet workers!");
  }
  @deprecated("Roll20 does not allow this in sheet workers!", "0.1")
  def addEventListener = throw new java.lang.SecurityException("Roll20 does not allow this in sheet workers!");
  @deprecated("Roll20 does not allow this in sheet workers!", "0.1")
  def removeEventListener = throw new java.lang.SecurityException("Roll20 does not allow this in sheet workers!");
  @deprecated("Roll20 does not allow this in sheet workers!", "0.1")
  def importScripts = throw new java.lang.SecurityException("Roll20 does not allow this in sheet workers!");
  @deprecated("Roll20 does not allow this in sheet workers!", "0.1")
  class XMLHttpRequest {
    throw new java.lang.SecurityException("Roll20 does not allow this in sheet workers!");
  }
  @deprecated("Roll20 does not allow this in sheet workers!", "0.1")
  def postMessage = throw new java.lang.SecurityException("Roll20 does not allow this in sheet workers!");
  @deprecated("Roll20 does not allow this in sheet workers!", "0.1")
  def attachEvent = throw new java.lang.SecurityException("Roll20 does not allow this in sheet workers!");
  @deprecated("Roll20 does not allow this in sheet workers!", "0.1")
  def detachEvent = throw new java.lang.SecurityException("Roll20 does not allow this in sheet workers!");
  @deprecated("Roll20 does not allow this in sheet workers!", "0.1")
  class ActiveXObject {
    throw new java.lang.SecurityException("Roll20 does not allow this in sheet workers!");
  }

  object SourceType {
    val player: String = "player";
    val sheetworker: String = "sheetworker";
  }

  object SetterOptions {
    val silentTrue = dynLiteral(silent = true);
    val silentFalse = dynLiteral(silent = false);
    def silent(option: Boolean) = if (option) silentTrue else silentFalse;
  }

  def getTranslationByKey(key: String): Option[String] = {
    Roll20.getTranslationByKey(key).asInstanceOf[Any] match {
      case false     => None
      case s: String => Some(s)
    }
  }

  def getTranslationLanguage(): String = Roll20.getTranslationLanguage();

}
