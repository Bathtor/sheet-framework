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

import com.lkroll.roll20.core._
import com.lkroll.roll20.sheet.model._

import collection.mutable
import util.{ Try, Success, Failure }

trait UpdateManager extends SheetWorker {
  def update(from: String, to: String): List[SheetWorkerOp];
  def updateUnversioned(version: String): List[SheetWorkerOp];

  def nameChange[T](oldField: Field[T], newField: Field[T]): SheetWorkerOp = {
    op(oldField) update {
      case (v) => Seq(newField <<= v, oldField <<= oldField.resetValue)
    }
  }
  def typeChange[I, O](oldField: Field[I], newField: Field[O])(mapper: I => O): SheetWorkerOp = {
    op(oldField) update {
      case (v) => Seq(newField <<= mapper(v))
    }
  }
}

case class SemanticVersion(major: Int, minor: Int, patch: Int, snapshot: Boolean) {
  /**
   * Per category version difference.
   *
   * Snapshot status is maintained if either version is a snapshot.
   */
  def -(other: SemanticVersion): SemanticVersion = {
    SemanticVersion(this.major - other.major, this.minor - other.minor, this.patch - other.patch, this.snapshot || other.snapshot)
  }
  def incMajor(): SemanticVersion = this.copy(major = this.major + 1, minor = 0, patch = 0);
  def incMinor(): SemanticVersion = this.copy(minor = this.minor + 1, patch = 0);
  def incPatch(): SemanticVersion = this.copy(patch = this.patch + 1);

  override def toString(): String = if (snapshot) s"$major.$minor.$patch=SNAPSHOT" else s"$major.$minor.$patch";
}

object SemanticVersion {
  val versionFormat = """(\d+)\.(\d+)\.(\d+)(-SNAPSHOT)?""".r;

  def fromString(s: String): Try[SemanticVersion] = {
    Try {
      s match {
        case versionFormat(majorS, minorS, patchS, snapS) => for {
          major <- Try(majorS.toInt);
          minor <- Try(minorS.toInt);
          patch <- Try(patchS.toInt);
          snap <- Try(if (snapS == null) false else true)
        } yield SemanticVersion(major, minor, patch, snap)
      }
    }.flatten
  }
}

trait MinorVersionUpdateManager extends UpdateManager {
  private val updates = mutable.Map.empty[Int, List[SheetWorkerOp]];

  def model: SheetModel;
  //def updateUnversioned(version: String): List[SheetWorkerOp];
  /**
   * Called on every version update operation with the new version.
   *
   * Return fields to be written.
   */
  def onEveryVersionUpdate(newVersion: String): Seq[(FieldLike[Any], Any)] = Seq(model.versionField <<= newVersion);

  override def update(from: String, to: String): List[SheetWorkerOp] = {
    log(s"Preparing version updates from $from to $to");
    val res = for {
      fromV <- SemanticVersion.fromString(from);
      toV <- SemanticVersion.fromString(to);
      fetch <- minorsToFetch(fromV, toV)
    } yield fetch.flatMap(lookup(_));
    res match {
      case Success(r) => {
        val vUpdate = nop update { _ =>
          onEveryVersionUpdate(to);
        };
        val l = r :+ vUpdate;
        debug(s"Collected ${l.size} updates.");
        l
      }
      case Failure(e) => error(e); List.empty
    }
  }
  private def minorsToFetch(fromV: SemanticVersion, toV: SemanticVersion): Try[List[Int]] = {
    val diff = toV - fromV;
    Try {
      assert(diff.major == 0, "Major version updates are not supported!");
      fromV.minor.until(toV.minor).toList
    }
  }
  private def lookup(minor: Int): List[SheetWorkerOp] = updates.getOrElse(minor, List.empty);
  private def setUpdate(minor: Int, ops: List[SheetWorkerOp]): Unit = updates += (minor -> ops);
  //  def +=(entry: (Int, SheetWorkerOp)): Unit = {
  //    val nl = updates.get(entry._1) match {
  //      case Some(l) => l :+ entry._2
  //      case None    => List(entry._2)
  //    };
  //    updates += (entry._1 -> nl);
  //  }
  //  def ++=(entry: (Int, List[SheetWorkerOp])): Unit = {
  //    val nl = updates.get(entry._1) match {
  //      case Some(l) => l ++ entry._2
  //      case None    => entry._2
  //    };
  //    updates += (entry._1 -> nl);
  //  }
  /**
   * Set all the updates for a particular version in a single invocation.
   *
   * Overrides previous entries for that version!
   */
  def forVersion(s: String)(updates: => List[SheetWorkerOp]): Unit = {
    val Success(v) = SemanticVersion.fromString(s);
    val newV = v.incMinor();
    val vUpdate = nop update { _ =>
      onEveryVersionUpdate(newV.toString());
    };
    setUpdate(v.minor, updates :+ vUpdate)
  }
}

