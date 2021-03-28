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
import org.scalatest.funsuite._
import org.scalatest.matchers.should.Matchers
import util.{Failure, Success}

class VersionTest extends AnyFunSuite with Matchers {
  test("Simple version parsing") {
    val s = "1.2.3";
    SemanticVersion.fromString(s) match {
      case Success(v) => {
        v.major should be(1);
        v.minor should be(2);
        v.patch should be(3);
        v.snapshot should be(false);
      }
      case Failure(e) => fail(e)
    }
  }

  test("Version with broken snapshot parsing") {
    val s = "1.2.3-SNAP";
    SemanticVersion.fromString(s) match {
      case Success(v) => {
        fail("-SNAP is not a valid semantic version!")
      }
      case Failure(_) => succeed
    }
  }

  test("Version with snapshot parsing") {
    val s = "1.2.3-SNAPSHOT";
    SemanticVersion.fromString(s) match {
      case Success(v) => {
        v.major should be(1);
        v.minor should be(2);
        v.patch should be(3);
        v.snapshot should be(true);
      }
      case Failure(e) => fail(e)
    }
  }
}
