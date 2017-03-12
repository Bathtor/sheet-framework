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

import scala.reflect.ClassTag;

trait Readable[T] {
  def isInstanceOf(x: Any): Boolean;
  def read(x: String): Option[T]
}

object Readable {

  def toReadable[T: ClassTag](p: String => T): Readable[T] = new Readable[T] {

    def isInstanceOf(x: Any) = implicitly[ClassTag[T]].runtimeClass.isInstance(x);

    def read(x: String): Option[T] = try {
      Some(p(x))
    } catch {
      case e: Throwable => e.printStackTrace(); None
    }
  }

  def apply[A](implicit instance: Readable[A]): Readable[A] = instance

  //    /**
  //     * Extend the string object with a read function.
  //     */
  //    object ops {
  //      implicit class pp[T](s: String) {
  //
  //        /**
  //         * The type parameter should have an implcit Readable in scope. Use
  //         * implicitly to access it and call the read function
  //         */
  //        def read[T: Readable]= implicitly[Readable[T]].read(s)
  //      }
  //    }
  //  }
}

