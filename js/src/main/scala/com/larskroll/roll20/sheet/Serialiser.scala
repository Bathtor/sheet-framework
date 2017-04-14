package com.larskroll.roll20.sheet

import scalajs.js;

trait Serialiser[T] {
  def serialise(o: T): js.Any
}

object DefaultSerialiser extends Serialiser[Any] {
  override def serialise(o: Any): js.Any = o.asInstanceOf[js.Any];
}
