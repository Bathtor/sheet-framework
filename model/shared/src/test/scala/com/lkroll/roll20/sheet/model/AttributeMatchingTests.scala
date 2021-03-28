package com.lkroll.roll20.sheet.model

import org.scalatest._
import org.scalatest.funsuite._
import org.scalatest.matchers.should.Matchers

class AttributeMatchingTests extends AnyFunSuite with Matchers {

  val attrs = List("repeating_testsec_-KkWmmPeaGP87vaZLpkt_testsec_testf",
                   "repeating_testsec_-uKudbakP63yteZLerta_testsec_testf"
  );

  test("Field should match any repeating id") {
    val res = attrs.filter(TestSection.test.nameMatcher);
    res should have size (2);
  }

  test("Field+rowId should match a single row") {
    val res = attrs.filter(TestSection.test.nameMatcherRow("-KkWmmPeaGP87vaZLpkt"));
    res should have size (1);
  }
}

object TestSection extends RepeatingSection with FieldImplicits {

  implicit val ctx = this.renderingContext;

  override def name = "testsec";

  val test: Field[String] = "testf".default("empty");
}
