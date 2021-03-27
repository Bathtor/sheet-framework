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

object TestPaletteData {
  val data = <palette>
               <url>http://paletton.com/#uid=72P0u0kllllaFw0g0qFqFg0w0aF</url>
               <colorset id="primary" title="Primary color">
                 <color id="primary-0" nr="0" rgb="2D882D" r="45" g="136" b="45" r0="0.176" g0="0.533" b0="0.176"/>
                 <color id="primary-1" nr="1" rgb="88CC88" r="136" g="204" b="136" r0="0.533" g0="0.8" b0="0.533"/>
                 <color id="primary-2" nr="2" rgb="55AA55" r="85" g="170" b="85" r0="0.333" g0="0.667" b0="0.333"/>
                 <color id="primary-3" nr="3" rgb="116611" r="17" g="102" b="17" r0="0.067" g0="0.4" b0="0.067"/>
                 <color id="primary-4" nr="4" rgb="004400" r="0" g="68" b="0" r0="0" g0="0.267" b0="0"/>
               </colorset>
               <colorset id="secondary-1" title="Secondary color (1)">
                 <color id="secondary-1-0" nr="0" rgb="226666" r="34" g="102" b="102" r0="0.133" g0="0.4" b0="0.4"/>
                 <color id="secondary-1-1" nr="1" rgb="669999" r="102" g="153" b="153" r0="0.4" g0="0.6" b0="0.6"/>
                 <color id="secondary-1-2" nr="2" rgb="407F7F" r="64" g="127" b="127" r0="0.251" g0="0.498" b0="0.498"/>
                 <color id="secondary-1-3" nr="3" rgb="0D4D4D" r="13" g="77" b="77" r0="0.051" g0="0.302" b0="0.302"/>
                 <color id="secondary-1-4" nr="4" rgb="003333" r="0" g="51" b="51" r0="0" g0="0.2" b0="0.2"/>
               </colorset>
               <colorset id="secondary-2" title="Secondary color (2)">
                 <color id="secondary-2-0" nr="0" rgb="AA6C39" r="170" g="108" b="57" r0="0.667" g0="0.424" b0="0.224"/>
                 <color id="secondary-2-1" nr="1" rgb="FFD1AA" r="255" g="209" b="170" r0="1" g0="0.82" b0="0.667"/>
                 <color id="secondary-2-2" nr="2" rgb="D49A6A" r="212" g="154" b="106" r0="0.831" g0="0.604" b0="0.416"/>
                 <color id="secondary-2-3" nr="3" rgb="804515" r="128" g="69" b="21" r0="0.502" g0="0.271" b0="0.082"/>
                 <color id="secondary-2-4" nr="4" rgb="552600" r="85" g="38" b="0" r0="0.333" g0="0.149" b0="0"/>
               </colorset>
               <colorset id="complement" title="Complement color">
                 <color id="complement-0" nr="0" rgb="AA3939" r="170" g="57" b="57" r0="0.667" g0="0.224" b0="0.224"/>
                 <color id="complement-1" nr="1" rgb="FFAAAA" r="255" g="170" b="170" r0="1" g0="0.667" b0="0.667"/>
                 <color id="complement-2" nr="2" rgb="D46A6A" r="212" g="106" b="106" r0="0.831" g0="0.416" b0="0.416"/>
                 <color id="complement-3" nr="3" rgb="801515" r="128" g="21" b="21" r0="0.502" g0="0.082" b0="0.082"/>
                 <color id="complement-4" nr="4" rgb="550000" r="85" g="0" b="0" r0="0.333" g0="0" b0="0"/>
               </colorset>
             </palette>

}

object TestPalette extends XMLColorPalette(TestPaletteData.data) {

}

class ColourTest extends AnyFunSuite with Matchers {
  test("Colours are right") {
    TestPalette.primaryShade0.css shouldBe "rgb(45, 136, 45)";
    TestPalette.complementShade3.css shouldBe "rgb(128, 21, 21)";
    val replaced = TestPalette.replaceColoursInText("""
a {
  background-color: ${complement-0};
}
""");
    println(replaced);
    replaced should include ("rgb");
  }
}
