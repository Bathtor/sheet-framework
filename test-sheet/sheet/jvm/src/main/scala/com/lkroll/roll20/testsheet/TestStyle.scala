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

package com.lkroll.roll20.testsheet

import com.lkroll.roll20.sheet._
import com.lkroll.roll20.sheet.model._

import scalatags.Text.all._
import scalatags.stylesheet._

object TestStyle extends SheetStyle {
  initStyleSheet();

  val c = TestPalette;

  val inlineNumber = cls(display.`inline-flex`);

  val inlineContentGroup =
    cls(display.`inline-flex`, marginLeft := 2.px, marginRight := 2.px, marginTop := 2.px, whiteSpace.nowrap);

  val max3charinline = cls(maxWidth := "3em", display.`inline-flex`);

  val inlineLabel = cls(fontWeight.bold, marginRight := 2.px);

  val labelledValue = cls();

  val labelGroup = cls(
    borderBottomWidth := "1px",
    borderBottomColor := c.mediumGrey.css,
    padding := "0px",
    marginLeft := "5px",
    marginRight := "5px",
    display.block //    ,
    //    div(display.`inline-block`)
  );

  val subLabel = cls(
    backgroundColor := c.transp.css,
    textTransform.uppercase,
    textAlign.left,
    color := c.darkText.css,
    fontSize := "0.9rem",
    marginTop := "1px",
    marginBottom := "2px",
    paddingLeft := "2px",
    paddingRight := "2px",
    borderTopStyle.dashed,
    borderTopColor := c.darkGrey.css,
    borderTopWidth := "1px"
  );

  val largeText = cls(input(fontSize := "2.2rem", fontWeight.bold), span(fontSize := "2.2rem", fontWeight.bold));

  val aCenter = cls(textAlign.center);
  val aRight = cls(textAlign.right);

  val wrapBox = cls(backgroundColor := c.boxBackground.css,
                    border := "1px solid #000000",
                    borderRadius := "3px",
                    padding := "3px",
                    marginBottom := "5px");

  val headerBox = cls(width := "100%");

  val footerBox =
    cls(borderTopStyle.solid, borderTopWidth := "1px", borderTopColor := c.roll20border.css, width := "100%");

  val margin5px = cls(margin := "5px");

  val description = cls(fontSize := 0.9.em);

  val `two-line-textarea`, `eight-line-textarea` = cls();

  val `flex-grow` = cls();
  val `flex-container` = cls();
  val `flex-start`, `flex-centre`, `flex-end`, `flex-stretch`, `flex-header` = cls();
  val `flex-col` = cls();

  val `description-toggle` = cls();
  val `toggle-wrapper-label` = cls();

}

object TestPalette extends XMLColorPalette(TestColourData.data) {

  val roll20border = rgb("roll20-border", 204, 204, 204);
  val textShadow = hex("text-shadow", 0);
  val lightGrey = hex("ligh-grey", 0xc9c9c9);
  val mediumGrey = hex("medium-grey", 0x7c7c7c);
  val darkGrey = hex("dark-grey", 0x242424);
  val transp = transparent("transp");
  val sheetBackground = hex("sheet-background", 0xffffff);
  val boxBackground = hex("box-background", 0xfafafa);
  val lightText = hex("light-text", 0xfafafa);
  val darkText = hex("dark-text", 0x0c0c0c);
  val overlayBackground = rgba("overlay-background", 0, 16, 27, 0.75);

  val titleText = alias("title-text", primaryShade3);
  val toggleSpanText = alias("toggle-span-text", mediumGrey);
  val toggleInputText = alias("toggle-input-text", mediumGrey);
  val toggleCheckedText = alias("toggle-checked-text", complementShade2);
  val editModeBackground = alias("edit-mode-background", primaryShade1);
  val presentationModeBackground = alias("presentation-mode-background", sheetBackground);
  val defaultInputBackground = alias("input-background-colour", transp);
  val defaultInput = alias("input-colour", darkText);
  val checkedButtonWrapperBackground = alias("checked-button-wrapper-background", primaryShade0);
  val buttonWrapperBackground = alias("button-wrapper-background", mediumGrey);
  val buttonWrapper = alias("button-wrapper", lightText);
  val buttonHighlight = alias("button-highlight", complementShade2);
  val templateBackground = alias("template-background", darkGrey);
  val templateText = alias("template-text", lightText);
  val templateHeader = alias("template-header", primaryShade0);
  val templateHeaderWarn = alias("template-header-warn", complementShade0);
  val templateHeaderError = alias("template-header-error", secondaryShade0);
  val textHighlight = alias("text-highlight", primaryShade2);
  val rollHighlight = alias("roll-highlight", complementShade2);
  val rollHighlightOver = alias("roll-highlight-over", complementShade1);
  val rollHighlightShadow = alias("roll-highlight-shadow", complementShade3);
  val remplateSuccess = alias("template-success", tertiaryShade0);
  val remplateFailure = alias("template-failure", secondaryShade0);
  val classTag = alias("class-tag-colour", primaryShade3);
  val catTag = alias("cat-tag-colour", secondaryShade3);
  val overlayText = alias("overlay-text", lightText);
  val hyperefHighlight = alias("hyperref-highlight", primaryShade2);
  val hyperefHighlightOver = alias("hyperref-highlight-over", primaryShade1);
}

object TestColourData {
  val data: scala.xml.Node =
    <palette>
      <url>http://paletton.com/#uid=73v2g0kmglA2KLccmu8vfdUUa5g</url>
      <colorset id="primary" title="Primary color">
        <color id="primary-0" nr="0" rgb="26526E" r="38" g="82" b="110" r0="0.149" g0="0.322" b0="0.431"/>
        <color id="primary-1" nr="1" rgb="C1CBD2" r="193" g="203" b="210" r0="0.757" g0="0.796" b0="0.824"/>
        <color id="primary-2" nr="2" rgb="62849A" r="98" g="132" b="154" r0="0.384" g0="0.518" b0="0.604"/>
        <color id="primary-3" nr="3" rgb="062D47" r="6" g="45" b="71" r0="0.024" g0="0.176" b0="0.278"/>
        <color id="primary-4" nr="4" rgb="00101B" r="0" g="16" b="27" r0="0" g0="0.063" b0="0.106"/>
      </colorset>
      <colorset id="secondary-1" title="Secondary color (1)">
        <color id="secondary-1-0" nr="0" rgb="AC4734" r="172" g="71" b="52" r0="0.675" g0="0.278" b0="0.204"/>
        <color id="secondary-1-1" nr="1" rgb="FFEDE9" r="255" g="237" b="233" r0="1" g0="0.929" b0="0.914"/>
        <color id="secondary-1-2" nr="2" rgb="F0A294" r="240" g="162" b="148" r0="0.941" g0="0.635" b0="0.58"/>
        <color id="secondary-1-3" nr="3" rgb="6F1303" r="111" g="19" b="3" r0="0.435" g0="0.075" b0="0.012"/>
        <color id="secondary-1-4" nr="4" rgb="2A0700" r="42" g="7" b="0" r0="0.165" g0="0.027" b0="0"/>
      </colorset>
      <colorset id="secondary-2" title="Secondary color (2)">
        <color id="secondary-2-0" nr="0" rgb="267E44" r="38" g="126" b="68" r0="0.149" g0="0.494" b0="0.267"/>
        <color id="secondary-2-1" nr="1" rgb="CADDD1" r="202" g="221" b="209" r0="0.792" g0="0.867" b0="0.82"/>
        <color id="secondary-2-2" nr="2" rgb="6CB083" r="108" g="176" b="131" r0="0.424" g0="0.69" b0="0.514"/>
        <color id="secondary-2-3" nr="3" rgb="02511D" r="2" g="81" b="29" r0="0.008" g0="0.318" b0="0.114"/>
        <color id="secondary-2-4" nr="4" rgb="001F0A" r="0" g="31" b="10" r0="0" g0="0.122" b0="0.039"/>
      </colorset>
      <colorset id="complement" title="Complement color">
        <color id="complement-0" nr="0" rgb="AC7734" r="172" g="119" b="52" r0="0.675" g0="0.467" b0="0.204"/>
        <color id="complement-1" nr="1" rgb="FFF5E9" r="255" g="245" b="233" r0="1" g0="0.961" b0="0.914"/>
        <color id="complement-2" nr="2" rgb="F0C794" r="240" g="199" b="148" r0="0.941" g0="0.78" b0="0.58"/>
        <color id="complement-3" nr="3" rgb="6F3F03" r="111" g="63" b="3" r0="0.435" g0="0.247" b0="0.012"/>
        <color id="complement-4" nr="4" rgb="2A1700" r="42" g="23" b="0" r0="0.165" g0="0.09" b0="0"/>
      </colorset>
    </palette>;

}
