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

package com.lkroll.roll20.sheet.tabbed

import com.lkroll.roll20.sheet.model.{APIOutputTemplate, SheetModel}
import com.lkroll.roll20.sheet.model.tabbed.TabbedI18N
import java.net.URL
import com.lkroll.roll20.sheet._
import com.lkroll.roll20.sheet.stylesheet._
import com.lkroll.roll20.sheet.stylesheet.ColourScheme

trait TabbedStyleColours { this: ColourScheme =>
  def textShadow: Colour
  def toggleSpanText: Colour
  def toggleInputText: Colour
  def editModeBackground: Colour
  def buttonWrapper: Colour
  def buttonWrapperBackground: Colour
  def checkedButtonWrapperBackground: Colour
  def overlayBackground: Colour
  def overlayText: Colour
}

trait TabbedSheet extends Sheet {
  import scalatags.Text.all._

  import SheetImplicits._

  def hidden: Seq[SheetElement];
  def header: FieldGroup;
  def tabs: Seq[Tab];
  def footer: FieldGroup;

  def style: SheetStyleSheet;
  def lightColourScheme: ColourScheme with TabbedStyleColours;
  def darkColourScheme: ColourScheme with TabbedStyleColours;
  def fontImports: List[String] = Nil;
  def translation: SheetI18NDefaults;
  def templates: List[RollTemplate] = Nil;

  def pageToggle =
    input(`type` := "hidden", TabbedStyleClasses.pageToggle, name := "attr_tab", value := 0)

  val tt = TabbedI18NDefaults;
  val model = TabbedModel;

  val modOverlay = div(
    TabbedStyleClasses.modoverlay,
    p(
      span(tt.processing),
      span(raw(" ")),
      input(
        `type` := "hidden",
        name := model.processingCount.name,
        value := model.processingCount.initialValue),
      span(name := model.processingCount.name)
    ),
    p(tt.doNotClose),
    p(
      label(
        TabbedStyleClasses.pseudoButtonWrapper,
        input(
          `type` := "checkbox",
          name := model.closeOverlay.name,
          value := model.closeOverlay.initialValue,
          checked := "checked"),
        span(tt.closeOverlay)
      )
    )
  );

  override def render: String = {
    val hiddenGroup = HiddenGroup(hidden);
    val tabBar = div(
      TabbedStyleClasses.nav,
      tabs.map(_.selector),
      div(
        TabbedStyleClasses.marLLg,
        input(`type` := "checkbox", name := "attr_edit_mode", TabbedStyleClasses.toggleEditMode),
        span(TabbedStyleClasses.toggleEditMode, TabbedStyleClasses.pictos, "p")
      )
    );
    val pages = tabs.map(_.render());
    val sheetString = div(
      TabbedStyleClasses.wrapper,
      input(
        `type` := "hidden",
        name := model.showOverlay.name,
        value := model.showOverlay.initialValue,
        TabbedStyleClasses.showModoverlay),
      modOverlay,
      hiddenGroup.render(),
      input(
        `type` := "checkbox",
        name := "attr_edit_mode",
        `class` := "sheet-toggle-edit-mode sheet-hidden"),
      header.render(),
      pageToggle,
      tabBar,
      pages,
      footer.render()
    ).render;
    val templatesString = templates.map(_.render).mkString("\n");
    sheetString + "\n" + templatesString;
  }
  override def renderStyle(): String = {
    val familyString = fontImports.map(_.replaceAll(" ", "+")).mkString("|")
    val fontImportStatement =
      s"@import url('https://fonts.googleapis.com/css?family=$familyString&display=swap');"
    val tabStyle = tabs.map(_.css).mkString("\n");
    val tabbedStyleRules = TabbedStyleRules(cDark = darkColourScheme, cLight = lightColourScheme);
    val tabStyleText = tabbedStyleRules.render;
    val childStyleText = style.render;
    List(
      fontImportStatement,
      "/****** Begin TabbedStyle ******/",
      tabStyle,
      tabStyleText,
      "/****** End TabbedStyle ******/",
      childStyleText).mkString("\n")
  };
  override def renderTranslation(): String = (TabbedI18NDefaults ++ translation).render;

  private var tabCount = 0;

  protected def tab(label: LabelsI18N, fg: FieldGroup): Tab = {
    val t = Tab(tabCount, label, fg.renderer, fg.members);
    tabCount += 1;
    t
  }
}

case class Tab(id: Int, labeli18n: LabelsI18N, renderer: GroupRenderer, members: Seq[SheetElement])
  extends FieldGroup {
  import scalatags.Text.all._

  val sty = TabbedStyleClasses;

  def selector =
    label(
      sty.pseudoButtonWrapper,
      input(`type` := "radio", name := "attr_tab", value := id),
      span(labeli18n.attrs));

  def css = s"""input${sty.pageToggle.render}:not([value="$id"]) ~ .tab$id { display: none }""";

  override def render(mode: RenderMode = RenderMode.Normal): Tag =
    div(cls := s"tab${id}", sty.tab, renderer.render(this, mode));
}

object TabbedModel extends SheetModel {
  override def version: String = "";
  override def outputTemplate: Option[APIOutputTemplate] = None;
}

object TabbedI18NDefaults extends SheetI18NDefaults {
  val keys = TabbedI18N;
  val processing = keys.processing <~ "Processing";
  val doNotClose = keys.doNotClose <~ "Do not close the sheet while its processing.";
  val closeOverlay = keys.closeOverlay <~ "Close";
}
// Keep the classes and rules separate, so we can depend on the colour schemes for the rules.
object TabbedStyleClasses extends SheetStyleSheet {
  val edit = cls("edit");
  val hidden = cls("hidden");
  val marLLg = cls("mar-l-lg");
  val modoverlay = cls("modoverlay");
  val nav = cls("nav");
  val pageToggle = cls("page-toggle");
  val pictos = cls("pictos");
  val presentation = cls("presentation");
  val pseudoButtonWrapper = cls("pseudo-button-wrapper");
  val repcontrol = cls("repcontrol");
  val showModoverlay = cls("show-modoverlay");
  val tab = cls("tab");
  val toggleEditMode = cls("toggle-edit-mode");
  val visibilityHidden = cls("visibility-hidden");
  val visibilityHiddenInPresentation = cls("visibility-hidden-in-presentation");
  val wrapper = cls("wrapper");
}
case class TabbedStyleRules(
    cDark: ColourScheme with TabbedStyleColours,
    cLight: ColourScheme with TabbedStyleColours)
  extends SheetStyleSheet {
  import scalatags.Text.tags._
  import TabbedStyleClasses._

  private def dualMode(f: ColourScheme with TabbedStyleColours => Colour): ColourDualModeValue = {
    dualMode(dark = f(cDark), light = f(cLight))
  }

  wrapper {
    fontSize :- 12.px;
    minWidth :- 24.rem;
    position :- "relative";
  }

  pictos {
    fontFamily :- "Pictos";
    fontSize :- 1.3.rem;
  }

  toggleEditMode {
    height :- 2.rem;
  }

  marLLg {
    margin.left :- 1.rem;
  }

  hidden {
    display :- "none";
  }

  nav {
    position :- "relative";
    zIndex :- 10;
    display :- "-webkit-box";
    display :- "-ms-flexbox";
    display :- "flex";
    flexWrap :- "wrap";
  }

  (visibilityHidden | (input & toggleEditMode.not(
    ANY.checked) ~ div / visibilityHiddenInPresentation)) {
    visibility :- "hidden!important";
  }

  ((input & toggleEditMode.not(ANY.checked) ~ div / repcontrol) |
    (input & toggleEditMode.not(ANY.checked) ~ div / edit) |
    (input & toggleEditMode.checked ~ div / presentation)) {
    display :- "none";
  }

  (input & toggleEditMode.checked + span & toggleEditMode) {
    textShadow :- dualMode(
      dark =
        s"-1px 0 ${cDark.textShadow.css}, 0 1px ${cDark.textShadow.css}, 1px 0 ${cDark.textShadow.css}, 0 -1px ${cDark.textShadow.css}",
      light =
        s"-1px 0 ${cLight.textShadow.css}, 0 1px ${cLight.textShadow.css}, 1px 0 ${cLight.textShadow.css}, 0 -1px ${cLight.textShadow.css}"
    );
  }

  (input & toggleEditMode) {
    margin.top :- 0;
  }

  (span & toggleEditMode) {
    textShadow :- dualMode(
      dark =
        s"-1px 0 ${cDark.textShadow.css}, 0 1px ${cDark.textShadow.css}, 1px 0 ${cDark.textShadow.css}, 0 -1px ${cDark.textShadow.css}",
      light =
        s"-1px 0 ${cLight.textShadow.css}, 0 1px ${cLight.textShadow.css}, 1px 0 ${cLight.textShadow.css}, 0 -1px ${cLight.textShadow.css}"
    );
    backgroundColor :- Transparent;
    color :- dualMode(_.toggleSpanText);
    fontSize :- 1.5.rem;
    margin.top :- 0;
    margin.left :- -2.3.rem;
    width :- 2.3.rem;
    height :- 2.rem;
    cursor :- "pointer";
    border.radius :- 3.px;
    display :- "inline-block";
    overflow :- "hidden";
    position :- "relative";
    textAlign :- "center";
    verticalAlign :- "top";
  }

  (input & toggleEditMode) {
    cursor :- "pointer";
    opacity :- 0;
    position :- "relative";
    zIndex :- 200;
    verticalAlign :- "top";
    height :- 2.rem;
    backgroundColor :- Transparent;
    color :- dualMode(_.toggleInputText);
    fontSize :- 1.2.rem;
    padding :- 0;
    margin.top :- -0.6.rem;
    width :- 2.3.rem;
  }

  ((input & toggleEditMode.checked ~ div / input) |
    (input & toggleEditMode.checked ~ div / select) |
    (input & toggleEditMode.checked ~ div / textarea)) {
    backgroundColor :- dualMode(_.editModeBackground);
  }

  pseudoButtonWrapper {
    cursor :- "pointer";
    display :- "inline-block";
    margin.top :- 0;
    margin.bottom :- 0;
    margin.left :- 1.px;
    margin.right :- 1.px;
  }

  (label & pseudoButtonWrapper) {
    width :- "auto";
  }

  (pseudoButtonWrapper / input) {
    display :- "none";
  }

  (pseudoButtonWrapper / input.checked + span) {
    backgroundColor :- dualMode(_.checkedButtonWrapperBackground);
  }

  (pseudoButtonWrapper / span) {
    backgroundColor :- dualMode(_.buttonWrapperBackground);
    border.radius :- 3.px;
    color :- dualMode(_.buttonWrapper);
    overflow :- "hidden";
    padding.top :- 0;
    padding.bottom :- 0;
    padding.left :- 3.px;
    padding.right :- 3.px;
    textAlign :- "center";
  }

  (showModoverlay.not(ANY.attributeEquals("value", "0")) ~ modoverlay) {
    display :- "block";
  }

  modoverlay {
    backgroundColor :- dualMode(_.overlayBackground);
    color :- dualMode(_.overlayText);
    display :- "none";
    height :- 100.perc;
    left :- 0;
    padding :- 2.px;
    position :- "absolute";
    top :- 0;
    width :- 100.perc;
    zIndex :- 99999;
    textAlign :- "center";
  }
}

case class HiddenGroup(members: Seq[SheetElement]) extends FieldGroup {
  override def renderer = HiddenRenderer;
}

object HiddenRenderer extends GroupRenderer {
  import scalatags.Text.all._

  override def fieldRenderers: GroupRenderer.FieldRenderer = { case (f, _) =>
    input(`type` := "hidden", name := f.name, value := f.initialValue)
  };

  override def fieldCombiner = { tags =>
    div(TabbedStyleClasses.hidden, tags)
  };
}
