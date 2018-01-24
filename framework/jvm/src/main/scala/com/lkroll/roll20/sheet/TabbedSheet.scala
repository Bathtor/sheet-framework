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

import scalatags.Text.all._
import scalatags.stylesheet._
import java.io.File
import java.net.URL

trait TabbedSheet extends Sheet {

  import SheetImplicits._

  def hidden: Seq[SheetElement];
  def header: FieldGroup;
  def tabs: Seq[Tab];
  def footer: FieldGroup;

  def style(): StyleSheet;
  def colourScheme: ColourScheme;
  def externalStyles: List[URL] = List();
  def translation(): SheetI18N;
  def templates: List[RollTemplate] = List();

  def pageToggle = input(`type` := "hidden", TabbedStyle.pageToggle, name := "attr_tab", value := 0)

  val tabbedStyle = this.getClass.getClassLoader.getResource("WEB-INF/tabbed.css");

  override def render(): String = {
    val hiddenGroup = HiddenGroup(hidden);
    val tabBar = div(TabbedStyle.nav, tabs.map(_.selector), div(
      TabbedStyle.`mar-l-lg`,
      input(`type` := "checkbox", name := "attr_edit_mode", TabbedStyle.`toggle-edit-mode`),
      span(TabbedStyle.`toggle-edit-mode`, TabbedStyle.pictos, "p")));
    val pages = tabs.map(_.render());
    val sheetString = div(
      TabbedStyle.wrapper,
      hiddenGroup.render(),
      input(`type` := "checkbox", name := "attr_edit_mode", `class` := "sheet-toggle-edit-mode sheet-hidden"),
      header.render(),
      pageToggle,
      tabBar,
      pages,
      footer.render()).render;
    val templatesString = templates.map(_.render).mkString("\n");
    sheetString + "\n" + templatesString;
  }
  override def renderStyle(): String = {
    val tabStyle = tabs.map(_.css).mkString("\n");
    val es = (tabbedStyle :: externalStyles).map(styleURL => {
      val source = io.Source.fromURL(styleURL);
      val sourceString = try CSSUtil.processFile(source) finally source.close();
      val replacedString = colourScheme.replaceColoursInText(sourceString);
      replacedString
    });
    (es ++ (tabStyle :: List(TabbedStyle, style).map(_.styleSheetText))).mkString("\n")
  };
  override def renderTranslation(): String = translation.render;

  private var tabCount = 0;

  protected def tab(label: LabelsI18N, fg: FieldGroup): Tab = {
    val t = Tab(tabCount, label, fg.renderer(), fg.members());
    tabCount += 1;
    t
  }
}

case class Tab(id: Int, labeli18n: LabelsI18N, renderer: GroupRenderer, members: Seq[SheetElement]) extends FieldGroup {
  val sty = TabbedStyle;

  def selector = label(
    sty.pseudoButtonWrapper,
    input(`type` := "radio", name := "attr_tab", value := id),
    span(labeli18n.attrs));

  def css = s"input.${sty.pageToggle.name}:not([value='${id}']) ~ .sheet-tab${id} { display: none }";

  override def render(mode: RenderMode = RenderMode.Normal): Tag = div(cls := s"sheet-tab${id}", sty.tab,
    renderer.render(this, mode));
}

object TabbedStyle extends CascadingStyleSheet {
  initStyleSheet();
  override def customSheetName = Some("sheet"); // required by roll20
  val tab = cls();

  private def selectorSeq(s1: Selector, s2: Selector): Selector = new Selector(s1.built ++ Seq("+") ++ s2.built);

  val wrapper = cls(
    fontSize := "12px",
    minWidth := "24rem");

  val pictos = cls(
    fontFamily := "Pictos",
    fontSize := "13px");

  val `toggle-edit-mode` = cls(
    height := "20px");

  val `mar-l-lg` = cls(
    marginLeft := "10px");

  val edit = cls();
  val presentation = cls();

  val pseudoButtonWrapper = cls();

  //  val pseudoButtonWrapper = cls(
  //    cursor.pointer,
  //    display.`inline-block`,
  //    label(width.auto),
  //    input(display.none),
  //    selectorSeq(input.checked, span)(
  //      backgroundColor := "#51624b"),
  //    span(
  //      backgroundColor := "#a7a7a7",
  //      borderRadius := "3px",
  //      color := "#FFFFFF",
  //      overflow.hidden,
  //      padding := "0 3px",
  //      textAlign.center),
  //    margin := "0 1px");

  val pageToggle = cls();

  val hidden = cls(display.none);

  val nav = cls(
    position.relative,
    zIndex := 10,
    display := "-webkit-box",
    display := "-ms-flexbox",
    display.flex,
    //"-ms-flex-wrap" := "wrap",
    flexWrap.wrap);
}

case class HiddenGroup(members: Seq[SheetElement]) extends FieldGroup {
  override def renderer = HiddenRenderer;
}

object HiddenRenderer extends GroupRenderer {
  override def fieldRenderers: GroupRenderer.FieldRenderer = {
    case (f, _) => input(`type` := "hidden", name := f.name, value := f.initialValue)
  };

  override def fieldCombiner = { tags =>
    div(TabbedStyle.hidden, tags)
  };
}
