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
//import scalatags.generic._
import collection.mutable
import com.lkroll.roll20.core._

trait SheetI18N extends Renderable {
  val entries = mutable.Map.empty[String, String];

  def text(key: String, default: String): DataLabel = {
    assert(!entries.contains(key));
    entries += (key -> default);
    DataLabel(key, default)
  }

  def abbr(abbrKey: String, abbrDefault: String, fullKey: String, fullDefault: String): Abbreviation = {
    val abbrL = text(abbrKey, abbrDefault);
    val fullL = text(fullKey, fullDefault);
    Abbreviation(abbrL, fullL.title)
  }

  def enum(prefix: String, options: Map[String, String]): OptionLabel = {
    val opts = options.map {
      case (key, default) => key -> text(s"${prefix}-$key", default)
    };
    return new OptionLabel(opts);
  }

  // TODO dynamic key replacement

  override def render(): String = {
    import org.codehaus.jettison.json.JSONObject;
    val sortedEntries = scala.collection.immutable.TreeMap(entries.toArray: _*)
    sortedEntries.map {
      case (k, v) => s"""\"$k\":${JSONObject.quote(v)}"""
    }.mkString("{", ",\n   ", "}")
  }

  def ++(other: SheetI18N): SheetI18NList = other match {
    case SheetI18NList(trs) => SheetI18NList(this :: trs)
    case _                  => SheetI18NList(List(this, other))
  }
}

object SheetI18N {
  val datai18n = attr("data-i18n");
  val datai18nTitle = attr("data-i18n-title");
  val datai18nAlt = attr("data-i18n-alt");
  val datai18nAriaLabel = attr("data-i18n-aria-label");
  val datai18nLabel = attr("data-i18n-label");
  val datai18nPlaceholder = attr("data-i18n-placeholder");
  private[sheet] val strAttr = genericAttr[String];
  val datai18nDynamic = attr("data-i18n-dynamic").empty;
}

case class SheetI18NList(translations: List[SheetI18N]) extends SheetI18N {
  override def render(): String = {
    import org.codehaus.jettison.json.JSONObject;
    val sortedEntries = translations.foldLeft(scala.collection.immutable.TreeMap.empty[String, String]) {
      case (acc, t) => acc ++ t.entries
    };
    //= scala.collection.immutable.TreeMap(entries.toArray: _*)
    sortedEntries.map {
      case (k, v) => s"""\"$k\":${JSONObject.quote(v)}"""
    }.mkString("{", ",\n   ", "}")
  }
  override def ++(other: SheetI18N): SheetI18NList = other match {
    case SheetI18NList(trs) => SheetI18NList(this.translations ++ trs)
    case _                  => SheetI18NList(this.translations ++ List(other))
  }
}

import SheetI18N._

sealed trait LabelsI18N {
  def attrs: Seq[AttrPair];
  def labels: Seq[LabelI18N]
  def ++(other: LabelsI18N): LabelSeq = LabelSeq(this.labels ++ other.labels);
}

case class LabelSeq(labels: Seq[LabelI18N]) extends LabelsI18N {
  override def attrs: Seq[AttrPair] = labels.flatMap(_.attrs);
}

class OptionLabel(options: Map[String, LabelI18N]) extends LabelsI18N {

  def labels: Seq[LabelI18N] = options.values.toSeq;
  override def attrs: Seq[AttrPair] = labels.flatMap(_.attrs);
  def apply(option: String): LabelI18N = options(option);
}

case class Abbreviation(abbrLabel: LabelI18N, fullLabel: LabelI18N) extends LabelsI18N {

  override def labels: Seq[LabelI18N] = Seq(abbrLabel, fullLabel);
  override def attrs: Seq[AttrPair] = labels.flatMap(_.attrs);

  def title = Abbreviation(abbrLabel, fullLabel.title);
  def alt = Abbreviation(abbrLabel, fullLabel.alt);
  def aria = Abbreviation(abbrLabel, fullLabel.aria);
  def label = Abbreviation(abbrLabel, fullLabel.label);
  def placeholder = Abbreviation(abbrLabel, fullLabel.placeholder);
}

sealed trait LabelI18N extends LabelsI18N {
  def key: String;
  def default: String;
  def dataAttr: Attr;

  def attr: AttrPair = scalatags.generic.AttrPair(dataAttr, key, strAttr);
  override def attrs: Seq[AttrPair] = Seq(attr);
  override def labels: Seq[LabelI18N] = Seq(this);

  def text = DataLabel(key, default);
  def title = TitleLabel(key, default);
  def alt = AltLabel(key, default);
  def aria = AriaLabel(key, default);
  def label = LabelLabel(key, default);
  def placeholder = PlaceholderLabel(key, default);
  def dynamic = DynamicLabel(key);
}

case class DynamicLabel(key: String) extends Renderable {
  override def render: String = s"^{${key}}";
}

case class DataLabel(key: String, default: String) extends LabelI18N {
  override def dataAttr = datai18n;
}

case class TitleLabel(key: String, default: String) extends LabelI18N {
  override def dataAttr = datai18nTitle;
}

case class AltLabel(key: String, default: String) extends LabelI18N {
  override def dataAttr = datai18nAlt;
}

case class AriaLabel(key: String, default: String) extends LabelI18N {
  override def dataAttr = datai18nAriaLabel;
}

case class LabelLabel(key: String, default: String) extends LabelI18N {
  override def dataAttr = datai18nLabel;
}

case class PlaceholderLabel(key: String, default: String) extends LabelI18N {
  override def dataAttr = datai18nPlaceholder;
}
