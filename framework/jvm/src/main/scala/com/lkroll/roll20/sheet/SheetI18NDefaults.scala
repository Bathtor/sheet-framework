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

import collection.mutable
import scalatags.Text.all._
import com.lkroll.roll20.core.Renderable
import com.lkroll.roll20.sheet.model._

trait SheetI18NDefaults extends Renderable {
  def keys: SheetI18N;

  protected[sheet] val entries = mutable.Map.empty[String, String];

  def text(key: DataKey, default: String): DataLabel = {
    if (!entries.contains(key.key)) {
      entries += (key.key -> default);
    } else {
      Console.err.println(
        s"Entries already contains a ${key} -> ${entries(key.key)} mapping. Ignoring new value ${default} and returning new label."
      );
    }
    DataLabel(key)
  }

  def abbr(keys: AbbreviationKey, abbrDefault: String, fullDefault: String): Abbreviation = {
    val abbrL = text(DataKey(keys.abbrKey), abbrDefault);
    val fullL = text(DataKey(keys.fullKey), fullDefault);
    Abbreviation(abbrL, fullL.title)
  }

  def enum[T <: Enumeration](keys: OptionKey[T], defaults: T#Value => String): OptionLabel = {
    val opts = keys.options.map { case (enumval, key) =>
      enumval.toString -> text(DataKey(key), defaults(enumval))
    };
    return new OptionLabel(opts);
  }

  implicit class AugmentedDataKey(d: DataKey) {
    def <~(default: String): DataLabel = text(d, default);
  }

  implicit class AugmentedAbbreviationKey(a: AbbreviationKey) {
    def <~(abbrDefault: String, fullDefault: String): Abbreviation = abbr(a, abbrDefault, fullDefault);
  }

  implicit class AugmentedOptionKey[T <: Enumeration](o: OptionKey[T]) {
    def <~(defaults: T#Value => String): OptionLabel = enum(o, defaults);
  }

  def verify(): Unit = {
    var complete = true;
    keys.allKeys.foreach { k =>
      if (!entries.contains(k)) {
        Console.err.println(s"${this.getClass.getSimpleName} is missing entry for key '${k}'!");
        complete = false;
      }
    }
    assert(complete);
  }

  override def render(): String = {
    import org.codehaus.jettison.json.JSONObject;
    verify();
    val sortedEntries = scala.collection.immutable.TreeMap(entries.toArray: _*)
    sortedEntries
      .map { case (k, v) =>
        s"""\"$k\":${JSONObject.quote(v)}"""
      }
      .mkString("{", ",\n   ", "}")
  }

  def ++(other: SheetI18NDefaults): SheetI18NDefaultsList = other match {
    case SheetI18NDefaultsList(trs) => SheetI18NDefaultsList(this :: trs)
    case _                          => SheetI18NDefaultsList(List(this, other))
  }
}

case class SheetI18NDefaultsList(translations: List[SheetI18NDefaults]) extends SheetI18NDefaults {
  override def keys: SheetI18N = ???; // could write a combiner for this, but doesn't seem useful

  override def render(): String = {
    import org.codehaus.jettison.json.JSONObject;
    translations.foreach { t =>
      t.verify();
    }
    val sortedEntries = translations.foldLeft(scala.collection.immutable.TreeMap.empty[String, String]) {
      case (acc, t) => acc ++ t.entries
    };
    //= scala.collection.immutable.TreeMap(entries.toArray: _*)
    sortedEntries
      .map { case (k, v) =>
        s"""\"$k\":${JSONObject.quote(v)}"""
      }
      .mkString("{", ",\n   ", "}")
  }
  override def ++(other: SheetI18NDefaults): SheetI18NDefaultsList = other match {
    case SheetI18NDefaultsList(trs) => SheetI18NDefaultsList(this.translations ++ trs)
    case _                          => SheetI18NDefaultsList(this.translations ++ List(other))
  }
}

object SheetI18NAttrs {
  val datai18n = attr("data-i18n");
  val datai18nTitle = attr("data-i18n-title");
  val datai18nAlt = attr("data-i18n-alt");
  val datai18nAriaLabel = attr("data-i18n-aria-label");
  val datai18nLabel = attr("data-i18n-label");
  val datai18nPlaceholder = attr("data-i18n-placeholder");
  private[sheet] val strAttr = genericAttr[String];
  val datai18nDynamic = attr("data-i18n-dynamic").empty;
}

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
  def key: DataKey;
  def dataAttr: Attr;

  def attr: AttrPair = scalatags.generic.AttrPair(dataAttr, key.key, SheetI18NAttrs.strAttr);
  override def attrs: Seq[AttrPair] = Seq(attr);
  override def labels: Seq[LabelI18N] = Seq(this);

  def text = DataLabel(key);
  def title = TitleLabel(key);
  def alt = AltLabel(key);
  def aria = AriaLabel(key);
  def label = LabelLabel(key);
  def placeholder = PlaceholderLabel(key);
  def dynamic = DynamicLabel(key);
}

case class DataLabel(key: DataKey) extends LabelI18N {
  override def dataAttr = SheetI18NAttrs.datai18n;
}

case class TitleLabel(key: DataKey) extends LabelI18N {
  override def dataAttr = SheetI18NAttrs.datai18nTitle;
}

case class AltLabel(key: DataKey) extends LabelI18N {
  override def dataAttr = SheetI18NAttrs.datai18nAlt;
}

case class AriaLabel(key: DataKey) extends LabelI18N {
  override def dataAttr = SheetI18NAttrs.datai18nAriaLabel;
}

case class LabelLabel(key: DataKey) extends LabelI18N {
  override def dataAttr = SheetI18NAttrs.datai18nLabel;
}

case class PlaceholderLabel(key: DataKey) extends LabelI18N {
  override def dataAttr = SheetI18NAttrs.datai18nPlaceholder;
}
