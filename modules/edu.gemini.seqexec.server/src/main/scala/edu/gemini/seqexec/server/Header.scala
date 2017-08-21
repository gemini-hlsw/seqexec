// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.server

import edu.gemini.seqexec.model.dhs.ImageFileId
import edu.gemini.seqexec.server.DhsClient._

trait Header {
  def sendBefore(id: ImageFileId, inst: String): SeqAction[Unit]
  def sendAfter(id: ImageFileId, inst: String): SeqAction[Unit]
}

object Header {
  // Default values for FITS headers
  val IntDefault: Int = -9999
  val DoubleDefault: Double = -9999.0
  val StrDefault: String = "No Value"
  val BooleanDefault: Boolean = false

  def buildKeyword[A](get: SeqAction[A], name: String, f: (String, A) => DhsClient.Keyword[A]): KeywordBag => SeqAction[KeywordBag] =
    k => get.map(x => k.add(f(name, x)))
  def buildInt8(get: SeqAction[Byte], name: String ): KeywordBag => SeqAction[KeywordBag]       = buildKeyword(get, name, Int8Keyword)
  def buildInt16(get: SeqAction[Short], name: String ): KeywordBag => SeqAction[KeywordBag]     = buildKeyword(get, name, Int16Keyword)
  def buildInt32(get: SeqAction[Int], name: String ): KeywordBag => SeqAction[KeywordBag]       = buildKeyword(get, name, Int32Keyword)
  def buildFloat(get: SeqAction[Float], name: String ): KeywordBag => SeqAction[KeywordBag]     = buildKeyword(get, name, FloatKeyword)
  def buildDouble(get: SeqAction[Double], name: String ): KeywordBag => SeqAction[KeywordBag]   = buildKeyword(get, name, DoubleKeyword)
  def buildBoolean(get: SeqAction[Boolean], name: String ): KeywordBag => SeqAction[KeywordBag] = buildKeyword(get, name, BooleanKeyword)
  def buildString(get: SeqAction[String], name: String ): KeywordBag => SeqAction[KeywordBag]   = buildKeyword(get, name, StringKeyword)

  private def bundleKeywords(inst: String, ks: Seq[KeywordBag => SeqAction[KeywordBag]]): SeqAction[KeywordBag] = {
    val z = SeqAction(KeywordBag(StringKeyword("instrument", inst)))
    ks.foldLeft(z) { case (a,b) => a.flatMap(b) }
  }

  def sendKeywords(id: ImageFileId, inst: String, hs: DhsClient, b: Seq[KeywordBag => SeqAction[KeywordBag]]): SeqAction[Unit] = for {
    bag <- bundleKeywords(inst, b)
    _   <- hs.setKeywords(id, bag, finalFlag = false)
  } yield ()


  object Implicits {
    // A simple typeclass to encapsulate default values
    trait DefaultValue[A] {
      def default: A
    }
    implicit class DefaultValueOps[A](a: Option[A])(implicit d: DefaultValue[A]) {
      def orDefault: A = a.getOrElse(d.default)
    }
    implicit val IntDefaultValue = new DefaultValue[Int] {
      val default = IntDefault
    }
    implicit val DoubleDefaultValue = new DefaultValue[Double] {
      val default = DoubleDefault
    }
    implicit val StrDefaultValue = new DefaultValue[String] {
      val default = StrDefault
    }

    implicit class A2SeqAction[A: DefaultValue](val v: Option[A]) {
      def toSeqAction: SeqAction[A] = SeqAction(v.orDefault)
    }

    implicit class SeqActionOption2SeqAction[A: DefaultValue](val v: SeqAction[Option[A]]) {
      def orDefault: SeqAction[A] = v.map(_.orDefault)
    }
  }
}
