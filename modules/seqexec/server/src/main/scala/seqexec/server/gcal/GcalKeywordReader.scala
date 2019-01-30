// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gcal

import cats.Eq
import cats.effect.IO
import seqexec.server.SeqActionF
import seqexec.server.keywords._
import cats.implicits._
import edu.gemini.seqexec.server.gcal.BinaryOnOff

trait GcalKeywordReader[F[_]] {
  def getDiffuser: SeqActionF[F, Option[String]]
  def getFilter: SeqActionF[F, Option[String]]
  def getLamp: SeqActionF[F, Option[String]]
  def getShutter: SeqActionF[F, Option[String]]
}

object DummyGcalKeywordsReader extends GcalKeywordReader[IO] {

  def getDiffuser: SeqActionF[IO, Option[String]] = None.toSeqActionO

  def getFilter: SeqActionF[IO, Option[String]] = None.toSeqActionO

  def getLamp: SeqActionF[IO, Option[String]] = None.toSeqActionO

  def getShutter: SeqActionF[IO, Option[String]] = None.toSeqActionO
}

object GcalKeywordsReaderImpl extends GcalKeywordReader[IO] {
  implicit val eq: Eq[BinaryOnOff] = Eq.by(_.ordinal())

  def getDiffuser: SeqActionF[IO, Option[String]] = SeqActionF(GcalEpics.instance.diffuser)

  def getFilter: SeqActionF[IO, Option[String]] = SeqActionF(GcalEpics.instance.filter)

  def getLamp: SeqActionF[IO, Option[String]] = SeqActionF{
    val ar   = GcalEpics.instance.lampAr().filter(_ === BinaryOnOff.ON) *> "Ar".some
    val cuAr = GcalEpics.instance.lampCuAr().filter(_ === BinaryOnOff.ON) *> "CuAr".some
    val ir   = GcalEpics.instance.lampIr().map{
      case BinaryOnOff.ON => "IRhigh"
      case _              => "IRlow"
    }
    val qh   = GcalEpics.instance.lampQH().filter(_ === BinaryOnOff.ON) *> "QH".some
    val thAr = GcalEpics.instance.lampThAr().filter(_ === BinaryOnOff.ON) *> "ThAr".some
    val xe   = GcalEpics.instance.lampXe().filter(_ === BinaryOnOff.ON) *> "Xe".some

    ar.orElse(xe).orElse(cuAr).orElse(thAr).orElse(qh).orElse(ir)
  }

  def getShutter: SeqActionF[IO, Option[String]] = SeqActionF(GcalEpics.instance.shutter.map{
    case "OPEN"  => "OPEN"
    case "CLOSE" => "CLOSED"
    case _       => "INDEF"
  })
}
