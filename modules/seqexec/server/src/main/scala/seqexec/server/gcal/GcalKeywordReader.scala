// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gcal

import cats.Eq
import seqexec.server.SeqAction
import seqexec.server.KeywordsReader._
import cats.implicits._
import edu.gemini.seqexec.server.gcal.BinaryOnOff

trait GcalKeywordReader {
  def getDiffuser: SeqAction[Option[String]]
  def getFilter: SeqAction[Option[String]]
  def getLamp: SeqAction[Option[String]]
  def getShutter: SeqAction[Option[String]]
}

object DummyGcalKeywordsReader extends GcalKeywordReader {

  def getDiffuser: SeqAction[Option[String]] = None

  def getFilter: SeqAction[Option[String]] = None

  def getLamp: SeqAction[Option[String]] = None

  def getShutter: SeqAction[Option[String]] = None
}

object GcalKeywordsReaderImpl extends GcalKeywordReader {
  implicit val eq: Eq[BinaryOnOff] = Eq.by(_.ordinal())

  def getDiffuser: SeqAction[Option[String]] = SeqAction(GcalEpics.instance.diffuser)

  def getFilter: SeqAction[Option[String]] = SeqAction(GcalEpics.instance.filter)

  def getLamp: SeqAction[Option[String]] = SeqAction{
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

  def getShutter: SeqAction[Option[String]] = SeqAction(GcalEpics.instance.shutter.map{
    case "OPEN"  => "OPEN"
    case "CLOSE" => "CLOSED"
    case _       => "INDEF"
  })
}
