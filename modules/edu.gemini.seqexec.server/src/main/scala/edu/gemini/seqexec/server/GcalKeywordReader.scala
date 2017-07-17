package edu.gemini.seqexec.server

import edu.gemini.seqexec.server.gcal.BinaryOnOff

import scalaz._
import Scalaz._

trait GcalKeywordReader {
  def getDiffuser: SeqAction[Option[String]]
  def getFilter: SeqAction[Option[String]]
  def getLamp: SeqAction[Option[String]]
  def getShutter: SeqAction[Option[String]]
}

object DummyGcalKeywordsReader extends GcalKeywordReader {
  import KeywordsReader._

  def getDiffuser: SeqAction[Option[String]] = None

  def getFilter: SeqAction[Option[String]] = None

  def getLamp: SeqAction[Option[String]] = None

  def getShutter: SeqAction[Option[String]] = None
}

object GcalKeywordsReaderImpl extends GcalKeywordReader {
  import KeywordsReader._

  def getDiffuser: SeqAction[Option[String]] = GcalEpics.instance.diffuser

  def getFilter: SeqAction[Option[String]] = GcalEpics.instance.filter

  def getLamp: SeqAction[Option[String]] = {
    def ar   = GcalEpics.instance.lampAr().filter(_ == BinaryOnOff.ON) *> "Ar".some
    def cuAr = GcalEpics.instance.lampCuAr().filter(_ == BinaryOnOff.ON) *> "CuAr".some
    def ir   = GcalEpics.instance.lampIr().map{
      case BinaryOnOff.ON => "IRhigh"
      case _              => "IRlow"
    }
    def qh   = GcalEpics.instance.lampQH().filter(_ == BinaryOnOff.ON) *> "QH".some
    def thAr = GcalEpics.instance.lampThAr().filter(_ == BinaryOnOff.ON) *> "ThAr".some
    def xe   = GcalEpics.instance.lampXe().filter(_ == BinaryOnOff.ON) *> "Xe".some

    Stream(ar, xe, cuAr, thAr, qh, ir).flatten.headOption
  }

  def getShutter: SeqAction[Option[String]] = GcalEpics.instance.shutter.map{
    case "OPEN"  => "OPEN"
    case "CLOSE" => "CLOSED"
    case _       => "INDEF"
  }
}
