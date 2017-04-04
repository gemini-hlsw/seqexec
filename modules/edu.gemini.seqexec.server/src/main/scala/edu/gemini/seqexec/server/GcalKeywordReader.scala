package edu.gemini.seqexec.server

import edu.gemini.seqexec.server.tcs.BinaryOnOff

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
    val ar   = GcalEpics.instance.lampAr().filter(_ == BinaryOnOff.On) *> "Ar".some
    val cuAr = GcalEpics.instance.lampCuAr().filter(_ == BinaryOnOff.On) *> "CuAr".some
    val ir   = GcalEpics.instance.lampIr().filter(_ == BinaryOnOff.On) *> "IR".some
    val qh   = GcalEpics.instance.lampQH().filter(_ == BinaryOnOff.On) *> "QH".some
    val thAr = GcalEpics.instance.lampThAr().filter(_ == BinaryOnOff.On) *> "ThAr".some
    val xe   = GcalEpics.instance.lampXe().filter(_ == BinaryOnOff.On) *> "Xe".some

    // Order is important to preserve alphabetical order
    List(ar, cuAr, ir, qh, thAr, xe).flatten.mkString("+")
  }

  def getShutter: SeqAction[Option[String]] = GcalEpics.instance.shutter
}
