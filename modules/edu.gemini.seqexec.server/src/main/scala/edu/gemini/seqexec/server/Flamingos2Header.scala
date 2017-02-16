package edu.gemini.seqexec.server

import java.time.LocalDate
import java.time.format.DateTimeFormatter

import edu.gemini.seqexec.model.dhs.ObsId
import edu.gemini.seqexec.server.ConfigUtilOps._
import edu.gemini.spModel.gemini.flamingos2.Flamingos2.{MOS_PREIMAGING_PROP, READMODE_PROP, ReadMode}
import edu.gemini.spModel.config2.Config
import edu.gemini.spModel.data.YesNoType
import edu.gemini.spModel.seqcomp.SeqConfigNames.INSTRUMENT_KEY

import scalaz.EitherT
import scalaz.concurrent.Task

/**
  * Created by jluhrs on 2/10/17.
  */

class Flamingos2Header(hs: DhsClient, f2ObsReader: Flamingos2Header.ObsKeywordsReader, f2Reader: Flamingos2Header.InstKeywordsReader, tcsKeywordsReader: TcsKeywordsReader) extends Header {
  import Header._
  override def sendBefore(id: ObsId, inst: String): SeqAction[Unit] =  {

    sendKeywords(id, inst, hs, List(
      buildString(f2ObsReader.getReadMode.map{
        case ReadMode.BRIGHT_OBJECT_SPEC => "Bright"
        case ReadMode.MEDIUM_OBJECT_SPEC => "Medium"
        case ReadMode.FAINT_OBJECT_SPEC  => "Dark"
      }, "READMODE"),
      buildInt32(f2ObsReader.getReadMode.map{
        case ReadMode.BRIGHT_OBJECT_SPEC => 1
        case ReadMode.MEDIUM_OBJECT_SPEC => 4
        case ReadMode.FAINT_OBJECT_SPEC  => 8
      }, "NREADS"),
      buildBoolean(f2ObsReader.getPreimage.map(_.toBoolean), "PREIMAGE"),
      buildString(SeqAction(LocalDate.now.format(DateTimeFormatter.ISO_LOCAL_DATE)), "DATE-OBS"),
      buildString(tcsKeywordsReader.getUT, "TIME-OBS")
    ))
  }

  override def sendAfter(id: ObsId, inst: String): SeqAction[Unit] = SeqAction(())
}

object Flamingos2Header {

  def apply(hs: DhsClient, f2ObsReader: ObsKeywordsReader, f2Reader: InstKeywordsReader, tcsKeywordsReader: TcsKeywordsReader) = new Flamingos2Header(hs, f2ObsReader, f2Reader, tcsKeywordsReader)

  trait ObsKeywordsReader {
    def getPreimage: SeqAction[YesNoType]
    def getReadMode: SeqAction[ReadMode]
  }

  class ObsKeywordsReaderImpl(config: Config) extends ObsKeywordsReader {
    override def getPreimage: SeqAction[YesNoType] = EitherT(Task.now(config.extract(INSTRUMENT_KEY / MOS_PREIMAGING_PROP)
      .as[YesNoType].leftMap(e => SeqexecFailure.Unexpected(ConfigUtilOps.explain(e)))))

    override def getReadMode: SeqAction[ReadMode] = EitherT(Task.now(config.extract(INSTRUMENT_KEY / READMODE_PROP)
      .as[ReadMode].leftMap(e => SeqexecFailure.Unexpected(ConfigUtilOps.explain(e)))))
  }

  trait InstKeywordsReader {
    def getHealth: SeqAction[String]
    def getState: SeqAction[String]
  }

  object DummyInstKeywordReader extends InstKeywordsReader {
    override def getHealth: SeqAction[String] = SeqAction("GOOD")

    override def getState: SeqAction[String] = SeqAction("RUNNING")
  }

  object InstKeywordReaderImpl extends InstKeywordsReader {
    implicit def fromStringOption(v: Option[String]): SeqAction[String] = SeqAction(v.getOrElse("No Value"))

    override def getHealth: SeqAction[String] = Flamingos2Epics.instance.health

    override def getState: SeqAction[String] = Flamingos2Epics.instance.state
  }

}
