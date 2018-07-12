// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.flamingos2

import cats.data.EitherT
import cats.effect.IO
import cats.implicits._
import gem.Observation
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import edu.gemini.spModel.config2.Config
import edu.gemini.spModel.data.YesNoType
import edu.gemini.spModel.gemini.flamingos2.Flamingos2.{MOS_PREIMAGING_PROP, READMODE_PROP, ReadMode}
import edu.gemini.spModel.seqcomp.SeqConfigNames.INSTRUMENT_KEY
import seqexec.model.dhs.ImageFileId
import seqexec.server.InstrumentSystem
import seqexec.server.ConfigUtilOps._
import seqexec.server.keywords._
import seqexec.server.tcs.TcsKeywordsReader
import seqexec.server.{ConfigUtilOps, SeqAction, SeqexecFailure}
import edu.gemini.spModel.config2.Config
import edu.gemini.spModel.data.YesNoType
import edu.gemini.spModel.gemini.flamingos2.Flamingos2.{MOS_PREIMAGING_PROP, READMODE_PROP, ReadMode}
import edu.gemini.spModel.seqcomp.SeqConfigNames.INSTRUMENT_KEY
import cats.implicits._

object Flamingos2Header {
  def header(inst: InstrumentSystem[IO], f2ObsReader: Flamingos2Header.ObsKeywordsReader, tcsKeywordsReader: TcsKeywordsReader): Header =
    new Header {
      override def sendBefore(obsId: Observation.Id, id: ImageFileId): SeqAction[Unit] =  {
        sendKeywords(id, inst, List(
          buildBoolean(f2ObsReader.getPreimage.map(_.toBoolean), "PREIMAGE"),
          buildString(SeqAction(LocalDate.now.format(DateTimeFormatter.ISO_LOCAL_DATE)), "DATE-OBS"),
          buildString(tcsKeywordsReader.getUT.orDefault, "TIME-OBS"),
          buildString(f2ObsReader.getReadMode.map{
            case ReadMode.BRIGHT_OBJECT_SPEC => "Bright"
            case ReadMode.MEDIUM_OBJECT_SPEC => "Medium"
            case ReadMode.FAINT_OBJECT_SPEC  => "Dark"
          }, "READMODE"),
          buildInt32(f2ObsReader.getReadMode.map{
            case ReadMode.BRIGHT_OBJECT_SPEC => 1
            case ReadMode.MEDIUM_OBJECT_SPEC => 4
            case ReadMode.FAINT_OBJECT_SPEC  => 8
          }, "NREADS"))
        )
      }

      override def sendAfter(id: ImageFileId): SeqAction[Unit] = SeqAction(())
    }

  trait ObsKeywordsReader {
    def getPreimage: SeqAction[YesNoType]
    def getReadMode: SeqAction[ReadMode]
  }

  class ObsKeywordsReaderImpl(config: Config) extends ObsKeywordsReader {
    override def getPreimage: SeqAction[YesNoType] = EitherT(IO.pure(config.extract(INSTRUMENT_KEY / MOS_PREIMAGING_PROP)
      .as[YesNoType].leftMap(e => SeqexecFailure.Unexpected(ConfigUtilOps.explain(e)))))

    override def getReadMode: SeqAction[ReadMode] = EitherT(IO.pure(config.extract(INSTRUMENT_KEY / READMODE_PROP)
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
    override def getHealth: SeqAction[String] = Flamingos2Epics.instance.health.toSeqAction

    override def getState: SeqAction[String] = Flamingos2Epics.instance.state.toSeqAction
  }

}
