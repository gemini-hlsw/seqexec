// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.flamingos2

import cats.data.EitherT
import cats.effect.IO
import cats.effect.Sync
import cats.implicits._
import gem.Observation
import gem.enum.KeywordName
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
import seqexec.server.{ConfigUtilOps, SeqAction, SeqActionF, SeqexecFailure}
import edu.gemini.spModel.config2.Config
import edu.gemini.spModel.data.YesNoType
import edu.gemini.spModel.gemini.flamingos2.Flamingos2.{MOS_PREIMAGING_PROP, READMODE_PROP, ReadMode}
import edu.gemini.spModel.seqcomp.SeqConfigNames.INSTRUMENT_KEY
import cats.implicits._

object Flamingos2Header {
  def header[F[_]: Sync](inst: InstrumentSystem[F], f2ObsReader: Flamingos2Header.ObsKeywordsReader[F], tcsKeywordsReader: TcsKeywordsReader[F]): Header[F] =
    new Header[F] {
      override def sendBefore(obsId: Observation.Id, id: ImageFileId): SeqActionF[F, Unit] =  {
        sendKeywords(id, inst, List(
          buildBoolean(f2ObsReader.getPreimage.map(_.toBoolean), KeywordName.PREIMAGE),
          buildString(SeqActionF(LocalDate.now.format(DateTimeFormatter.ISO_LOCAL_DATE)), KeywordName.DATE_OBS),
          buildString(tcsKeywordsReader.getUT.orDefault, KeywordName.TIME_OBS),
          buildString(f2ObsReader.getReadMode.map{
            case ReadMode.BRIGHT_OBJECT_SPEC => "Bright"
            case ReadMode.MEDIUM_OBJECT_SPEC => "Medium"
            case ReadMode.FAINT_OBJECT_SPEC  => "Dark"
          }, KeywordName.READMODE),
          buildInt32(f2ObsReader.getReadMode.map{
            case ReadMode.BRIGHT_OBJECT_SPEC => 1
            case ReadMode.MEDIUM_OBJECT_SPEC => 4
            case ReadMode.FAINT_OBJECT_SPEC  => 8
          }, KeywordName.NREADS))
        )
      }

      override def sendAfter(id: ImageFileId): SeqActionF[F, Unit] = SeqActionF.void
    }

  trait ObsKeywordsReader[F[_]] {
    def getPreimage: SeqActionF[F, YesNoType]
    def getReadMode: SeqActionF[F, ReadMode]
  }

  class ObsKeywordsReaderImpl[F[_]: Sync](config: Config) extends ObsKeywordsReader[F] {
    override def getPreimage: SeqActionF[F, YesNoType] = EitherT(Sync[F].delay(config.extractAs[YesNoType](INSTRUMENT_KEY / MOS_PREIMAGING_PROP)
      leftMap(e => SeqexecFailure.Unexpected(ConfigUtilOps.explain(e)))))

    override def getReadMode: SeqActionF[F, ReadMode] = EitherT(Sync[F].delay(config.extractAs[ReadMode](INSTRUMENT_KEY / READMODE_PROP)
      leftMap(e => SeqexecFailure.Unexpected(ConfigUtilOps.explain(e)))))
  }

  trait InstKeywordsReader[F[_]] {
    def getHealth: SeqActionF[F, String]
    def getState: SeqActionF[F, String]
  }

  object DummyInstKeywordReader extends InstKeywordsReader[IO] {
    override def getHealth: SeqActionF[IO, String] = SeqAction("GOOD")

    override def getState: SeqActionF[IO, String] = SeqAction("RUNNING")
  }

  object InstKeywordReaderImpl extends InstKeywordsReader[IO] {
    override def getHealth: SeqActionF[IO, String] = Flamingos2Epics.instance.health.toSeqActionDefault

    override def getState: SeqActionF[IO, String] = Flamingos2Epics.instance.state.toSeqActionDefault
  }

}
