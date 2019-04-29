// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.flamingos2

import cats.Applicative
import cats.data.EitherT
import cats.effect.LiftIO
import cats.effect.Sync
import cats.implicits._
import gem.Observation
import gem.enum.KeywordName
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import edu.gemini.spModel.config2.Config
import edu.gemini.spModel.data.YesNoType
import edu.gemini.spModel.gemini.flamingos2.Flamingos2.MOS_PREIMAGING_PROP
import edu.gemini.spModel.gemini.flamingos2.Flamingos2.READMODE_PROP
import edu.gemini.spModel.gemini.flamingos2.Flamingos2.ReadMode
import edu.gemini.spModel.seqcomp.SeqConfigNames.INSTRUMENT_KEY
import seqexec.model.dhs.ImageFileId
import seqexec.server.InstrumentSystem
import seqexec.server.ConfigUtilOps._
import seqexec.server.keywords._
import seqexec.server.tcs.TcsKeywordsReader
import seqexec.server.ConfigUtilOps
import seqexec.server.SeqexecFailure
import edu.gemini.spModel.config2.Config
import edu.gemini.spModel.data.YesNoType
import edu.gemini.spModel.gemini.flamingos2.Flamingos2.MOS_PREIMAGING_PROP
import edu.gemini.spModel.gemini.flamingos2.Flamingos2.READMODE_PROP
import edu.gemini.spModel.gemini.flamingos2.Flamingos2.ReadMode
import edu.gemini.spModel.seqcomp.SeqConfigNames.INSTRUMENT_KEY
import cats.implicits._

object Flamingos2Header {
  def header[F[_]: Sync](inst:              InstrumentSystem[F],
                         f2ObsReader:       Flamingos2Header.ObsKeywordsReader[F],
                         tcsKeywordsReader: TcsKeywordsReader[F]): Header[F] =
    new Header[F] {
      override def sendBefore(obsId: Observation.Id, id: ImageFileId): F[Unit] =
        sendKeywords(
          id,
          inst,
          List(
            buildBooleanS(f2ObsReader.preimage, KeywordName.PREIMAGE),
            buildStringS(
              Sync[F].delay(
                LocalDate.now.format(DateTimeFormatter.ISO_LOCAL_DATE)),
              KeywordName.DATE_OBS),
            buildStringS(tcsKeywordsReader.getUT, KeywordName.TIME_OBS),
            buildStringS(f2ObsReader.readMode, KeywordName.READMODE),
            buildInt32S(f2ObsReader.nReads, KeywordName.NREADS)
          )
        )

      override def sendAfter(id: ImageFileId): F[Unit] = Applicative[F].unit
    }

  trait ObsKeywordsReader[F[_]] {
    def preimage: F[Boolean]
    def readMode: F[String]
    def nReads: F[Int]
  }

  object ObsKeywordsReaderODB {
    def apply[F[_]: Sync](config: Config): ObsKeywordsReader[F] =
      new ObsKeywordsReader[F] {
        def getPreimage: F[YesNoType] =
          EitherT(
            Sync[F].delay(config
              .extractAs[YesNoType](INSTRUMENT_KEY / MOS_PREIMAGING_PROP)
              .leftMap[Throwable](e =>
                SeqexecFailure.Unexpected(ConfigUtilOps.explain(e))))).rethrowT

        def getReadMode: F[ReadMode] =
          EitherT(
            Sync[F].delay(config
              .extractAs[ReadMode](INSTRUMENT_KEY / READMODE_PROP)
              .leftMap[Throwable](e =>
                SeqexecFailure.Unexpected(ConfigUtilOps.explain(e))))).rethrowT

        override def preimage: F[Boolean] = getPreimage.map(_.toBoolean)
        override def readMode: F[String] =
          getReadMode.map {
            case ReadMode.BRIGHT_OBJECT_SPEC => "Bright"
            case ReadMode.MEDIUM_OBJECT_SPEC => "Medium"
            case ReadMode.FAINT_OBJECT_SPEC  => "Dark"
          }
        override def nReads: F[Int] =
          getReadMode.map {
            case ReadMode.BRIGHT_OBJECT_SPEC => 1
            case ReadMode.MEDIUM_OBJECT_SPEC => 4
            case ReadMode.FAINT_OBJECT_SPEC  => 8
          }
      }
  }

  trait InstKeywordsReader[F[_]] {
    def getHealth: F[String]
    def getState: F[String]
  }

  object InstKeywordReaderDummy {
    def apply[F[_]: Applicative]: InstKeywordsReader[F] =
      new InstKeywordsReader[F] {
        override def getHealth: F[String] = "GOOD".pure[F]

        override def getState: F[String] = "RUNNING".pure[F]
      }
  }

  object InstKeywordReaderEpics {
    def apply[F[_]: Sync: LiftIO]: InstKeywordsReader[F] =
      new InstKeywordsReader[F] {
        private val sys                   = Flamingos2Epics.instance
        override def getHealth: F[String] = sys.health.safeValOrDefault.to[F]

        override def getState: F[String] = sys.state.safeValOrDefault.to[F]
      }
  }

}
