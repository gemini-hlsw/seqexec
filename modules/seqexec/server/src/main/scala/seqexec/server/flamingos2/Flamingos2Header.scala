// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.flamingos2

import cats.Applicative
import cats.data.EitherT
import cats.effect.Sync
import cats.implicits._
import gem.Observation
import gem.enum.KeywordName
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import seqexec.model.dhs.ImageFileId
import seqexec.server.{CleanConfig, ConfigUtilOps, InstrumentSystem, SeqexecFailure}
import seqexec.server.CleanConfig.extractItem
import seqexec.server.ConfigUtilOps._
import seqexec.server.keywords._
import seqexec.server.tcs.TcsKeywordsReader
import edu.gemini.spModel.data.YesNoType
import edu.gemini.spModel.gemini.flamingos2.Flamingos2.MOS_PREIMAGING_PROP
import edu.gemini.spModel.gemini.flamingos2.Flamingos2.READMODE_PROP
import edu.gemini.spModel.gemini.flamingos2.Flamingos2.ReadMode

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
            buildBoolean(f2ObsReader.preimage, KeywordName.PREIMAGE, DefaultHeaderValue.FalseDefaultValue),
            buildString(
              Sync[F].delay(
                LocalDate.now.format(DateTimeFormatter.ISO_LOCAL_DATE)),
              KeywordName.DATE_OBS),
            buildString(tcsKeywordsReader.ut, KeywordName.TIME_OBS),
            buildString(f2ObsReader.readMode, KeywordName.READMODE),
            buildInt32(f2ObsReader.nReads, KeywordName.NREADS)
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
    def apply[F[_]: Sync](config: CleanConfig): ObsKeywordsReader[F] =
      new ObsKeywordsReader[F] {
        def getPreimage: F[YesNoType] =
          EitherT(
            Sync[F].delay(config
              .extractInstAs[YesNoType](MOS_PREIMAGING_PROP)
              .leftMap[Throwable](e =>
                SeqexecFailure.Unexpected(ConfigUtilOps.explain(e))))).rethrowT

        def getReadMode: F[ReadMode] =
          EitherT(
            Sync[F].delay(config
              .extractInstAs[ReadMode](READMODE_PROP)
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
    def apply[F[_]: Sync](sys: Flamingos2Epics[F]): InstKeywordsReader[F] =
      new InstKeywordsReader[F] {
        override def getHealth: F[String] = sys.health

        override def getState: F[String] = sys.state
      }
  }

}
