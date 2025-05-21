// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.igrins2

import cats.MonadThrow
import cats.syntax.all._
import org.typelevel.log4cats.Logger
import seqexec.model.Observation
import seqexec.model.dhs.ImageFileId
import seqexec.model.enums.KeywordName
import seqexec.server.keywords._
import seqexec.server.tcs.TcsKeywordsReader

object Igrins2Header {

  def header[F[_]: MonadThrow: Logger](
    gdsClient:         GdsClient[F],
    tcsKeywordsReader: TcsKeywordsReader[F]
  ): Header[F] =
    new Header[F] {
      override def sendBefore(obsId: Observation.Id, id: ImageFileId): F[Unit] = {
        val ks = GdsInstrument.bundleKeywords[F] {
          List(
            buildInt32(tcsKeywordsReader.igrins2InstPort, KeywordName.INPORT),
            buildString(tcsKeywordsReader.dateUT, KeywordName.DATE_OBS),
            buildString(s"$id.fits".pure[F], KeywordName.ORIGNAME)
          )
        }
        ks.flatMap(gdsClient.setKeywords(id, _))
      }

      override def sendAfter(id: ImageFileId): F[Unit] =
        sendGdsKeywords(id,
                        gdsClient,
                        List(
                          buildString(tcsKeywordsReader.hourAngle, KeywordName.HAEND),
                          buildString(tcsKeywordsReader.date, KeywordName.DATEEND)
                        )
        )
    }
}
