// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.igrins2

import cats.MonadThrow
import cats.syntax.all._
import seqexec.model.Observation
import seqexec.model.dhs.ImageFileId
import seqexec.server.keywords._
import seqexec.model.enums.KeywordName
import seqexec.server.tcs.TcsKeywordsReader
import org.typelevel.log4cats.Logger

object Igrins2Header {

  def header[F[_]: MonadThrow: Logger](
    gdsClient:         GdsClient[F],
    tcsKeywordsReader: TcsKeywordsReader[F]
  ): Header[F] =
    new Header[F] {
      override def sendBefore(obsId: Observation.Id, id: ImageFileId): F[Unit] = {
        val ks = GdsInstrument.bundleKeywords[F] {
          List(
            buildInt32(tcsKeywordsReader.igrins2InstPort, KeywordName.INPORT)
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
