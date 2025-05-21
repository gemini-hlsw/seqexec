// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gpi

import cats.Applicative
import cats.data.Nested
import cats.effect.Sync
import cats.syntax.all._
import seqexec.model.Observation
import seqexec.model.dhs.ImageFileId
import seqexec.model.enums.KeywordName
import seqexec.server.keywords._
import seqexec.server.tcs.CRFollow
import seqexec.server.tcs.TcsKeywordsReader

object GpiHeader {

  def header[F[_]: Sync](
    gdsClient:         GdsClient[F],
    tcsKeywordsReader: TcsKeywordsReader[F],
    obsKeywordsReader: ObsKeywordsReader[F]
  ): Header[F] =
    new Header[F] {
      override def sendBefore(obsId: Observation.Id, id: ImageFileId): F[Unit] = {
        val ks = GdsInstrument.bundleKeywords(
          List(
            buildDouble(tcsKeywordsReader.parallacticAngle.map(_.toDegrees), KeywordName.PAR_ANG),
            buildInt32(tcsKeywordsReader.gpiInstPort, KeywordName.INPORT),
            buildBoolean(obsKeywordsReader.astrometicField,
                         KeywordName.ASTROMTC,
                         DefaultHeaderValue.FalseDefaultValue
            ),
            buildString(Nested(tcsKeywordsReader.crFollow)
                          .map(CRFollow.keywordValue)
                          .value
                          .orDefault,
                        KeywordName.CRFOLLOW
            )
          )
        )
        ks.flatMap(gdsClient.openObservation(obsId, id, _))
      }

      override def sendAfter(id: ImageFileId): F[Unit] =
        Applicative[F].unit
    }
}
