// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gpi

import cats.Applicative
import cats.effect.Sync
import cats.implicits._
import gem.Observation
import gem.enum.KeywordName
import seqexec.model.dhs.ImageFileId
import seqexec.server.keywords._
import seqexec.server.tcs.TcsKeywordsReader
import seqexec.server.tcs.CRFollow

object GpiHeader {

  def header[F[_]: Sync](
             gdsClient: GdsClient[F],
             tcsKeywordsReader: TcsKeywordsReader[F],
             obsKeywordsReader: ObsKeywordsReader[F]): Header[F] =
    new Header[F] {
      override def sendBefore(obsId: Observation.Id,
                              id: ImageFileId): F[Unit] = {
        val ks = GdsInstrument.bundleKeywords(
          List(
            buildDouble(tcsKeywordsReader.getParallacticAngle
                          .map(_.map(_.toSignedDoubleDegrees))
                          .orDefault,
                        KeywordName.PAR_ANG),
            buildInt32(tcsKeywordsReader.getGpiInstPort.orDefault,
                       KeywordName.INPORT),
            buildBoolean(obsKeywordsReader.getAstrometicField,
                         KeywordName.ASTROMTC),
            buildString(tcsKeywordsReader.getCRFollow.map(
                          _.map(CRFollow.keywordValue).getOrElse("INDEF")),
                        KeywordName.CRFOLLOW)
          )
        )
        ks.flatMap(gdsClient.openObservation(obsId, id, _))
      }

      override def sendAfter(id: ImageFileId): F[Unit] =
        Applicative[F].unit
    }
}
