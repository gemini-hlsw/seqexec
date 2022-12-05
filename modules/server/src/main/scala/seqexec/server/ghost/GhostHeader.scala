// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.ghost

import cats.Applicative
import cats.MonadThrow
import cats.syntax.all._
import seqexec.model.Observation
import seqexec.model.dhs.ImageFileId
import seqexec.server.keywords._
import lucuma.core.enums.KeywordName

object GhostHeader {
  val StrDefaultValue: DefaultHeaderValue[String] =
    new DefaultHeaderValue[String] {
      val default: String = ""
    }

  def header[F[_]: MonadThrow](
    gdsClient:           GdsClient[F],
    ghostKeywordsReader: GhostKeywordsReader[F]
  ): Header[F] =
    new Header[F] {
      override def sendBefore(obsId: Observation.Id, id: ImageFileId): F[Unit] = {
        val ks = GdsInstrument.bundleKeywords[F](
          List(
            buildBoolean(ghostKeywordsReader.basePos,
                         KeywordName.BASEPO,
                         DefaultHeaderValue.TrueDefaultValue
            ),
            buildString(ghostKeywordsReader.srifu1, KeywordName.SRIFU1),
            buildString(ghostKeywordsReader.srifu2, KeywordName.SRIFU2),
            buildString(ghostKeywordsReader.hrifu1, KeywordName.HRIFU1),
            buildString(ghostKeywordsReader.hrifu2, KeywordName.HRIFU2),
            buildBoolean(ghostKeywordsReader.fiberAgitator1Enabled,
                         KeywordName.FAGITAT1,
                         DefaultHeaderValue.FalseDefaultValue
            ),
            buildBoolean(ghostKeywordsReader.fiberAgitator2Enabled,
                         KeywordName.FAGITAT2,
                         DefaultHeaderValue.FalseDefaultValue
            )
          )
        )
        ks.flatMap(gdsClient.openObservation(obsId, id, _))
      }

      override def sendAfter(id: ImageFileId): F[Unit] =
        Applicative[F].unit
    }
}
