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
            )
          )
        )
        ks.flatMap(gdsClient.openObservation(obsId, id, _))
      }

      override def sendAfter(id: ImageFileId): F[Unit] =
        Applicative[F].unit
    }
}
