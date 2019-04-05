// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.altair

import cats.Applicative
import cats.effect.Sync
import gem.Observation
import gem.enum.KeywordName
import seqexec.model.dhs.ImageFileId
import seqexec.server.InstrumentSystem
import seqexec.server.keywords._

object AltairLgsHeader {

  def header[F[_]: Sync](inst: InstrumentSystem[F], altairReader: AltairKeywordReader[F]): Header[F] =
    new Header[F] {

      override def sendAfter(id: ImageFileId): F[Unit] =
        sendKeywords(
          id,
          inst,
          List(
            buildDoubleS(altairReader.aofreq, KeywordName.LGDFOCUS),
            buildDoubleS(altairReader.aofreq, KeywordName.LGTTCNTS),
            buildDoubleS(altairReader.aofreq, KeywordName.LGTTEXP),
            buildDoubleS(altairReader.aofreq, KeywordName.LGSFCNTS),
            buildDoubleS(altairReader.aofreq, KeywordName.LGSFEXP),
            buildDoubleS(altairReader.aofreq, KeywordName.FSMTIP),
            buildDoubleS(altairReader.aofreq, KeywordName.FSMTILT),
            buildDoubleS(altairReader.aofreq, KeywordName.LGZMPOS),
            buildDoubleS(altairReader.aofreq, KeywordName.NAALT),
            buildDoubleS(altairReader.aofreq, KeywordName.NATHICK),
            buildDoubleS(altairReader.aofreq, KeywordName.LGNDFILT),
            buildDoubleS(altairReader.aofreq, KeywordName.LGTTIRIS)
          )
        )

      override def sendBefore(obsId: Observation.Id, id: ImageFileId): F[Unit] = Applicative[F].unit
    }

}
