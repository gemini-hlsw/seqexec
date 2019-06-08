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
            buildDouble(altairReader.lgdfocus, KeywordName.LGDFOCUS),
            buildDouble(altairReader.lgttcnts, KeywordName.LGTTCNTS),
            buildInt32(altairReader.lgttexp, KeywordName.LGTTEXP),
            buildDouble(altairReader.lgsfcnts, KeywordName.LGSFCNTS),
            buildDouble(altairReader.lgsfexp, KeywordName.LGSFEXP),
            buildDouble(altairReader.fsmtip, KeywordName.FSMTIP),
            buildDouble(altairReader.fsmtilt, KeywordName.FSMTILT),
            buildDouble(altairReader.lgzmpos, KeywordName.LGZMPOS),
            buildDouble(altairReader.naalt, KeywordName.NAALT),
            buildDouble(altairReader.nathick, KeywordName.NATHICK),
            buildString(altairReader.lgndfilt, KeywordName.LGNDFILT),
            buildString(altairReader.lgttiris, KeywordName.LGTTIRIS)
          )
        )

      override def sendBefore(obsId: Observation.Id, id: ImageFileId): F[Unit] =
        Applicative[F].unit
    }

}
