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
            buildDoubleS(altairReader.lgdfocus, KeywordName.LGDFOCUS),
            buildDoubleS(altairReader.lgttcnts, KeywordName.LGTTCNTS),
            buildInt32S(altairReader.lgttexp, KeywordName.LGTTEXP),
            buildDoubleS(altairReader.lgsfcnts, KeywordName.LGSFCNTS),
            buildDoubleS(altairReader.lgsfexp, KeywordName.LGSFEXP),
            buildDoubleS(altairReader.fsmtip, KeywordName.FSMTIP),
            buildDoubleS(altairReader.fsmtilt, KeywordName.FSMTILT),
            buildDoubleS(altairReader.lgzmpos, KeywordName.LGZMPOS),
            buildDoubleS(altairReader.naalt, KeywordName.NAALT),
            buildDoubleS(altairReader.nathick, KeywordName.NATHICK),
            buildStringS(altairReader.lgndfilt, KeywordName.LGNDFILT),
            buildStringS(altairReader.lgttiris, KeywordName.LGTTIRIS)
          )
        )

      override def sendBefore(obsId: Observation.Id, id: ImageFileId): F[Unit] = Applicative[F].unit
    }

}
