// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.altair

import cats.Applicative
import cats.effect.Sync
import gem.Observation
import gem.enum.KeywordName
import seqexec.model.dhs.ImageFileId
import seqexec.server.InstrumentSystem
import seqexec.server.keywords._

object AltairHeader {
  def header[F[_]: Sync](inst: InstrumentSystem[F], altairReader: AltairKeywordReader[F]/*tcsKeywordsReader: TcsKeywordsReader[F]*/): Header[F] =
    new Header[F] {
      override def sendBefore(obsId: Observation.Id, id: ImageFileId): F[Unit] =  {
        sendKeywords(
          id,
          inst,
          List(
            buildDoubleS(altairReader.aoexpt, KeywordName.AOFREQ),
            buildDoubleS(altairReader.aocounts, KeywordName.AOCOUNTS),
            buildDoubleS(altairReader.aoseeing, KeywordName.AOSEEING),
            buildDoubleS(altairReader.aowfsx, KeywordName.AOWFSX),
            buildDoubleS(altairReader.aowfsy, KeywordName.AOWFSY),
            buildDoubleS(altairReader.aowfsz, KeywordName.AOWFSZ),
            buildDoubleS(altairReader.aogain, KeywordName.AOGAIN),
            buildStringS(altairReader.aoncpa, KeywordName.AONCPAF),
            buildStringS(altairReader.ngndfilt, KeywordName.AONDFILT),
            buildStringS(altairReader.astar, KeywordName.AOFLENS),
            buildStringS(altairReader.aoflex, KeywordName.AOFLEXF),
            buildStringS(altairReader.lgustage, KeywordName.LGUSTAGE),
            buildStringS(altairReader.aobs, KeywordName.AOBS)
          ))
      }

      override def sendAfter(id: ImageFileId): F[Unit] = Applicative[F].unit
    }

}
