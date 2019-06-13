// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.altair

import cats.Applicative
import cats.effect.Sync
import cats.implicits._
import gem.Observation
import gem.enum.KeywordName
import seqexec.model.dhs.ImageFileId
import seqexec.server.InstrumentSystem
import seqexec.server.keywords._
import seqexec.server.tcs.TcsKeywordsReader
import seqexec.server.tcs.CRFollow

object AltairHeader {
  def header[F[_]: Sync](inst:              InstrumentSystem[F],
                         altairReader:      AltairKeywordReader[F],
                         tcsKeywordsReader: TcsKeywordsReader[F]): Header[F] =
    new Header[F] {
      override def sendBefore(obsId: Observation.Id, id: ImageFileId): F[Unit] =
        sendKeywords(
          id,
          inst,
          List(
            buildDouble(altairReader.aofreq, KeywordName.AOFREQ),
            buildDouble(altairReader.aocounts, KeywordName.AOCOUNTS),
            buildDouble(altairReader.aoseeing, KeywordName.AOSEEING),
            buildDouble(altairReader.aowfsx, KeywordName.AOWFSX),
            buildDouble(altairReader.aowfsy, KeywordName.AOWFSY),
            buildDouble(altairReader.aowfsz, KeywordName.AOWFSZ),
            buildDouble(altairReader.aogain, KeywordName.AOGAIN),
            buildString(altairReader.aoncpa, KeywordName.AONCPAF),
            buildString(tcsKeywordsReader.crFollow.map(
                           _.map(CRFollow.keywordValue).getOrElse("INDEF")),
                         KeywordName.CRFOLLOW),
            buildString(altairReader.ngndfilt, KeywordName.AONDFILT),
            buildString(altairReader.astar, KeywordName.AOFLENS),
            buildString(altairReader.aoflex, KeywordName.AOFLEXF),
            buildString(altairReader.lgustage, KeywordName.LGUSTAGE),
            buildString(altairReader.aobs, KeywordName.AOBS)
          )
        )

      override def sendAfter(id: ImageFileId): F[Unit] = Applicative[F].unit
    }

}
