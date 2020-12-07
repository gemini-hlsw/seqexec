// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.altair

import cats.Applicative
import cats.effect.Sync
import cats.syntax.all._
import io.chrisdavenport.log4cats.Logger
import lucuma.core.enum.KeywordName
import seqexec.model.Observation
import seqexec.model.dhs.ImageFileId
import seqexec.server.keywords._
import seqexec.server.tcs.CRFollow
import seqexec.server.tcs.TcsKeywordsReader

object AltairHeader {
  def header[F[_]: Sync: Logger](kwClient:  KeywordsClient[F],
                         altairReader:      AltairKeywordReader[F],
                         tcsKeywordsReader: TcsKeywordsReader[F]): Header[F] =
    new Header[F] {
      override def sendBefore(obsId: Observation.Id, id: ImageFileId): F[Unit] =
        sendKeywords(
          id,
          kwClient,
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
