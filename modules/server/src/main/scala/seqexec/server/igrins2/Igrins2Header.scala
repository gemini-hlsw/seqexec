// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.igrins2

import cats.MonadThrow
import cats.syntax.all._
import seqexec.model.Observation
import seqexec.model.dhs.ImageFileId
import seqexec.server.keywords._
import seqexec.model.enums.KeywordName
import seqexec.server.tcs.TcsKeywordsReader
import org.typelevel.log4cats.Logger

object Igrins2Header {

  def header[F[_]: MonadThrow: Logger](
    gdsClient:         GdsClient[F],
    tcsKeywordsReader: TcsKeywordsReader[F]
  ): Header[F] =
    new Header[F] {
      override def sendBefore(obsId: Observation.Id, id: ImageFileId): F[Unit] = {
        val ks = GdsInstrument.bundleKeywords[F] {
          List(
            buildInt32(tcsKeywordsReader.igrins2InstPort, KeywordName.INPORT),
            buildString(tcsKeywordsReader.date, KeywordName.DATE_OBS),
            buildDouble(tcsKeywordsReader.p2ara, KeywordName.P2ARA),
            buildDouble(tcsKeywordsReader.p2adec, KeywordName.P2ADEC),
            buildDouble(tcsKeywordsReader.p2arv, KeywordName.P2ARV),
            buildDouble(tcsKeywordsReader.p2awavel, KeywordName.P2AWAVEL),
            buildString(tcsKeywordsReader.p2aepoch, KeywordName.P2AEPOCH),
            buildString(tcsKeywordsReader.p2aequin, KeywordName.P2AEQUIN),
            buildString(tcsKeywordsReader.p2aobject, KeywordName.P2AOBJEC),
            buildDouble(tcsKeywordsReader.p2apmdec, KeywordName.P2APMDEC),
            buildDouble(tcsKeywordsReader.p2apmra, KeywordName.P2APMRA),
            buildDouble(tcsKeywordsReader.p2aparal, KeywordName.P2APARAL),
            buildDouble(tcsKeywordsReader.defocusA, KeywordName.P2FOCUS),
            buildDouble(tcsKeywordsReader.freq, KeywordName.P2FREQ)
          )
        }
        ks.flatMap(gdsClient.setKeywords(id, _))
      }

      override def sendAfter(id: ImageFileId): F[Unit] =
        sendGdsKeywords(id,
                        gdsClient,
                        List(
                          buildString(tcsKeywordsReader.hourAngle, KeywordName.HAEND),
                          buildString(tcsKeywordsReader.date, KeywordName.DATEEND)
                        )
        )
    }
}
