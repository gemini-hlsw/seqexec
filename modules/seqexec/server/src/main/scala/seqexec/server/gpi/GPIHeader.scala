// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gpi

import gem.Observation
import seqexec.model.dhs.ImageFileId
import seqexec.server.keywords.Header
import seqexec.server.SeqAction
import seqexec.server.keywords.GDSClient
import seqexec.server.Header._
import seqexec.server.Header.Implicits._
import seqexec.server.tcs.TcsKeywordsReader

object GPIHeader {

  def header[A: HeaderProvider](inst: A,
                                gdsClient: GDSClient,
                                tcsKeywordsReader: TcsKeywordsReader,
                                obsKeywordsReader: ObsKeywordsReader): Header =
    new Header {
      override def sendBefore(obsId: Observation.Id,
                              id: ImageFileId): SeqAction[Unit] = {
        val ks = bundleKeywords(
          inst,
          List(
            buildDouble(tcsKeywordsReader.getParallacticAngle
                          .map(_.map(_.toDoubleDegrees))
                          .orDefault,
                        "PAR_ANG"),
            buildInt32(tcsKeywordsReader.getGpiInstPort.orDefault, "INPORT"),
            buildBoolean(obsKeywordsReader.getAstrometicField, "ASTROMTC")
          )
        )
        ks.flatMap(gdsClient.openObservation(obsId, id, _))
      }

      override def sendAfter(obsId: Observation.Id,
                             id: ImageFileId): SeqAction[Unit] =
        gdsClient.closeObservation(id)
    }
}
