// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gnirs

import cats.effect.Sync
import cats.syntax.all._
import seqexec.model.enums.KeywordName
import seqexec.model.Observation
import seqexec.model.dhs.ImageFileId
import seqexec.server.keywords._
import seqexec.server.tcs.TcsKeywordsReader

object GnirsHeader {
  def header[F[_]: Sync](
    gdsClient:   GdsClient[F],
    gnirsReader: GnirsKeywordReader[F],
    tcsReader:   TcsKeywordsReader[F]
  ): Header[F] = new Header[F] {
    override def sendBefore(obsId: Observation.Id, id: ImageFileId): F[Unit] = {
      val ks = GdsInstrument.bundleKeywords[F](
        List(
          buildInt32(tcsReader.gnirsInstPort, KeywordName.INPORT),
          buildString(gnirsReader.arrayId, KeywordName.ARRAYID),
          buildString(gnirsReader.arrayType, KeywordName.ARRAYTYP),
          buildString(tcsReader.date, KeywordName.DATE_OBS),
          buildString(tcsReader.ut, KeywordName.TIME_OBS),
          buildString(tcsReader.ut, KeywordName.UTSTART),
          buildString(gnirsReader.filter1, KeywordName.FILTER1),
          buildInt32(gnirsReader.filterWheel1Pos, KeywordName.FW1_ENG),
          buildString(gnirsReader.filter2, KeywordName.FILTER2),
          buildInt32(gnirsReader.filterWheel2Pos, KeywordName.FW2_ENG),
          buildString(gnirsReader.camera, KeywordName.CAMERA),
          buildInt32(gnirsReader.cameraPos, KeywordName.CAM_ENG),
          buildString(gnirsReader.slit, KeywordName.SLIT),
          buildInt32(gnirsReader.slitPos, KeywordName.SLIT_ENG),
          buildString(gnirsReader.decker, KeywordName.DECKER),
          buildInt32(gnirsReader.deckerPos, KeywordName.DKR_ENG),
          buildString(gnirsReader.grating, KeywordName.GRATING),
          buildInt32(gnirsReader.gratingPos, KeywordName.GR_ENG),
          buildDouble(gnirsReader.gratingWavelength, KeywordName.GRATWAVE),
          buildInt32(gnirsReader.gratingOrder, KeywordName.GRATORD),
          buildDouble(gnirsReader.gratingTilt, KeywordName.GRATTILT),
          buildString(gnirsReader.prism, KeywordName.PRISM),
          buildInt32(gnirsReader.prismPos, KeywordName.PRSM_ENG),
          buildString(gnirsReader.acquisitionMirror, KeywordName.ACQMIR),
          buildString(gnirsReader.windowCover, KeywordName.COVER),
          buildString(gnirsReader.focus, KeywordName.FOCUS),
          buildInt32(gnirsReader.focusPos, KeywordName.FCS_ENG),
          buildDouble(gnirsReader.detectorBias, KeywordName.DETBIAS)
        )
      )
      ks.flatMap(gdsClient.openObservation(obsId, id, _))
    }

    override def sendAfter(obsId: ImageFileId): F[Unit] = {
      val ks = GdsInstrument.bundleKeywords[F](
        List(
          buildString(tcsReader.ut, KeywordName.UTEND),
          buildDouble(gnirsReader.obsEpoch, KeywordName.OBSEPOCH)
        )
      )
      ks.flatMap(gdsClient.setKeywords(obsId, _)) *> gdsClient.closeObservation(obsId)
    }
  }
}
