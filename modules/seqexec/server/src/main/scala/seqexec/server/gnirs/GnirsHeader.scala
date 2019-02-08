// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gnirs

import cats.effect.Sync
import gem.Observation
import gem.enum.KeywordName
import seqexec.model.dhs.ImageFileId
import seqexec.server.keywords._
import seqexec.server.InstrumentSystem
import seqexec.server.tcs.TcsKeywordsReader

object GnirsHeader {
  def header[F[_]: Sync](inst: InstrumentSystem[F], gnirsReader: GnirsKeywordReader[F], tcsReader: TcsKeywordsReader[F]): Header[F] = new Header[F] {
    override def sendBefore(obsId: Observation.Id, id: ImageFileId): F[Unit] =
      sendKeywords(id, inst, List(
        buildInt32(tcsReader.getGnirsInstPort.orDefault, KeywordName.INPORT),
        buildString(gnirsReader.getArrayId, KeywordName.ARRAYID),
        buildString(gnirsReader.getArrayType, KeywordName.ARRAYTYP),
        buildString(tcsReader.getDate.orDefault, KeywordName.DATE_OBS),
        buildString(tcsReader.getUT.orDefault, KeywordName.TIME_OBS),
        buildString(tcsReader.getUT.orDefault, KeywordName.UTSTART),
        buildString(gnirsReader.getFilter1, KeywordName.FILTER1),
        buildInt32(gnirsReader.getFilterWheel1Pos, KeywordName.FW1_ENG),
        buildString(gnirsReader.getFilter2, KeywordName.FILTER2),
        buildInt32(gnirsReader.getFilterWheel2Pos, KeywordName.FW2_ENG),
        buildString(gnirsReader.getCamera, KeywordName.CAMERA),
        buildInt32(gnirsReader.getCameraPos, KeywordName.CAM_ENG),
        buildString(gnirsReader.getSlit, KeywordName.SLIT),
        buildInt32(gnirsReader.getSlitPos, KeywordName.SLIT_ENG),
        buildString(gnirsReader.getDecker, KeywordName.DECKER),
        buildInt32(gnirsReader.getDeckerPos, KeywordName.DKR_ENG),
        buildString(gnirsReader.getGrating, KeywordName.GRATING),
        buildInt32(gnirsReader.getGratingPos, KeywordName.GR_ENG),
        buildDouble(gnirsReader.getGratingWavelength, KeywordName.GRATWAVE),
        buildInt32(gnirsReader.getGratingOrder, KeywordName.GRATORD),
        buildDouble(gnirsReader.getGratingTilt, KeywordName.GRATTILT),
        buildString(gnirsReader.getPrism, KeywordName.PRISM),
        buildInt32(gnirsReader.getPrismPos, KeywordName.PRSM_ENG),
        buildString(gnirsReader.getAcquisitionMirror, KeywordName.ACQMIR),
        buildString(gnirsReader.getWindowCover, KeywordName.COVER),
        buildString(gnirsReader.getFocus, KeywordName.FOCUS),
        buildInt32(gnirsReader.getFocusPos, KeywordName.FCS_ENG),
        buildDouble(gnirsReader.getDetectorBias, KeywordName.DETBIAS)
      ) )

    override def sendAfter(id: ImageFileId): F[Unit] =
      sendKeywords(id, inst, List(
        buildString(tcsReader.getUT.orDefault, KeywordName.UTEND),
        buildDouble(gnirsReader.getObsEpoch, KeywordName.OBSEPOCH)
      ) )
  }
}
