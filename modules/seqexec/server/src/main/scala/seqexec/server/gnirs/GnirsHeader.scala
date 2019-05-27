// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
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
        buildInt32S(tcsReader.gnirsInstPort, KeywordName.INPORT),
        buildStringS(gnirsReader.arrayId, KeywordName.ARRAYID),
        buildStringS(gnirsReader.arrayType, KeywordName.ARRAYTYP),
        buildStringS(tcsReader.date, KeywordName.DATE_OBS),
        buildStringS(tcsReader.ut, KeywordName.TIME_OBS),
        buildStringS(tcsReader.ut, KeywordName.UTSTART),
        buildStringS(gnirsReader.filter1, KeywordName.FILTER1),
        buildInt32S(gnirsReader.filterWheel1Pos, KeywordName.FW1_ENG),
        buildStringS(gnirsReader.filter2, KeywordName.FILTER2),
        buildInt32S(gnirsReader.filterWheel2Pos, KeywordName.FW2_ENG),
        buildStringS(gnirsReader.camera, KeywordName.CAMERA),
        buildInt32S(gnirsReader.cameraPos, KeywordName.CAM_ENG),
        buildStringS(gnirsReader.slit, KeywordName.SLIT),
        buildInt32S(gnirsReader.slitPos, KeywordName.SLIT_ENG),
        buildStringS(gnirsReader.decker, KeywordName.DECKER),
        buildInt32S(gnirsReader.deckerPos, KeywordName.DKR_ENG),
        buildStringS(gnirsReader.grating, KeywordName.GRATING),
        buildInt32S(gnirsReader.gratingPos, KeywordName.GR_ENG),
        buildDoubleS(gnirsReader.gratingWavelength, KeywordName.GRATWAVE),
        buildInt32S(gnirsReader.gratingOrder, KeywordName.GRATORD),
        buildDoubleS(gnirsReader.gratingTilt, KeywordName.GRATTILT),
        buildStringS(gnirsReader.prism, KeywordName.PRISM),
        buildInt32S(gnirsReader.prismPos, KeywordName.PRSM_ENG),
        buildStringS(gnirsReader.acquisitionMirror, KeywordName.ACQMIR),
        buildStringS(gnirsReader.windowCover, KeywordName.COVER),
        buildStringS(gnirsReader.focus, KeywordName.FOCUS),
        buildInt32S(gnirsReader.focusPos, KeywordName.FCS_ENG),
        buildDoubleS(gnirsReader.detectorBias, KeywordName.DETBIAS)
      ) )

    override def sendAfter(id: ImageFileId): F[Unit] =
      sendKeywords(id, inst, List(
        buildStringS(tcsReader.ut, KeywordName.UTEND),
        buildDoubleS(gnirsReader.obsEpoch, KeywordName.OBSEPOCH)
      ) )
  }
}
