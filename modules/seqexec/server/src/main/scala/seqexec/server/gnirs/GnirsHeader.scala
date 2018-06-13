// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gnirs

import seqexec.model.dhs.ImageFileId
import seqexec.server.Header._
import seqexec.server.Header.Implicits._
import seqexec.server.tcs.TcsKeywordsReader
import seqexec.server.{DhsClient, Header, SeqAction}

class GnirsHeader(hs: DhsClient, gnirsReader: GnirsKeywordReader, tcsReader: TcsKeywordsReader) extends Header {
  override def sendBefore(id: ImageFileId, inst: String): SeqAction[Unit] =
    sendKeywords(id, inst, hs, List(
      buildInt32(tcsReader.getGnirsInstPort.orDefault, "INPORT"),
      buildString(gnirsReader.getArrayId, "ARRAYID"),
      buildString(gnirsReader.getArrayType, "ARRAYTYP"),
      buildString(tcsReader.getDate.orDefault, "DATE-OBS"),
      buildString(tcsReader.getUT.orDefault, "TIME-OBS"),
      buildString(tcsReader.getUT.orDefault, "UTSTART"),
      buildString(gnirsReader.getFilter1, "FILTER1"),
      buildInt32(gnirsReader.getFilterWheel1Pos, "FW1_ENG"),
      buildString(gnirsReader.getFilter2, "FILTER2"),
      buildInt32(gnirsReader.getFilterWheel2Pos, "FW2_ENG"),
      buildString(gnirsReader.getCamera, "CAMERA"),
      buildInt32(gnirsReader.getCameraPos, "CAM_ENG"),
      buildString(gnirsReader.getSlit, "SLIT"),
      buildInt32(gnirsReader.getSlitPos, "SLIT_ENG"),
      buildString(gnirsReader.getDecker, "DECKER"),
      buildInt32(gnirsReader.getDeckerPos, "DKR_ENG"),
      buildString(gnirsReader.getGrating, "GRATING"),
      buildInt32(gnirsReader.getGratingPos, "GR_ENG"),
      buildDouble(gnirsReader.getGratingWavelength, "GRATWAVE"),
      buildInt32(gnirsReader.getGratingOrder, "GRATORD"),
      buildDouble(gnirsReader.getGratingTilt, "GRATTILT"),
      buildString(gnirsReader.getPrism, "PRISM"),
      buildInt32(gnirsReader.getPrismPos, "PRSM_ENG"),
      buildString(gnirsReader.getAcquisitionMirror, "ACQMIR"),
      buildString(gnirsReader.getWindowCover, "COVER"),
      buildString(gnirsReader.getFocus, "FOCUS"),
      buildInt32(gnirsReader.getFocusPos, "FCS_ENG"),
      buildDouble(gnirsReader.getDetectorBias, "DETBIAS")
    ) )

  override def sendAfter(id: ImageFileId, inst: String): SeqAction[Unit] =
    sendKeywords(id, inst, hs, List(
      buildString(tcsReader.getUT.orDefault, "UTEND"),
      buildDouble(gnirsReader.getObsEpoch, "OBSEPOCH")
    ) )
}

object GnirsHeader {
  def apply(hs: DhsClient, gnirsReader: GnirsKeywordReader, tcsReader: TcsKeywordsReader): GnirsHeader =
    new GnirsHeader(hs, gnirsReader, tcsReader)
}
