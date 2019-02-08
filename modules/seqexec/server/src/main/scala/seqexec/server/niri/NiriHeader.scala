// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.niri

import cats.effect.Sync
import gem.Observation
import gem.enum.KeywordName
import seqexec.model.dhs.ImageFileId
import seqexec.server.keywords.{Header, buildDouble, buildString, _}
import seqexec.server.InstrumentSystem
import seqexec.server.tcs.TcsKeywordsReader

object NiriHeader {
  // scalastyle:off
  def header[F[_]: Sync](inst: InstrumentSystem[F], instReader: NiriKeywordReader[F],
             tcsKeywordsReader: TcsKeywordsReader[F]): Header[F] = new Header[F] {
    override def sendBefore(obsId: Observation.Id, id: ImageFileId): F[Unit] =
      sendKeywords(id, inst, List(
        buildString(instReader.arrayId, KeywordName.ARRAYID),
        buildString(instReader.arrayType, KeywordName.ARRAYTYP),
        buildString(instReader.camera, KeywordName.CAMERA),
        buildInt32(instReader.coadds, KeywordName.COADDS),
        buildString(tcsKeywordsReader.getDate.orDefault, KeywordName.DATE_OBS),
        buildDouble(instReader.exposureTime, KeywordName.EXPTIME),
        buildString(instReader.filter1, KeywordName.FILTER1),
        buildString(instReader.filter2, KeywordName.FILTER2),
        buildString(instReader.filter3, KeywordName.FILTER3),
        buildString(instReader.focusName, KeywordName.FOCUSNAM),
        buildDouble(instReader.focusPosition, KeywordName.FOCUSPOS),
        buildString(instReader.focalPlaneMask, KeywordName.FPMASK),
        buildString(instReader.beamSplitter, KeywordName.BEAMSPLT),
        buildString(instReader.windowCover, KeywordName.WINDCOVR),
        buildInt32(instReader.framesPerCycle, KeywordName.FRMSPCYCL),
        buildString(instReader.headerTiming, KeywordName.HDRTIMING),
        buildInt32(tcsKeywordsReader.getNiriInstPort.orDefault, KeywordName.INPORT),
        buildInt32(instReader.lnrs, KeywordName.LNRS),
        buildString(instReader.mode, KeywordName.MODE),
        buildInt32(instReader.numberDigitalAverage, KeywordName.NDAVGS),
        buildString(instReader.pupilViewer, KeywordName.PVIEW),
        buildDouble(instReader.detectorTemperature, KeywordName.TDETABS),
        buildString(tcsKeywordsReader.getUT.orDefault, KeywordName.TIME),
        buildString(tcsKeywordsReader.getUT.orDefault, KeywordName.TIME_OBS),
        buildDouble(instReader.mountTemperature, KeywordName.TMOUNT),
        buildString(instReader.µcodeName, KeywordName.UCODENAM),
        buildString(instReader.µcodeType, KeywordName.UCODETYP),
        buildDouble(instReader.cl1VoltageDD, KeywordName.VDDCL1),
        buildDouble(instReader.cl2VoltageDD, KeywordName.VDDCL2),
        buildDouble(instReader.ucVoltage, KeywordName.VDDUC),
        buildDouble(instReader.detectorVoltage, KeywordName.VDET),
        buildDouble(instReader.cl1VoltageGG, KeywordName.VGGCL1),
        buildDouble(instReader.cl2VoltageGG, KeywordName.VGGCL2),
        buildDouble(instReader.setVoltage, KeywordName.VSET)
      ))

    override def sendAfter(id: ImageFileId): F[Unit] =
      sendKeywords(id, inst, List(
        buildDouble(instReader.detectorTemperature, KeywordName.A_TDETABS),
        buildDouble(instReader.mountTemperature, KeywordName.A_TMOUNT),
        buildDouble(instReader.cl1VoltageDD, KeywordName.A_VDDCL1),
        buildDouble(instReader.cl2VoltageDD, KeywordName.A_VDDCL2),
        buildDouble(instReader.ucVoltage, KeywordName.A_VDDUC),
        buildDouble(instReader.detectorVoltage, KeywordName.A_VDET),
        buildDouble(instReader.cl1VoltageGG, KeywordName.A_VGGCL1),
        buildDouble(instReader.cl2VoltageGG, KeywordName.A_VGGCL2),
        buildDouble(instReader.setVoltage, KeywordName.A_VSET),
        buildString(tcsKeywordsReader.getUT.orDefault, KeywordName.UT),
        buildDouble(instReader.observationEpoch, KeywordName.OBSEPOCH)
      ))
  }
  // scalastyle:on

}
