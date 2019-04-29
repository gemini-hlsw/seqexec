// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.niri

import cats.effect.Sync
import gem.Observation
import gem.enum.KeywordName
import seqexec.model.dhs.ImageFileId
import seqexec.server.keywords.{Header, buildDoubleS, buildStringS, _}
import seqexec.server.InstrumentSystem
import seqexec.server.tcs.TcsKeywordsReader

object NiriHeader {
  // scalastyle:off
  def header[F[_]: Sync](inst: InstrumentSystem[F], instReader: NiriKeywordReader[F],
             tcsKeywordsReader: TcsKeywordsReader[F]): Header[F] = new Header[F] {
    override def sendBefore(obsId: Observation.Id, id: ImageFileId): F[Unit] =
      sendKeywords(id, inst, List(
        buildStringS(instReader.arrayId, KeywordName.ARRAYID),
        buildStringS(instReader.arrayType, KeywordName.ARRAYTYP),
        buildStringS(instReader.camera, KeywordName.CAMERA),
        buildInt32S(instReader.coadds, KeywordName.COADDS),
        buildStringS(tcsKeywordsReader.getDate, KeywordName.DATE_OBS),
        buildDoubleS(instReader.exposureTime, KeywordName.EXPTIME),
        buildStringS(instReader.filter1, KeywordName.FILTER1),
        buildStringS(instReader.filter2, KeywordName.FILTER2),
        buildStringS(instReader.filter3, KeywordName.FILTER3),
        buildStringS(instReader.focusName, KeywordName.FOCUSNAM),
        buildDoubleS(instReader.focusPosition, KeywordName.FOCUSPOS),
        buildStringS(instReader.focalPlaneMask, KeywordName.FPMASK),
        buildStringS(instReader.beamSplitter, KeywordName.BEAMSPLT),
        buildStringS(instReader.windowCover, KeywordName.WINDCOVR),
        buildInt32S(instReader.framesPerCycle, KeywordName.FRMSPCYCL),
        buildStringS(instReader.headerTiming, KeywordName.HDRTIMING),
        buildInt32S(tcsKeywordsReader.getNiriInstPort, KeywordName.INPORT),
        buildInt32S(instReader.lnrs, KeywordName.LNRS),
        buildStringS(instReader.mode, KeywordName.MODE),
        buildInt32S(instReader.numberDigitalAverage, KeywordName.NDAVGS),
        buildStringS(instReader.pupilViewer, KeywordName.PVIEW),
        buildDoubleS(instReader.detectorTemperature, KeywordName.TDETABS),
        buildStringS(tcsKeywordsReader.getUT, KeywordName.TIME),
        buildStringS(tcsKeywordsReader.getUT, KeywordName.TIME_OBS),
        buildDoubleS(instReader.mountTemperature, KeywordName.TMOUNT),
        buildStringS(instReader.µcodeName, KeywordName.UCODENAM),
        buildStringS(instReader.µcodeType, KeywordName.UCODETYP),
        buildDoubleS(instReader.cl1VoltageDD, KeywordName.VDDCL1),
        buildDoubleS(instReader.cl2VoltageDD, KeywordName.VDDCL2),
        buildDoubleS(instReader.ucVoltage, KeywordName.VDDUC),
        buildDoubleS(instReader.detectorVoltage, KeywordName.VDET),
        buildDoubleS(instReader.cl1VoltageGG, KeywordName.VGGCL1),
        buildDoubleS(instReader.cl2VoltageGG, KeywordName.VGGCL2),
        buildDoubleS(instReader.setVoltage, KeywordName.VSET)
      ))

    override def sendAfter(id: ImageFileId): F[Unit] =
      sendKeywords(id, inst, List(
        buildDoubleS(instReader.detectorTemperature, KeywordName.A_TDETABS),
        buildDoubleS(instReader.mountTemperature, KeywordName.A_TMOUNT),
        buildDoubleS(instReader.cl1VoltageDD, KeywordName.A_VDDCL1),
        buildDoubleS(instReader.cl2VoltageDD, KeywordName.A_VDDCL2),
        buildDoubleS(instReader.ucVoltage, KeywordName.A_VDDUC),
        buildDoubleS(instReader.detectorVoltage, KeywordName.A_VDET),
        buildDoubleS(instReader.cl1VoltageGG, KeywordName.A_VGGCL1),
        buildDoubleS(instReader.cl2VoltageGG, KeywordName.A_VGGCL2),
        buildDoubleS(instReader.setVoltage, KeywordName.A_VSET),
        buildStringS(tcsKeywordsReader.getUT, KeywordName.UTEND),
        buildDoubleS(instReader.observationEpoch, KeywordName.OBSEPOCH)
      ))
  }
  // scalastyle:on

}
