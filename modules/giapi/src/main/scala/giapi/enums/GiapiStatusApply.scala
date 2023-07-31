// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package giapi.enums

import cats.syntax.eq._
import lucuma.core.util.Enumerated
import lucuma.core.enums.Instrument

/**
 * Enumerated type for Giapi Status Apply.
 * @group Enumerations (Generated)
 */
sealed abstract class GiapiStatusApply(
  val tag:        String,
  val instrument: Instrument,
  val statusType: GiapiType,
  val statusItem: String,
  val applyItem:  String,
  val tolerance:  Option[BigDecimal]
) extends Product
    with Serializable

object GiapiStatusApply {

  /** @group Constructors */
  case object GpiAdc
      extends GiapiStatusApply("GpiAdc",
                               Instrument.Gpi,
                               GiapiType.Int,
                               "gpi:adcDeploy",
                               "gpi:selectAdc.deploy",
                               Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GpiUseAo
      extends GiapiStatusApply("GpiUseAo",
                               Instrument.Gpi,
                               GiapiType.Int,
                               "gpi:ao:useAo",
                               "gpi:configAo.useAo",
                               Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GpiAoOptimize
      extends GiapiStatusApply("GpiAoOptimize",
                               Instrument.Gpi,
                               GiapiType.Int,
                               "gpi:ao:optimization",
                               "gpi:configAo.optimize",
                               Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GpiUseCal
      extends GiapiStatusApply("GpiUseCal",
                               Instrument.Gpi,
                               GiapiType.Int,
                               "gpi:cal:useCal",
                               "gpi:configCal.useCal",
                               Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GpiFpmPinholeBias
      extends GiapiStatusApply("GpiFpmPinholeBias",
                               Instrument.Gpi,
                               GiapiType.Int,
                               "gpi:cal:fpmPinholeBias",
                               "gpi:configCal.fpmPinholeBias",
                               Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GpiIntegrationTime
      extends GiapiStatusApply("GpiIntegrationTime",
                               Instrument.Gpi,
                               GiapiType.Float,
                               "gpi:currentIntegrationTime",
                               "gpi:configIfs.integrationTime",
                               Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GpiNumCoadds
      extends GiapiStatusApply("GpiNumCoadds",
                               Instrument.Gpi,
                               GiapiType.Int,
                               "gpi:currentNumCoadds",
                               "gpi:configIfs.numCoadds",
                               Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GpiMagI
      extends GiapiStatusApply("GpiMagI",
                               Instrument.Gpi,
                               GiapiType.Float,
                               "gpi:starIntensity",
                               "gpi:configAo.magnitudeI",
                               Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GpiMagH
      extends GiapiStatusApply("GpiMagH",
                               Instrument.Gpi,
                               GiapiType.Float,
                               "gpi:cal:magH",
                               "gpi:configCal.magnitudeH",
                               Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GpiCalEntranceShutter
      extends GiapiStatusApply("GpiCalEntranceShutter",
                               Instrument.Gpi,
                               GiapiType.Int,
                               "gpi:calEntranceShutter",
                               "gpi:selectShutter.calEntranceShutter",
                               Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GpiCalReferenceShutter
      extends GiapiStatusApply("GpiCalReferenceShutter",
                               Instrument.Gpi,
                               GiapiType.Int,
                               "gpi:referenceShutter",
                               "gpi:selectShutter.calReferenceShutter",
                               Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GpiCalScienceShutter
      extends GiapiStatusApply("GpiCalScienceShutter",
                               Instrument.Gpi,
                               GiapiType.Int,
                               "gpi:scienceShutter",
                               "gpi:selectShutter.calScienceShutter",
                               Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GpiEntranceShutter
      extends GiapiStatusApply("GpiEntranceShutter",
                               Instrument.Gpi,
                               GiapiType.Int,
                               "gpi:omssEntranceShutter",
                               "gpi:selectShutter.entranceShutter",
                               Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GpiCalExitShutter
      extends GiapiStatusApply("GpiCalExitShutter",
                               Instrument.Gpi,
                               GiapiType.Int,
                               "gpi:calExitShutter",
                               "gpi:selectShutter.calExitShutter",
                               Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GpiPupilCamera
      extends GiapiStatusApply("GpiPupilCamera",
                               Instrument.Gpi,
                               GiapiType.Int,
                               "gpi:pupilViewingMirror",
                               "gpi:selectPupilCamera.deploy",
                               Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GpiSCPower
      extends GiapiStatusApply("GpiSCPower",
                               Instrument.Gpi,
                               GiapiType.Float,
                               "gpi:artificialSourceSCpower",
                               "gpi:selectSource.sourceSCpower",
                               Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GpiSCAttenuation
      extends GiapiStatusApply("GpiSCAttenuation",
                               Instrument.Gpi,
                               GiapiType.Float,
                               "gpi:artificialSourceSCDb",
                               "gpi:selectSource.sourceSCatten",
                               Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GpiSrcVis
      extends GiapiStatusApply("GpiSrcVis",
                               Instrument.Gpi,
                               GiapiType.Int,
                               "gpi:artificialSourceVIS",
                               "gpi:selectSource.sourceVis",
                               Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GpiSrcIR
      extends GiapiStatusApply("GpiSrcIR",
                               Instrument.Gpi,
                               GiapiType.Int,
                               "gpi:artificialSourceIR",
                               "gpi:selectSource.sourceIr",
                               Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GpiPolarizerDeplay
      extends GiapiStatusApply("GpiPolarizerDeplay",
                               Instrument.Gpi,
                               GiapiType.Int,
                               "gpi:polarModulatorDeploy",
                               "gpi:configPolarizer.deploy",
                               Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GpiObservationMode
      extends GiapiStatusApply("GpiObservationMode",
                               Instrument.Gpi,
                               GiapiType.String,
                               "gpi:observationMode",
                               "gpi:observationMode.mode",
                               Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GpiIFSFilter
      extends GiapiStatusApply("GpiIFSFilter",
                               Instrument.Gpi,
                               GiapiType.String,
                               "gpi:ifsFilter",
                               "gpi:ifs:selectIfsFilter.maskStr",
                               Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GpiPPM
      extends GiapiStatusApply("GpiPPM",
                               Instrument.Gpi,
                               GiapiType.String,
                               "gpi:ppmMask",
                               "gpi:selectPupilPlaneMask.maskStr",
                               Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GpiFPM
      extends GiapiStatusApply("GpiFPM",
                               Instrument.Gpi,
                               GiapiType.String,
                               "gpi:fpmMask",
                               "gpi:selectFocalPlaneMask.maskStr",
                               Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GpiLyot
      extends GiapiStatusApply("GpiLyot",
                               Instrument.Gpi,
                               GiapiType.String,
                               "gpi:lyotMask",
                               "gpi:selectLyotMask.maskStr",
                               Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GpiAlignAndCalib
      extends GiapiStatusApply("GpiAlignAndCalib",
                               Instrument.Gpi,
                               GiapiType.Int,
                               "gpi:alignAndCalib.part1",
                               "gpi:alignAndCalib.part1",
                               Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GpiIFSReadMode
      extends GiapiStatusApply("GpiIFSReadMode",
                               Instrument.Gpi,
                               GiapiType.Int,
                               "gpi:currentReadMode",
                               "gpi:configIfs.readoutMode",
                               Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GpiIFSStartX
      extends GiapiStatusApply("GpiIFSStartX",
                               Instrument.Gpi,
                               GiapiType.Int,
                               "gpi:currentStartX",
                               "gpi:gpi:configIfs.startx",
                               Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GpiIFSStartY
      extends GiapiStatusApply("GpiIFSStartY",
                               Instrument.Gpi,
                               GiapiType.Int,
                               "gpi:currentStartY",
                               "gpi:gpi:configIfs.starty",
                               Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GpiIFSEndX
      extends GiapiStatusApply("GpiIFSEndX",
                               Instrument.Gpi,
                               GiapiType.Int,
                               "gpi:currentEndX",
                               "gpi:gpi:configIfs.endx",
                               Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GpiIFSEndY
      extends GiapiStatusApply("GpiIFSEndY",
                               Instrument.Gpi,
                               GiapiType.Int,
                               "gpi:currentEndY",
                               "gpi:gpi:configIfs.endy",
                               Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GpiPolarizerAngle
      extends GiapiStatusApply("GpiPolarizerAngle",
                               Instrument.Gpi,
                               GiapiType.Float,
                               "gpi:polarizerAngle",
                               "gpi:configPolarizer.angle",
                               Some(1.0000)
      )

  /** @group Constructors */
  case object GhostAdc1
      extends GiapiStatusApply("GhostAdc1",
                               Instrument.Ghost,
                               GiapiType.String,
                               "ghost:cc:cu:adc1.type",
                               "ghost:cc:cu:adc1.type",
                               None
      )

  /** @group Constructors */
  case object GhostAdc2
      extends GiapiStatusApply("GhostAdc2",
                               Instrument.Ghost,
                               GiapiType.String,
                               "ghost:cc:cu:adc2.type",
                               "ghost:cc:cu:adc2.type",
                               None
      )

  /** @group Constructors */
  case object GhostFiberAgitator1
      extends GiapiStatusApply("GhostFiberAgitator1",
                               Instrument.Ghost,
                               GiapiType.Int,
                               "ghost:sad:cc:slu:fa1.active",
                               "ghost:cc:slu:fa1.type",
                               Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GhostFiberAgitator2
      extends GiapiStatusApply("GhostFiberAgitator2",
                               Instrument.Ghost,
                               GiapiType.Int,
                               "ghost:sad:cc:slu:fa2.active",
                               "ghost:cc:slu:fa2.type",
                               Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GhostRedExposureTime
      extends GiapiStatusApply("GhostRedExposureTime",
                               Instrument.Ghost,
                               GiapiType.Double,
                               "ghost:sad:dc:red.exposedRQ",
                               "ghost:dc:red.duration",
                               Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GhostRedExposureCount
      extends GiapiStatusApply("GhostRedExposureCount",
                               Instrument.Ghost,
                               GiapiType.Int,
                               "ghost:sad:dc:red.repeat",
                               "ghost:dc:red.repeat",
                               Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GhostRedBinningRcf
      extends GiapiStatusApply("GhostRedBinningRcf",
                               Instrument.Ghost,
                               GiapiType.Int,
                               "ghost:sad:dc:red:binx",
                               "ghost:dc:red.rcf",
                               Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GhostRedBinningCcf
      extends GiapiStatusApply("GhostRedBinningCcf",
                               Instrument.Ghost,
                               GiapiType.Int,
                               "ghost:sad:dc:red:biny",
                               "ghost:dc:red.ccf",
                               Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GhostRedUnit
      extends GiapiStatusApply("GhostRedUnit",
                               Instrument.Ghost,
                               GiapiType.Double,
                               "ghost:sad:dc:red.unit",
                               "ghost:dc:red.unit",
                               None
      )

  /** @group Constructors */
  case object GhostRedRequestType
      extends GiapiStatusApply("GhostRedRequestType",
                               Instrument.Ghost,
                               GiapiType.String,
                               "ghost:sad:dc:red.request_type",
                               "ghost:dc:red.request_type",
                               None
      )

  /** @group Constructors */
  case object GhostRedRepeat
      extends GiapiStatusApply("GhostRedRepeat",
                               Instrument.Ghost,
                               GiapiType.Int,
                               "ghost:sad:dc:red.repeat",
                               "ghost:dc:red.repeat",
                               None
      )

  /** @group Constructors */
  case object GhostRedReadMode
      extends GiapiStatusApply("GhostRedReadMode",
                               Instrument.Ghost,
                               GiapiType.Int,
                               "ghost:sad:dc:red.read_mode",
                               "ghost:dc:red.read_mode",
                               None
      )

  /** @group Constructors */
  case object GhostRedImageType
      extends GiapiStatusApply("GhostRedImageType",
                               Instrument.Ghost,
                               GiapiType.String,
                               "ghost:sad:dc:red.imagetyp",
                               "ghost:dc:red.imagetyp",
                               None
      )

  /** @group Constructors */
  case object GhostRedDoDisplay
      extends GiapiStatusApply("GhostRedDoDisplay",
                               Instrument.Ghost,
                               GiapiType.Int,
                               "ghost:sad:dc:red.do_display",
                               "ghost:dc:red.do_display",
                               None
      )

  /** @group Constructors */
  case object GhostRedDoFlush
      extends GiapiStatusApply("GhostRedDoFlush",
                               Instrument.Ghost,
                               GiapiType.Int,
                               "ghost:sad:dc:red.do_flush",
                               "ghost:dc:red.do_flush",
                               None
      )

  /** @group Constructors */
  case object GhostRedDoContinuous
      extends GiapiStatusApply("GhostRedDoContinous",
                               Instrument.Ghost,
                               GiapiType.Int,
                               "ghost:sad:dc:red.do_continuous",
                               "ghost:dc:red.do_continuous",
                               None
      )

  /** @group Constructors */
  case object GhostRedDoReadout
      extends GiapiStatusApply("GhostRedDoReadout",
                               Instrument.Ghost,
                               GiapiType.Int,
                               "ghost:sad:dc:red.do_readout",
                               "ghost:dc:red.do_readout",
                               None
      )

  /** @group Constructors */
  case object GhostRedDoSave
      extends GiapiStatusApply("GhostRedDoSave",
                               Instrument.Ghost,
                               GiapiType.Int,
                               "ghost:sad:dc:red.do_save",
                               "ghost:dc:red.do_save",
                               None
      )

  /** @group Constructors */
  case object GhostRedCcdType
      extends GiapiStatusApply("GhostRedCcdType",
                               Instrument.Ghost,
                               GiapiType.String,
                               "ghost:sad:dc:red.ccd_type",
                               "ghost:dc:red.ccd_type",
                               None
      )

  /** @group Constructors */
  case object GhostRedCcdRequestType
      extends GiapiStatusApply("GhostRedCcdRequestType",
                               Instrument.Ghost,
                               GiapiType.String,
                               "ghost:sad:dc:red.ccd_request_type",
                               "ghost:dc:red.ccd_request_type",
                               None
      )

  /** @group Constructors */
  case object GhostRedDuration
      extends GiapiStatusApply("GhostRedDuration",
                               Instrument.Ghost,
                               GiapiType.Double,
                               "ghost:sad:dc:red.duration",
                               "ghost:dc:red.duration",
                               Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GhostBlueExposureTime
      extends GiapiStatusApply("GhostBlueExposureTime",
                               Instrument.Ghost,
                               GiapiType.Double,
                               "ghost:sad:dc:blue.exposedRQ",
                               "ghost:dc:blue.duration",
                               Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GhostBlueExposureCount
      extends GiapiStatusApply("GhostBlueExposureCount",
                               Instrument.Ghost,
                               GiapiType.Int,
                               "ghost:sad:dc:blue.repeat:GhostBlue.repeat",
                               "ghost:dc:blue.repeat",
                               Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GhostBlueBinningRcf
      extends GiapiStatusApply("GhostBlueBinningRcf",
                               Instrument.Ghost,
                               GiapiType.Int,
                               "ghost:sad:dc:blue:binx",
                               "ghost:dc:blue.rcf",
                               Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GhostBlueBinningCcf
      extends GiapiStatusApply("GhostBlueBinningCcf",
                               Instrument.Ghost,
                               GiapiType.Int,
                               "ghost:sad:dc:blue:biny",
                               "ghost:dc:blue.ccf",
                               Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GhostBlueUnit
      extends GiapiStatusApply("GhostBlueUnit",
                               Instrument.Ghost,
                               GiapiType.Double,
                               "ghost:sad:dc:blue.unit",
                               "ghost:dc:blue.unit",
                               None
      )

  /** @group Constructors */
  case object GhostBlueRequestType
      extends GiapiStatusApply("GhostBlueRequestType",
                               Instrument.Ghost,
                               GiapiType.String,
                               "ghost:sad:dc:blue.request_type",
                               "ghost:dc:blue.request_type",
                               None
      )

  /** @group Constructors */
  case object GhostBlueRepeat
      extends GiapiStatusApply("GhostBlueRepeat",
                               Instrument.Ghost,
                               GiapiType.Int,
                               "ghost:sad:dc:blue.repeat",
                               "ghost:dc:blue.repeat",
                               None
      )

  /** @group Constructors */
  case object GhostBlueReadMode
      extends GiapiStatusApply("GhostBlueReadMode",
                               Instrument.Ghost,
                               GiapiType.Int,
                               "ghost:sad:dc:blue.read_mode",
                               "ghost:dc:blue.read_mode",
                               None
      )

  /** @group Constructors */
  case object GhostBlueImageType
      extends GiapiStatusApply("GhostBlueImageType",
                               Instrument.Ghost,
                               GiapiType.String,
                               "ghost:sad:dc:blue.imagetyp",
                               "ghost:dc:blue.imagetyp",
                               None
      )

  /** @group Constructors */
  case object GhostBlueDoDisplay
      extends GiapiStatusApply("GhostBlueDoDisplay",
                               Instrument.Ghost,
                               GiapiType.Int,
                               "ghost:sad:dc:blue.do_display",
                               "ghost:dc:blue.do_display",
                               None
      )

  /** @group Constructors */
  case object GhostBlueDoFlush
      extends GiapiStatusApply("GhostBlueDoFlush",
                               Instrument.Ghost,
                               GiapiType.Int,
                               "ghost:sad:dc:blue.do_flush",
                               "ghost:dc:blue.do_flush",
                               None
      )

  /** @group Constructors */
  case object GhostBlueDoContinuous
      extends GiapiStatusApply("GhostBlueDoContinous",
                               Instrument.Ghost,
                               GiapiType.Int,
                               "ghost:sad:dc:blue.do_continuous",
                               "ghost:dc:blue.do_continuous",
                               None
      )

  /** @group Constructors */
  case object GhostBlueDoReadout
      extends GiapiStatusApply("GhostBlueDoReadout",
                               Instrument.Ghost,
                               GiapiType.Int,
                               "ghost:sad:dc:blue.do_readout",
                               "ghost:dc:blue.do_readout",
                               None
      )

  /** @group Constructors */
  case object GhostBlueDoSave
      extends GiapiStatusApply("GhostBlueDoSave",
                               Instrument.Ghost,
                               GiapiType.Int,
                               "ghost:sad:dc:blue.do_save",
                               "ghost:dc:blue.do_save",
                               None
      )

  /** @group Constructors */
  case object GhostBlueCcdType
      extends GiapiStatusApply("GhostBlueCcdType",
                               Instrument.Ghost,
                               GiapiType.String,
                               "ghost:sad:dc:blue.ccd_type",
                               "ghost:dc:blue.ccd_type",
                               None
      )

  /** @group Constructors */
  case object GhostBlueCcdRequestType
      extends GiapiStatusApply("GhostBlueCcdRequestType",
                               Instrument.Ghost,
                               GiapiType.String,
                               "ghost:sad:dc:blue.ccd_request_type",
                               "ghost:dc:blue.ccd_request_type",
                               None
      )

  /** @group Constructors */
  case object GhostBlueDuration
      extends GiapiStatusApply("GhostBlueDuration",
                               Instrument.Ghost,
                               GiapiType.Double,
                               "ghost:sad:dc:blue.duration",
                               "ghost:dc:blue.duration",
                               Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GhostIFU1Type
      extends GiapiStatusApply("GhostIFU1Type",
                               Instrument.Ghost,
                               GiapiType.Int,
                               "ghost:sad:cc:cu:ifu1.type",
                               "ghost:cc:cu:ifu1.type",
                               Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GhostIFU1X
      extends GiapiStatusApply("GhostIFU1X",
                               Instrument.Ghost,
                               GiapiType.Double,
                               "ghost:sad:cc:cu:ifu1.fpx",
                               "ghost:cc:cu:ifu1.fpx",
                               Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GhostIFU1Y
      extends GiapiStatusApply("GhostIFU1Y",
                               Instrument.Ghost,
                               GiapiType.Double,
                               "ghost:sad:cc:cu:ifu1.fpy",
                               "ghost:cc:cu:ifu1.fpy",
                               Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GhostIFU1MoveMode
      extends GiapiStatusApply("GhostIFU1MoveMode",
                               Instrument.Ghost,
                               GiapiType.Double,
                               "ghost:sad:cc:cu:ifu1.move_mode",
                               "ghost:cc:cu:ifu1.move_mode",
                               Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GhostIFU2Type
      extends GiapiStatusApply("GhostIFU2Type",
                               Instrument.Ghost,
                               GiapiType.Int,
                               "ghost:sad:cc:cu:ifu2.type",
                               "ghost:cc:cu:ifu2.type",
                               Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GhostIFU2X
      extends GiapiStatusApply("GhostIFU2X",
                               Instrument.Ghost,
                               GiapiType.Double,
                               "ghost:sad:cc:cu:ifu2.fpx",
                               "ghost:cc:cu:ifu2.fpx",
                               Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GhostIFU2Y
      extends GiapiStatusApply("GhostIFU2Y",
                               Instrument.Ghost,
                               GiapiType.Double,
                               "ghost:sad:cc:cu:ifu2.fpy",
                               "ghost:cc:cu:ifu2.fpy",
                               Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GhostIFU2MoveMode
      extends GiapiStatusApply("GhostIFU2MoveMode",
                               Instrument.Ghost,
                               GiapiType.Double,
                               "ghost:sad:cc:cu:ifu2.move_mode",
                               "ghost:cc:cu:ifu2.move_mode",
                               Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GhostIFU1Target
      extends GiapiStatusApply("GhostIFU1Target",
                               Instrument.Ghost,
                               GiapiType.Int,
                               "ghost:sad:cc:cu:ifu1.target",
                               "ghost:cc:cu:ifu1.target",
                               Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GhostIFU2Target
      extends GiapiStatusApply("GhostIFU2Target",
                               Instrument.Ghost,
                               GiapiType.Int,
                               "ghost:sad:cc:cu:ifu2.target",
                               "ghost:cc:cu:ifu2.target",
                               Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GhostIFU1Bundle
      extends GiapiStatusApply("GhostIFU1Bundle",
                               Instrument.Ghost,
                               GiapiType.Int,
                               "ghost:sad:cc:cu:ifu1.bundle",
                               "ghost:cc:cu:ifu1.bundle",
                               Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GhostIFU2Bundle
      extends GiapiStatusApply("GhostIFU2Bundle",
                               Instrument.Ghost,
                               GiapiType.Int,
                               "ghost:sad:cc:cu:ifu2.bundle",
                               "ghost:cc:cu:ifu2.bundle",
                               Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GhostSRIFU1CoordsRADeg
      extends GiapiStatusApply("GhostSRIFU1CoordsRADeg",
                               Instrument.Ghost,
                               GiapiType.Double,
                               "ghost:sad:cc:cu:ifu1.ra",
                               "ghost:cc:cu:ifu1.ra",
                               Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GhostSRIFU1CoordsDecDeg
      extends GiapiStatusApply("GhostSRIFU1CoordsDecDeg",
                               Instrument.Ghost,
                               GiapiType.Double,
                               "ghost:sad:cc:cu:ifu1.dec",
                               "ghost:cc:cu:ifu1.dec",
                               Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GhostSRIFU2CoordsRADeg
      extends GiapiStatusApply("GhostSRIFU2CoordsRADeg",
                               Instrument.Ghost,
                               GiapiType.Double,
                               "ghost:sad:cc:cu:ifu2.ra",
                               "ghost:cc:cu:ifu2.ra",
                               Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GhostSRIFU2CoordsDecDeg
      extends GiapiStatusApply("GhostSRIFU2CoordsDecDeg",
                               Instrument.Ghost,
                               GiapiType.Double,
                               "ghost:sad:cc:cu:ifu2.dec",
                               "ghost:cc:cu:ifu2.dec",
                               Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GhostHRIFU1CoordsRADeg
      extends GiapiStatusApply("GhostHRIFU1CoordsRADeg",
                               Instrument.Ghost,
                               GiapiType.Double,
                               "ghost:sad:cc:cu:ifu1.ra",
                               "ghost:cc:cu:ifu1.ra",
                               Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GhostHRIFU1CoordsDecDeg
      extends GiapiStatusApply("GhostHRIFU1CoordsDecDeg",
                               Instrument.Ghost,
                               GiapiType.Double,
                               "ghost:sad:cc:cu:ifu1.dec",
                               "ghost:cc:cu:ifu1.dec",
                               Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GhostHRIFU2CoordsRADeg
      extends GiapiStatusApply("GhostHRIFU2CoordsRADeg",
                               Instrument.Ghost,
                               GiapiType.Double,
                               "ghost:sad:cc:cu:ifu2.ra",
                               "ghost:cc:cu:ifu2.ra",
                               Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GhostHRIFU2CoordsDecDeg
      extends GiapiStatusApply("GhostHRIFU2CoordsDecDeg",
                               Instrument.Ghost,
                               GiapiType.Double,
                               "ghost:sad:cc:cu:ifu2.dec",
                               "ghost:cc:cu:ifu2.dec",
                               Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GhostUserTarget1Name
      extends GiapiStatusApply(
        "GhostUserTarget1Name",
        Instrument.Ghost,
        GiapiType.String,
        "ghost:cc:cu:targets.target1_name",
        "ghost:cc:cu:targets.target1_name",
        Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GhostUserTarget1CoordsRADeg
      extends GiapiStatusApply(
        "GhostUserTarget1CoordsRADeg",
        Instrument.Ghost,
        GiapiType.Double,
        "ghost:cc:cu:targets.target1_ra",
        "ghost:cc:cu:targets.target1_ra",
        Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GhostUserTarget1CoordsDecDeg
      extends GiapiStatusApply(
        "GhostUserTarget1CoordsDecDeg",
        Instrument.Ghost,
        GiapiType.Double,
        "ghost:cc:cu:targets.target1_dec",
        "ghost:cc:cu:targets.target1_dec",
        Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GhostUserTarget2Name
      extends GiapiStatusApply(
        "GhostUserTarget2Name",
        Instrument.Ghost,
        GiapiType.String,
        "ghost:cc:cu:targets.target2_name",
        "ghost:cc:cu:targets.target2_name",
        Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GhostUserTarget2CoordsRADeg
      extends GiapiStatusApply(
        "GhostUserTarget2CoordsRADeg",
        Instrument.Ghost,
        GiapiType.Double,
        "ghost:cc:cu:targets.target2_ra",
        "ghost:cc:cu:targets.target2_ra",
        Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GhostUserTarget2CoordsDecDeg
      extends GiapiStatusApply(
        "GhostUserTarget2CoordsDecDeg",
        Instrument.Ghost,
        GiapiType.Double,
        "ghost:cc:cu:targets.target2_dec",
        "ghost:cc:cu:targets.target2_dec",
        Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GhostUserTarget3Name
      extends GiapiStatusApply(
        "GhostUserTarget3Name",
        Instrument.Ghost,
        GiapiType.String,
        "ghost:cc:cu:targets.target3_name",
        "ghost:cc:cu:targets.target3_name",
        Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GhostUserTarget3CoordsRADeg
      extends GiapiStatusApply(
        "GhostUserTarget3CoordsRADeg",
        Instrument.Ghost,
        GiapiType.Double,
        "ghost:cc:cu:targets.target3_ra",
        "ghost:cc:cu:targets.target3_ra",
        Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GhostUserTarget3CoordsDecDeg
      extends GiapiStatusApply(
        "GhostUserTarget3CoordsDecDeg",
        Instrument.Ghost,
        GiapiType.Double,
        "ghost:cc:cu:targets.target3_dec",
        "ghost:cc:cu:targets.target3_dec",
        Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GhostUserTarget4Name
      extends GiapiStatusApply(
        "GhostUserTarget4Name",
        Instrument.Ghost,
        GiapiType.String,
        "ghost:cc:cu:targets.target4_name",
        "ghost:cc:cu:targets.target4_name",
        Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GhostUserTarget4CoordsRADeg
      extends GiapiStatusApply(
        "GhostUserTarget4CoordsRADeg",
        Instrument.Ghost,
        GiapiType.Double,
        "ghost:cc:cu:targets.target4_ra",
        "ghost:cc:cu:targets.target4_ra",
        Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GhostUserTarget4CoordsDecDeg
      extends GiapiStatusApply(
        "GhostUserTarget4CoordsDecDeg",
        Instrument.Ghost,
        GiapiType.Double,
        "ghost:cc:cu:targets.target4_dec",
        "ghost:cc:cu:targets.target4_dec",
        Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GhostUserTarget5Name
      extends GiapiStatusApply(
        "GhostUserTarget5Name",
        Instrument.Ghost,
        GiapiType.String,
        "ghost:cc:cu:targets.target5_name",
        "ghost:cc:cu:targets.target5_name",
        Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GhostUserTarget5CoordsRADeg
      extends GiapiStatusApply(
        "GhostUserTarget5CoordsRADeg",
        Instrument.Ghost,
        GiapiType.Double,
        "ghost:cc:cu:targets.target5_ra",
        "ghost:cc:cu:targets.target5_ra",
        Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GhostUserTarget5CoordsDecDeg
      extends GiapiStatusApply(
        "GhostUserTarget5CoordsDecDeg",
        Instrument.Ghost,
        GiapiType.Double,
        "ghost:cc:cu:targets.target5_dec",
        "ghost:cc:cu:targets.target5_dec",
        Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GhostUserTarget6Name
      extends GiapiStatusApply(
        "GhostUserTarget6Name",
        Instrument.Ghost,
        GiapiType.String,
        "ghost:cc:cu:targets.target6_name",
        "ghost:cc:cu:targets.target6_name",
        Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GhostUserTarget6CoordsRADeg
      extends GiapiStatusApply(
        "GhostUserTarget6CoordsRADeg",
        Instrument.Ghost,
        GiapiType.Double,
        "ghost:cc:cu:targets.target6_ra",
        "ghost:cc:cu:targets.target6_ra",
        Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GhostUserTarget6CoordsDecDeg
      extends GiapiStatusApply(
        "GhostUserTarget6CoordsDecDeg",
        Instrument.Ghost,
        GiapiType.Double,
        "ghost:cc:cu:targets.target6_dec",
        "ghost:cc:cu:targets.target6_dec",
        Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GhostUserTarget7Name
      extends GiapiStatusApply(
        "GhostUserTarget7Name",
        Instrument.Ghost,
        GiapiType.String,
        "ghost:cc:cu:targets.target7_name",
        "ghost:cc:cu:targets.target7_name",
        Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GhostUserTarget7CoordsRADeg
      extends GiapiStatusApply(
        "GhostUserTarget7CoordsRADeg",
        Instrument.Ghost,
        GiapiType.Double,
        "ghost:cc:cu:targets.target7_ra",
        "ghost:cc:cu:targets.target7_ra",
        Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GhostUserTarget7CoordsDecDeg
      extends GiapiStatusApply(
        "GhostUserTarget7CoordsDecDeg",
        Instrument.Ghost,
        GiapiType.Double,
        "ghost:cc:cu:targets.target7_dec",
        "ghost:cc:cu:targets.target7_dec",
        Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GhostUserTarget8Name
      extends GiapiStatusApply(
        "GhostUserTarget8Name",
        Instrument.Ghost,
        GiapiType.String,
        "ghost:cc:cu:targets.target8_name",
        "ghost:cc:cu:targets.target8_name",
        Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GhostUserTarget8CoordsRADeg
      extends GiapiStatusApply(
        "GhostUserTarget8CoordsRADeg",
        Instrument.Ghost,
        GiapiType.Double,
        "ghost:cc:cu:targets.target8_ra",
        "ghost:cc:cu:targets.target8_ra",
        Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GhostUserTarget8CoordsDecDeg
      extends GiapiStatusApply(
        "GhostUserTarget8CoordsDecDeg",
        Instrument.Ghost,
        GiapiType.Double,
        "ghost:cc:cu:targets.target8_dec",
        "ghost:cc:cu:targets.target8_dec",
        Option.empty[BigDecimal]
      )

  /** @group Constructors */
  case object GhostUserTargetCount
      extends GiapiStatusApply(
        "GhostUserTargetCount",
        Instrument.Ghost,
        GiapiType.Int,
        "ghost:cc:cu:targets.targets.n_targets",
        "ghost:cc:cu:targets.targets.n_targets",
        Option.empty[BigDecimal]
      )

  // SlitViemw
  /** @group Constructors */
  case object GhostSVCcdRequestType
      extends GiapiStatusApply("GhostSVCcdRequestType",
                               Instrument.Ghost,
                               GiapiType.String,
                               "ghost:sad:dc:sv.ccd_request_type",
                               "ghost:dc:sv.ccd_request_type",
                               None
      )

  /** @group Constructors */
  case object GhostSVRequestType
      extends GiapiStatusApply("GhostSVRequestType",
                               Instrument.Ghost,
                               GiapiType.String,
                               "ghost:sad:dc:sv.request_type",
                               "ghost:dc:sv.request_type",
                               None
      )

  /** @group Constructors */
  case object GhostSVRepeat
      extends GiapiStatusApply("GhostSVRepeat",
                               Instrument.Ghost,
                               GiapiType.Int,
                               "ghost:sad:dc:sv.repeat",
                               "ghost:dc:sv.repeat",
                               None
      )

  /** @group Constructors */
  case object GhostSVRunNumber
      extends GiapiStatusApply("GhostSVRunNumber",
                               Instrument.Ghost,
                               GiapiType.Int,
                               "ghost:sad:dc:sv.run_number",
                               "ghost:dc:sv.run_number",
                               None
      )

  /** @group Constructors */
  case object GhostSVDuration
      extends GiapiStatusApply("GhostSVDuration",
                               Instrument.Ghost,
                               GiapiType.Int,
                               "ghost:sad:dc:sv.duration",
                               "ghost:dc:sv.duration",
                               None
      )

  /** @group Constructors */
  case object GhostSVUnit
      extends GiapiStatusApply("GhostSVUnit",
                               Instrument.Ghost,
                               GiapiType.Double,
                               "ghost:sad:dc:sv.unit",
                               "ghost:dc:sv.unit",
                               None
      )

  /** @group Constructors */
  case object GhostSVDoSave
      extends GiapiStatusApply("GhostSVDoSave",
                               Instrument.Ghost,
                               GiapiType.Int,
                               "ghost:sad:dc:sv.do_save",
                               "ghost:dc:sv.do_save",
                               None
      )

  /** @group Constructors */
  case object GhostSVDoDisplay
      extends GiapiStatusApply("GhostSVDoDisplay",
                               Instrument.Ghost,
                               GiapiType.Int,
                               "ghost:sad:dc:sv.do_display",
                               "ghost:dc:sv.do_display",
                               None
      )

  /** @group Constructors */
  case object GhostSVRcf
      extends GiapiStatusApply("GhostSVRcf",
                               Instrument.Ghost,
                               GiapiType.Int,
                               "ghost:sad:dc:sv.rcf",
                               "ghost:dc:sv.rcf",
                               None
      )

  /** @group Constructors */
  case object GhostSVCcf
      extends GiapiStatusApply("GhostSVCcf",
                               Instrument.Ghost,
                               GiapiType.Int,
                               "ghost:sad:dc:sv.ccf",
                               "ghost:dc:sv.ccf",
                               None
      )

  /** @group Constructors */
  case object GhostSVImageType
      extends GiapiStatusApply("GhostSVImageType",
                               Instrument.Ghost,
                               GiapiType.String,
                               "ghost:sad:dc:sv.imagetyp",
                               "ghost:dc:sv.imagetyp",
                               None
      )

  /** @group Constructors */
  case object GhostSVNRegions
      extends GiapiStatusApply("GhostSVNRegions",
                               Instrument.Ghost,
                               GiapiType.Int,
                               "ghost:sad:dc:sv.nregions",
                               "ghost:dc:sv.nregions",
                               None
      )

  /** @group Constructors */
  case object GhostSVXO
      extends GiapiStatusApply("GhostSVXO",
                               Instrument.Ghost,
                               GiapiType.Int,
                               "ghost:sad:dc:sv.xo",
                               "ghost:dc:sv.xo",
                               None
      )

  /** @group Constructors */
  case object GhostSVYO
      extends GiapiStatusApply("GhostSVYO",
                               Instrument.Ghost,
                               GiapiType.Int,
                               "ghost:sad:dc:sv.yo",
                               "ghost:dc:sv.yo",
                               None
      )

  /** @group Constructors */
  case object GhostSVWidth
      extends GiapiStatusApply("GhostSVWidth",
                               Instrument.Ghost,
                               GiapiType.Int,
                               "ghost:sad:dc:sv.width",
                               "ghost:dc:sv.width",
                               None
      )

  /** @group Constructors */
  case object GhostSVHeigth
      extends GiapiStatusApply("GhostSVHeight",
                               Instrument.Ghost,
                               GiapiType.Int,
                               "ghost:sad:dc:sv.height",
                               "ghost:dc:sv.height",
                               None
      )

  /** @group Constructors */
  case object GhostSVIFU1BlueThreshold
      extends GiapiStatusApply("GhostSVIFU1BlueThreshold",
                               Instrument.Ghost,
                               GiapiType.Double,
                               "ghost:sad:dc:sv.ifu1bluethreshold",
                               "ghost:dc:sv.ifu1bluethreshold",
                               None
      )

  /** @group Constructors */
  case object GhostSVIFU1BlueThresholdEnabled
      extends GiapiStatusApply(
        "GhostSVIFU1BlueThresholdEnabled",
        Instrument.Ghost,
        GiapiType.Int,
        "ghost:sad:dc:sv.ifu1bluethreshold_enabled",
        "ghost:dc:sv.ifu1bluethreshold_enabled",
        None
      )

  /** @group Constructors */
  case object GhostSVIFU1RedThreshold
      extends GiapiStatusApply("GhostSVIFU1RedThreshold",
                               Instrument.Ghost,
                               GiapiType.Double,
                               "ghost:sad:dc:sv.ifu1redthreshold",
                               "ghost:dc:sv.ifu1redthreshold",
                               None
      )

  /** @group Constructors */
  case object GhostSVIFU1RedThresholdEnabled
      extends GiapiStatusApply(
        "GhostSVIFU1RedThresholdEnabled",
        Instrument.Ghost,
        GiapiType.Int,
        "ghost:sad:dc:sv.ifu1redthreshold_enabled",
        "ghost:dc:sv.ifu1redthreshold_enabled",
        None
      )

  /** @group Constructors */
  case object GhostSVIFU2BlueThreshold
      extends GiapiStatusApply("GhostSVIFU2BlueThreshold",
                               Instrument.Ghost,
                               GiapiType.Double,
                               "ghost:sad:dc:sv.ifu2bluethreshold",
                               "ghost:dc:sv.ifu2bluethreshold",
                               None
      )

  /** @group Constructors */
  case object GhostSVIFU2BlueThresholdEnabled
      extends GiapiStatusApply(
        "GhostSVIFU2BlueThresholdEnabled",
        Instrument.Ghost,
        GiapiType.Int,
        "ghost:sad:dc:sv.ifu2bluethreshold_enabled",
        "ghost:dc:sv.ifu2bluethreshold_enabled",
        None
      )

  /** @group Constructors */
  case object GhostSVIFU2RedThreshold
      extends GiapiStatusApply("GhostSVIFU2RedThreshold",
                               Instrument.Ghost,
                               GiapiType.Double,
                               "ghost:sad:dc:sv.ifu2redthreshold",
                               "ghost:dc:sv.ifu2redthreshold",
                               None
      )

  /** @group Constructors */
  case object GhostSVIFU2RedThresholdEnabled
      extends GiapiStatusApply(
        "GhostSVIFU2RedThresholdEnabled",
        Instrument.Ghost,
        GiapiType.Int,
        "ghost:sad:dc:sv.ifu2redthreshold_enabled",
        "ghost:dc:sv.ifu2redthreshold_enabled",
        None
      )

  /** @group Constructors */
  case object GhostSVHIBlueThreshold
      extends GiapiStatusApply("GhostSVHIBlueThreshold",
                               Instrument.Ghost,
                               GiapiType.Double,
                               "ghost:sad:dc:sv.hibluethreshold",
                               "ghost:dc:sv.hibluethreshold",
                               None
      )

  /** @group Constructors */
  case object GhostSVHIBlueThresholdEnabled
      extends GiapiStatusApply("GhostSVHIBlueThresholdEnabled",
                               Instrument.Ghost,
                               GiapiType.Int,
                               "ghost:sad:dc:sv.hibluethreshold_enabled",
                               "ghost:dc:sv.hibluethreshold_enabled",
                               None
      )

  /** @group Constructors */
  case object GhostSVHIRedThreshold
      extends GiapiStatusApply("GhostSVHIRedThreshold",
                               Instrument.Ghost,
                               GiapiType.Double,
                               "ghost:sad:dc:sv.hiredthreshold",
                               "ghost:dc:sv.hiredthreshold",
                               None
      )

  /** @group Constructors */
  case object GhostSVHIRedThresholdEnabled
      extends GiapiStatusApply("GhostSVHIRedThresholdEnabled",
                               Instrument.Ghost,
                               GiapiType.Int,
                               "ghost:sad:dc:sv.hiredthreshold_enabled",
                               "ghost:dc:sv.hiredthreshold_enabled",
                               None
      )

  /** @group Constructors */
  case object GhostSVZeroAccumulatedFlux
      extends GiapiStatusApply("GhostSVZeroAccumulatedFlux",
                               Instrument.Ghost,
                               GiapiType.Int,
                               "ghost:sad:dc:sv.zero_accumulated_flux",
                               "ghost:dc:sv.zero_accumulated_flux",
                               None
      )

  /** @group Constructors */
  case object GhostSVDoContinuous
      extends GiapiStatusApply("GhostSVDoContinuous",
                               Instrument.Ghost,
                               GiapiType.Int,
                               "ghost:sad:dc:sv.do_continuous",
                               "ghost:dc:sv.do_continuous",
                               None
      )

  // Guiding Camera
  /** @group Constructors */
  case object GhostAGCcdRequestType
      extends GiapiStatusApply("GhostAGCcdRequestType",
                               Instrument.Ghost,
                               GiapiType.String,
                               "ghost:sad:dc:ag.ccd_request_type",
                               "ghost:dc:ag.ccd_request_type",
                               None
      )

  /** @group Constructors */
  case object GhostAGRequestType
      extends GiapiStatusApply("GhostAGRequestType",
                               Instrument.Ghost,
                               GiapiType.String,
                               "ghost:sad:dc:ag.request_type",
                               "ghost:dc:ag.request_type",
                               None
      )

  /** @group Constructors */
  case object GhostAGRepeat
      extends GiapiStatusApply("GhostAGRepeat",
                               Instrument.Ghost,
                               GiapiType.Int,
                               "ghost:sad:dc:ag.repeat",
                               "ghost:dc:ag.repeat",
                               None
      )

  /** @group Constructors */
  case object GhostAGDuration
      extends GiapiStatusApply("GhostAGDuration",
                               Instrument.Ghost,
                               GiapiType.Int,
                               "ghost:sad:dc:ag.duration",
                               "ghost:dc:ag.duration",
                               None
      )

  /** @group Constructors */
  case object GhostAGUnit
      extends GiapiStatusApply("GhostAGUnit",
                               Instrument.Ghost,
                               GiapiType.Double,
                               "ghost:sad:dc:ag.unit",
                               "ghost:dc:ag.unit",
                               None
      )

  /** @group Constructors */
  case object GhostAGDoSave
      extends GiapiStatusApply("GhostAGDoSave",
                               Instrument.Ghost,
                               GiapiType.Int,
                               "ghost:sad:dc:ag.do_save",
                               "ghost:dc:ag.do_save",
                               None
      )

  /** @group Constructors */
  case object GhostAGDoDisplay
      extends GiapiStatusApply("GhostAGDoDisplay",
                               Instrument.Ghost,
                               GiapiType.Int,
                               "ghost:sad:dc:ag.do_display",
                               "ghost:dc:ag.do_display",
                               None
      )

  /** @group Constructors */
  case object GhostAGRcf
      extends GiapiStatusApply("GhostAGRcf",
                               Instrument.Ghost,
                               GiapiType.Int,
                               "ghost:sad:dc:ag.rcf",
                               "ghost:dc:ag.rcf",
                               None
      )

  /** @group Constructors */
  case object GhostAGCcf
      extends GiapiStatusApply("GhostAGCcf",
                               Instrument.Ghost,
                               GiapiType.Int,
                               "ghost:sad:dc:ag.ccf",
                               "ghost:dc:ag.ccf",
                               None
      )

  /** @group Constructors */
  case object GhostAGXO
      extends GiapiStatusApply("GhostAGXO",
                               Instrument.Ghost,
                               GiapiType.Int,
                               "ghost:sad:dc:ag.xo",
                               "ghost:dc:ag.xo",
                               None
      )

  /** @group Constructors */
  case object GhostAGYO
      extends GiapiStatusApply("GhostAGYO",
                               Instrument.Ghost,
                               GiapiType.Int,
                               "ghost:sad:dc:ag.yo",
                               "ghost:dc:ag.yo",
                               None
      )

  /** @group Constructors */
  case object GhostAGWidth
      extends GiapiStatusApply("GhostAGWidth",
                               Instrument.Ghost,
                               GiapiType.Int,
                               "ghost:sad:dc:ag.width",
                               "ghost:dc:ag.width",
                               None
      )

  /** @group Constructors */
  case object GhostAGHeigth
      extends GiapiStatusApply("GhostAGHeight",
                               Instrument.Ghost,
                               GiapiType.Int,
                               "ghost:sad:dc:ag.height",
                               "ghost:dc:ag.height",
                               None
      )

  /** @group Constructors */
  case object GhostAGEnableGuide
      extends GiapiStatusApply("GhostAGEnableGuide",
                               Instrument.Ghost,
                               GiapiType.Int,
                               "ghost:sad:dc:ag.enable_guide",
                               "ghost:dc:ag.enable_guide",
                               None
      )

  /** @group Constructors */
  case object GhostAGBackground
      extends GiapiStatusApply("GhostAGBackground",
                               Instrument.Ghost,
                               GiapiType.Int,
                               "ghost:sad:dc:ag.background",
                               "ghost:dc:ag.background",
                               None
      )

  /** @group Constructors */
  case object GhostAGSimulateFlux
      extends GiapiStatusApply("GhostAGSimulateFlux",
                               Instrument.Ghost,
                               GiapiType.Int,
                               "ghost:sad:dc:ag.simulate_flux",
                               "ghost:dc:ag.simulate_flux",
                               None
      )

  /** @group Constructors */
  case object GhostAGDoContinuous
      extends GiapiStatusApply("GhostAGDoContinuous",
                               Instrument.Ghost,
                               GiapiType.Int,
                               "ghost:sad:dc:ag.do_continuous",
                               "ghost:dc:ag.do_continuous",
                               None
      )

  /** @group Constructors */
  case object GhostThXeLamp
      extends GiapiStatusApply("GhostThXeLamp",
                               Instrument.Ghost,
                               GiapiType.Int,
                               "ghost:sad:cc:slu:hclPower.value",
                               "ghost:cc:slu:ins.hclPowerCtl",
                               None
      )

  /** @group Constructors */
  case object GhostSlitMaskPositioner
      extends GiapiStatusApply("GhostSlitMaskPositioner",
                               Instrument.Ghost,
                               GiapiType.String,
                               "ghost:sad:cc:slu:smp.position",
                               "ghost:cc:slu:smp.position",
                               None
      )

  /** @group Constructors */
  case object GhostSlitMaskPositionerType
      extends GiapiStatusApply("GhostSlitMaskPositionerType",
                               Instrument.Ghost,
                               GiapiType.String,
                               "ghost:cc:slu:smp.type",
                               "ghost:cc:slu:smp.type",
                               None
      )

  /** All members of GiapiStatusApply, in canonical order. */
  val all: List[GiapiStatusApply] =
    List(
      GpiAdc,
      GpiUseAo,
      GpiAoOptimize,
      GpiUseCal,
      GpiFpmPinholeBias,
      GpiIntegrationTime,
      GpiNumCoadds,
      GpiMagI,
      GpiMagH,
      GpiCalEntranceShutter,
      GpiCalReferenceShutter,
      GpiCalScienceShutter,
      GpiEntranceShutter,
      GpiCalExitShutter,
      GpiPupilCamera,
      GpiSCPower,
      GpiSCAttenuation,
      GpiSrcVis,
      GpiSrcIR,
      GpiPolarizerDeplay,
      GpiObservationMode,
      GpiIFSFilter,
      GpiPPM,
      GpiFPM,
      GpiLyot,
      GpiAlignAndCalib,
      GpiIFSReadMode,
      GpiIFSStartX,
      GpiIFSStartY,
      GpiIFSEndX,
      GpiIFSEndY,
      GpiPolarizerAngle,
      GhostAdc1,
      GhostAdc2,
      GhostFiberAgitator1,
      GhostFiberAgitator2,
      GhostRedExposureTime,
      GhostRedExposureCount,
      GhostRedBinningRcf,
      GhostRedBinningCcf,
      GhostRedUnit,
      GhostRedRequestType,
      GhostRedRepeat,
      GhostRedReadMode,
      GhostRedImageType,
      GhostRedDoDisplay,
      GhostRedDoFlush,
      GhostRedDoContinuous,
      GhostRedDoReadout,
      GhostRedDoSave,
      GhostRedDoSave,
      GhostRedCcdType,
      GhostRedCcdRequestType,
      GhostRedDuration,
      GhostBlueExposureTime,
      GhostBlueExposureCount,
      GhostBlueBinningRcf,
      GhostBlueBinningCcf,
      GhostBlueUnit,
      GhostBlueRequestType,
      GhostBlueRepeat,
      GhostBlueReadMode,
      GhostBlueImageType,
      GhostBlueDoDisplay,
      GhostBlueDoFlush,
      GhostBlueDoContinuous,
      GhostBlueDoReadout,
      GhostBlueDoSave,
      GhostBlueDoSave,
      GhostBlueCcdType,
      GhostBlueCcdRequestType,
      GhostBlueDuration,
      GhostIFU1Type,
      GhostIFU1X,
      GhostIFU1Y,
      GhostIFU1MoveMode,
      GhostIFU2Type,
      GhostIFU2X,
      GhostIFU2Y,
      GhostIFU2MoveMode,
      GhostIFU1Target,
      GhostIFU2Target,
      GhostIFU1Bundle,
      GhostIFU2Bundle,
      GhostSRIFU1CoordsRADeg,
      GhostSRIFU1CoordsDecDeg,
      GhostSRIFU2CoordsRADeg,
      GhostSRIFU2CoordsDecDeg,
      GhostHRIFU1CoordsRADeg,
      GhostHRIFU1CoordsDecDeg,
      GhostHRIFU2CoordsRADeg,
      GhostHRIFU2CoordsDecDeg,
      GhostUserTarget1Name,
      GhostUserTarget1CoordsRADeg,
      GhostUserTarget1CoordsDecDeg,
      GhostUserTarget2Name,
      GhostUserTarget2CoordsRADeg,
      GhostUserTarget2CoordsDecDeg,
      GhostUserTarget3Name,
      GhostUserTarget3CoordsRADeg,
      GhostUserTarget3CoordsDecDeg,
      GhostUserTarget4Name,
      GhostUserTarget4CoordsRADeg,
      GhostUserTarget4CoordsDecDeg,
      GhostUserTarget5Name,
      GhostUserTarget5CoordsRADeg,
      GhostUserTarget5CoordsDecDeg,
      GhostUserTarget6Name,
      GhostUserTarget6CoordsRADeg,
      GhostUserTarget6CoordsDecDeg,
      GhostUserTarget7Name,
      GhostUserTarget7CoordsRADeg,
      GhostUserTarget7CoordsDecDeg,
      GhostUserTarget8Name,
      GhostUserTarget8CoordsRADeg,
      GhostUserTarget8CoordsDecDeg,
      GhostUserTargetCount,
      GhostSVCcdRequestType,
      GhostSVRequestType,
      GhostSVRepeat,
      GhostSVRunNumber,
      GhostSVDuration,
      GhostSVUnit,
      GhostSVDoSave,
      GhostSVDoDisplay,
      GhostSVRcf,
      GhostSVCcf,
      GhostSVImageType,
      GhostSVNRegions,
      GhostSVXO,
      GhostSVYO,
      GhostSVWidth,
      GhostSVHeigth,
      GhostSVIFU1BlueThreshold,
      GhostSVIFU1BlueThresholdEnabled,
      GhostSVIFU1RedThreshold,
      GhostSVIFU1RedThresholdEnabled,
      GhostSVIFU2BlueThreshold,
      GhostSVIFU2BlueThresholdEnabled,
      GhostSVIFU2RedThreshold,
      GhostSVIFU2RedThresholdEnabled,
      GhostSVHIBlueThreshold,
      GhostSVHIBlueThresholdEnabled,
      GhostSVHIRedThreshold,
      GhostSVHIRedThresholdEnabled,
      GhostSVZeroAccumulatedFlux,
      GhostSVDoContinuous,
      GhostAGCcdRequestType,
      GhostAGRequestType,
      GhostAGRepeat,
      GhostAGDuration,
      GhostAGUnit,
      GhostAGDoSave,
      GhostAGDoDisplay,
      GhostAGRcf,
      GhostAGCcf,
      GhostAGXO,
      GhostAGYO,
      GhostAGWidth,
      GhostAGHeigth,
      GhostAGEnableGuide,
      GhostAGBackground,
      GhostAGSimulateFlux,
      GhostAGDoContinuous,
      GhostThXeLamp,
      GhostSlitMaskPositioner,
      GhostSlitMaskPositionerType
    )

  /** Select the member of GiapiStatusApply with the given tag, if any. */
  def fromTag(s: String): Option[GiapiStatusApply] =
    all.find(_.tag === s)

  /** Select the member of GiapiStatusApply with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): GiapiStatusApply =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"GiapiStatusApply: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  implicit val GiapiStatusApplyEnumerated: Enumerated[GiapiStatusApply] =
    new Enumerated[GiapiStatusApply] {
      def all = GiapiStatusApply.all
      def tag(a: GiapiStatusApply) = a.tag
      override def unsafeFromTag(s: String): GiapiStatusApply =
        GiapiStatusApply.unsafeFromTag(s)
    }

}
