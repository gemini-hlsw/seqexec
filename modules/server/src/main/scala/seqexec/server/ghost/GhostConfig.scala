// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.ghost

import cats.Eq
import cats.syntax.all._
import giapi.client.commands.Configuration
import giapi.client.GiapiConfig
import giapi.client.syntax.all._
import giapi.enums.GiapiStatusApply
import giapi.enums.GiapiStatusApply._
import seqexec.model.Conditions
import seqexec.server.ConfigUtilOps.ContentError
import seqexec.server.ConfigUtilOps.ExtractFailure
import seqexec.server.ghost.implicits._
import lucuma.core.model.{ Target => GemTarget }
import lucuma.core.model.SiderealTracking
import lucuma.core.math.Coordinates
import GhostConfig._
import edu.gemini.spModel.target.env.ResolutionMode
import shapeless.tag
import shapeless.tag.@@

// GHOST has a number of different possible configuration modes: we add types for them here.
sealed trait GhostConfig extends GhostLUT {
  def obsType: String
  def obsClass: String
  def blueConfig: ChannelConfig @@ BlueChannel
  def redConfig: ChannelConfig @@ RedChannel

  def baseCoords: Option[Coordinates]
  def userTargets: List[GemTarget]

  def fiberAgitator1: FiberAgitator
  def fiberAgitator2: FiberAgitator
  def ifu1TargetType: IFUTargetType
  def ifu2TargetType: IFUTargetType
  def ifu1BundleType: BundleConfig
  def ifu1Coordinates: Coordinates
  def ifu2BundleType: Option[BundleConfig]
  def resolutionMode: Option[ResolutionMode]
  def conditions: Conditions
  def scienceMagnitude: Option[Double]
  def coAdds: Option[Int] = None

  def baseConfiguration: Configuration = Configuration.Zero

  def slitMaskConfiguration: Configuration

  def targetConfig(t: GemTarget, i: Int): Configuration =
    // Note the base coordinates are already PM corrected in the OT
    t match {
      case GemTarget.Sidereal(_, SiderealTracking(baseCoordinates, _, _, _, _), _, _) =>
        GhostConfig.UserTargetsApply
          .get(i + 1)
          .map { case (name, ra, dec) =>
            GhostConfig.giapiConfig(name, s""""${t.name.value}"""") |+|
              GhostConfig.giapiConfig(ra, baseCoordinates.ra.toAngle.toDoubleDegrees) |+|
              GhostConfig.giapiConfig(dec, baseCoordinates.dec.toAngle.toSignedDoubleDegrees)
          }
          .combineAll
      case _                                                                          =>
        Configuration.Zero
    }

  def userTargetsConfig: Configuration =
    userTargets.zipWithIndex.map(Function.tupled(targetConfig)).combineAll |+|
      giapiConfig(GiapiStatusApply.GhostUserTargetCount, userTargets.length)

  def ifuNone(ifuNum: IFUNum): Configuration =
    giapiConfig(ifuNum.targetItem, IFUTargetType.NoTarget: IFUTargetType) |+|
      giapiConfig(ifuNum.demandItem, DemandType.DemandNone: DemandType)

  def isDayCal: Boolean = obsClass.toLowerCase match {
    case "daycal" => true
    case _        => false
  }

  def ifuCalibration: Configuration =
    if (isDayCal) {
      giapiConfig(GhostIFU1Target, IFUTargetType.Target("XY"): IFUTargetType) |+|
        giapiConfig(GhostIFU2Target, IFUTargetType.Target("XY"): IFUTargetType) |+|
        giapiConfig(GhostIFU1Type, DemandType.DemandXY: DemandType) |+|
        giapiConfig(GhostIFU2Type, DemandType.DemandXY: DemandType) |+|
        giapiConfig(GhostIFU1MoveMode, "IFU_ABSOLUTE") |+|
        giapiConfig(GhostIFU2MoveMode, "IFU_ABSOLUTE") |+|
        giapiConfig(GhostIFU1X, 55.0) |+|
        giapiConfig(GhostIFU1Y, 0.0) |+|
        giapiConfig(GhostIFU2X, -55.0) |+|
        giapiConfig(GhostIFU2Y, 0.0)
    } else Configuration.Zero

  def ifu1Config: Configuration =
    GhostConfig.ifuConfig(IFUNum.IFU1, ifu1TargetType, ifu1Coordinates, ifu1BundleType)

  def isBias: Boolean = imageTypeConf(obsType).equalsIgnoreCase("BIAS")
  def isFlat: Boolean = imageTypeConf(obsType).equalsIgnoreCase("FLAT")
  def isArc: Boolean  = imageTypeConf(obsType).equalsIgnoreCase("ARC")
  def isDark: Boolean = imageTypeConf(obsType).equalsIgnoreCase("DARK")

  def ifu2Configuration: Configuration

  def adcConfiguration: Configuration

  final def ifu2Config: Configuration =
    ifu2Configuration

  def channelConfig: Configuration =
    giapiConfig(GhostBlueBinningRcf, blueConfig.binning.getSpectralBinning()) |+|
      giapiConfig(GhostBlueBinningCcf, blueConfig.binning.getSpatialBinning()) |+|
      giapiConfig(GhostBlueDuration, blueConfig.exposure.toMillis.toInt) |+|
      giapiConfig(GhostBlueUnit, 0.001) |+|
      giapiConfig(GhostBlueRequestType, "HARDWARE") |+|
      giapiConfig(GhostBlueExposureCount, calcBlueCount(obsType, coAdds, blueConfig)) |+|
      giapiConfig(GhostBlueImageType, imageTypeConf(obsType)) |+|
      giapiConfig(GhostBlueDoDisplay, 1) |+|
      giapiConfig(GhostBlueDoFlush, 1) |+|
      giapiConfig(GhostBlueDoContinuous, 0) |+|
      giapiConfig(GhostBlueDoReadout, 1) |+|
      giapiConfig(GhostBlueDoSave, 1) |+|
      giapiConfig(GhostBlueReadMode, blueConfig.readMode.value) |+|
      giapiConfig(GhostBlueCcdRequestType, "CCD_CAMERA_SET") |+|
      giapiConfig(GhostRedBinningRcf, redConfig.binning.getSpectralBinning()) |+|
      giapiConfig(GhostRedBinningCcf, redConfig.binning.getSpatialBinning()) |+|
      giapiConfig(GhostRedDuration, redConfig.exposure.toMillis.toInt) |+|
      giapiConfig(GhostRedUnit, 0.001) |+|
      giapiConfig(GhostRedRequestType, "HARDWARE") |+|
      giapiConfig(GhostRedExposureCount, calcRedCount(obsType, coAdds, redConfig)) |+|
      giapiConfig(GhostRedImageType, imageTypeConf(obsType)) |+|
      giapiConfig(GhostRedDoDisplay, 1) |+|
      giapiConfig(GhostRedDoFlush, 1) |+|
      giapiConfig(GhostRedDoContinuous, 0) |+|
      giapiConfig(GhostRedDoReadout, 1) |+|
      giapiConfig(GhostRedDoSave, 1) |+|
      giapiConfig(GhostRedReadMode, redConfig.readMode.value) |+|
      giapiConfig(GhostRedCcdRequestType, "CCD_CAMERA_SET")

  def baseSVConfig: Configuration =
    giapiConfig(GhostSVCcdRequestType, "CCD_CAMERA_SET") |+|
      giapiConfig(GhostSVRequestType, "HARDWARE") |+|
      giapiConfig(GhostSVRunNumber, 0) |+|
      giapiConfig(GhostSVDoSave, 1) |+|
      giapiConfig(GhostSVDoDisplay, 1) |+|
      giapiConfig(GhostSVNRegions, 1) |+|
      giapiConfig(GhostSVRcf, 2) |+|
      giapiConfig(GhostSVCcf, 2) |+|
      giapiConfig(GhostSVImageType, imageTypeConf(obsType)) |+|
      giapiConfig(GhostSVXO, 800) |+|
      giapiConfig(GhostSVYO, 680) |+|
      giapiConfig(GhostSVWidth, 300) |+|
      giapiConfig(GhostSVHeigth, 260) |+|
      giapiConfig(GhostSVIFU1BlueThreshold, 0) |+|
      giapiConfig(GhostSVIFU1BlueThresholdEnabled, 0) |+|
      giapiConfig(GhostSVIFU1RedThreshold, 0) |+|
      giapiConfig(GhostSVIFU1RedThresholdEnabled, 0) |+|
      giapiConfig(GhostSVIFU2BlueThreshold, 0) |+|
      giapiConfig(GhostSVIFU2BlueThresholdEnabled, 0) |+|
      giapiConfig(GhostSVIFU2RedThreshold, 0) |+|
      giapiConfig(GhostSVIFU2RedThresholdEnabled, 0) |+|
      giapiConfig(GhostSVHIBlueThreshold, 0) |+|
      giapiConfig(GhostSVHIBlueThresholdEnabled, 0) |+|
      giapiConfig(GhostSVHIRedThreshold, 0) |+|
      giapiConfig(GhostSVHIRedThresholdEnabled, 0) |+|
      giapiConfig(GhostSVZeroAccumulatedFlux, 1) |+|
      giapiConfig(GhostSVDoContinuous, 0)

  def baseAGConfig: Configuration =
    giapiConfig(GhostAGCcdRequestType, "CCD_CAMERA_SET") |+|
      giapiConfig(GhostAGRequestType, "HARDWARE") |+|
      giapiConfig(GhostAGRepeat, 1) |+|
      giapiConfig(GhostAGDoSave, 0) |+|
      giapiConfig(GhostAGDoDisplay, 1) |+|
      giapiConfig(GhostAGRcf, 2) |+|
      giapiConfig(GhostAGCcf, 2) |+|
      giapiConfig(GhostAGXO, 400) |+|
      giapiConfig(GhostAGYO, 300) |+|
      giapiConfig(GhostAGWidth, 128) |+|
      giapiConfig(GhostAGHeigth, 162) |+|
      giapiConfig(GhostAGBackground, 0) |+|
      giapiConfig(GhostAGSimulateFlux, 0) |+|
      giapiConfig(GhostAGDoContinuous, 1)

  val SVDurationFactor = 1000

  def svCalib: Configuration =
    baseSVConfig |+|
      giapiConfig(GhostSVDuration, svCalibExposureTime(obsType).toMillis.toInt) |+|
      giapiConfig(GhostSVRepeat, svCalibSVRepeats(obsType, blueConfig, redConfig, coAdds)) |+|
      giapiConfig(GhostSVUnit, 1.0 / SVDurationFactor)

  def svConfiguration(mag: Option[Double]): Configuration =
    baseSVConfig |+|
      giapiConfig(GhostSVDuration, (svCameraTime(conditions, mag) * SVDurationFactor).toInt) |+|
      giapiConfig(GhostSVRepeat,
                  svCameraRepeats(conditions, mag, this.blueConfig, this.redConfig)
      ) |+|
      giapiConfig(GhostSVUnit, 1.0 / SVDurationFactor)

  val AGDurationFactor = 10

  def agConfiguration(mag: Option[Double]): Configuration =
    baseAGConfig |+|
      giapiConfig(GhostAGDuration, (agCameraTime(conditions, mag) * AGDurationFactor).toInt) |+|
      giapiConfig(GhostAGUnit, 1.0 / AGDurationFactor)

  def thXeLamp: Configuration =
    if (isScience(obsType) && resolutionMode === Some(ResolutionMode.GhostPRV)) {
      giapiConfig(GhostThXeLamp, 1)
    } else {
      giapiConfig(GhostThXeLamp, 0)
    }

  def configuration: Configuration =
    baseConfiguration |+| slitMaskConfiguration |+| (
      if (!isScience(obsType)) {
        ifuCalibration |+| channelConfig |+|
          svCalib |+|
          GhostConfig.fiberConfig1(fiberAgitator1) |+|
          GhostConfig.fiberConfig2(fiberAgitator2)
      } else
        {
          ifu1Config |+| ifu2Config |+|
            GhostConfig.fiberConfig1(FiberAgitator.None) |+|
            GhostConfig.fiberConfig2(FiberAgitator.None)
        } |+|
          userTargetsConfig |+| channelConfig |+| adcConfiguration |+|
          svConfiguration(scienceMagnitude) |+| /* agConfiguration(scienceMagnitude) |+| */ thXeLamp
    ) |+| giapiConfig(GhostSlitMaskPositionerType, "SMP_DEMAND_POSITION")

}

// These are the parameters passed to GHOST from the WDBA.
// We use them to determine the type of configuration being used by GHOST, and instantiate it.
object GhostConfig {

  def giapiConfig[A: GiapiConfig](app: GiapiStatusApply, value: A): Configuration =
    Configuration.single(app.applyItem, value.configValue)

  private[ghost] def ifuConfig(
    ifuNum:        IFUNum,
    ifuTargetType: IFUTargetType,
    coordinates:   Coordinates,
    bundleConfig:  BundleConfig
  ): Configuration = {
    def cfg[P: GiapiConfig](paramName: String, paramVal: P) =
      Configuration.single(s"${ifuNum.configValue}.$paramName", paramVal)

    val demand: DemandType = DemandType.DemandRADec

    val current =
      giapiConfig(ifuNum.targetItem, ifuTargetType) |+|
        giapiConfig(ifuNum.demandItem, demand) |+|
        cfg("ra", coordinates.ra.toAngle.toDoubleDegrees) |+|
        cfg("dec", coordinates.dec.toAngle.toSignedDoubleDegrees) |+|
        giapiConfig(ifuNum.bundleItem, bundleConfig)
    current
  }

  val UserTargetsApply: Map[Int, (GiapiStatusApply, GiapiStatusApply, GiapiStatusApply)] =
    Map(
      1 -> ((GhostUserTarget1Name, GhostUserTarget1CoordsRADeg, GhostUserTarget1CoordsDecDeg)),
      2 -> ((GhostUserTarget2Name, GhostUserTarget2CoordsRADeg, GhostUserTarget2CoordsDecDeg)),
      3 -> ((GhostUserTarget3Name, GhostUserTarget3CoordsRADeg, GhostUserTarget3CoordsDecDeg)),
      4 -> ((GhostUserTarget4Name, GhostUserTarget4CoordsRADeg, GhostUserTarget4CoordsDecDeg)),
      5 -> ((GhostUserTarget5Name, GhostUserTarget5CoordsRADeg, GhostUserTarget5CoordsDecDeg)),
      6 -> ((GhostUserTarget6Name, GhostUserTarget6CoordsRADeg, GhostUserTarget6CoordsDecDeg)),
      7 -> ((GhostUserTarget7Name, GhostUserTarget7CoordsRADeg, GhostUserTarget7CoordsDecDeg)),
      8 -> ((GhostUserTarget8Name, GhostUserTarget8CoordsRADeg, GhostUserTarget8CoordsDecDeg))
    )

  private[ghost] def ifuPark(ifuNum: IFUNum): Configuration =
    giapiConfig(ifuNum.targetItem, IFUTargetType.NoTarget: IFUTargetType) |+|
      giapiConfig(ifuNum.demandItem, DemandType.DemandPark: DemandType)

  private[ghost] def fiberConfig1(fa: FiberAgitator): Configuration =
    giapiConfig(GhostFiberAgitator1, fa)

  private[ghost] def fiberConfig2(fa: FiberAgitator): Configuration =
    giapiConfig(GhostFiberAgitator2, fa)

  def apply(
    obsType:          String,
    obsClass:         String,
    blueConfig:       ChannelConfig @@ BlueChannel,
    redConfig:        ChannelConfig @@ RedChannel,
    baseCoords:       Option[Coordinates],
    fiberAgitator1:   FiberAgitator,
    fiberAgitator2:   FiberAgitator,
    srifu1Name:       Option[String],
    srifu1Coords:     Option[Coordinates],
    srifu2Name:       Option[String],
    srifu2Coords:     Option[Coordinates],
    hrifu1Name:       Option[String],
    hrifu1Coords:     Option[Coordinates],
    hrifu2Name:       Option[String],
    hrifu2Coords:     Option[Coordinates],
    userTargets:      List[GemTarget],
    resolutionMode:   Option[ResolutionMode],
    conditions:       Conditions,
    scienceMagnitude: Option[Double]
  ): Either[ExtractFailure, GhostConfig] = {
    import IFUTargetType._

    val sifu1 = determineType(srifu1Name)
    val sifu2 = determineType(srifu2Name)
    val hifu1 = determineType(hrifu1Name)
    val hifu2 = determineType(hrifu2Name)

    val extracted = (sifu1, sifu2, hifu1, hifu2) match {
      case (Target(t), NoTarget, NoTarget, NoTarget)    =>
        srifu1Coords.map(
          StandardResolutionMode
            .SingleTarget(obsType,
                          obsClass,
                          blueConfig,
                          redConfig,
                          baseCoords,
                          fiberAgitator1,
                          fiberAgitator2,
                          t,
                          _,
                          userTargets,
                          resolutionMode,
                          conditions,
                          scienceMagnitude
            )
        )
      case (NoTarget, Target(t), NoTarget, NoTarget)    =>
        srifu2Coords.map(
          StandardResolutionMode
            .SingleTarget(obsType,
                          obsClass,
                          blueConfig,
                          redConfig,
                          baseCoords,
                          fiberAgitator1,
                          fiberAgitator2,
                          t,
                          _,
                          userTargets,
                          resolutionMode,
                          conditions,
                          scienceMagnitude
            )
        )
      case (Target(t1), Target(t2), NoTarget, NoTarget) =>
        (srifu1Coords, srifu2Coords).mapN(
          StandardResolutionMode
            .DualTarget(
              obsType,
              obsClass,
              tag[BlueChannel][ChannelConfig](blueConfig),
              tag[RedChannel][ChannelConfig](redConfig),
              baseCoords,
              fiberAgitator1,
              fiberAgitator2,
              t1,
              _,
              t2,
              _,
              userTargets,
              resolutionMode,
              conditions,
              scienceMagnitude
            )
        )
      case (Target(t), SkyPosition, NoTarget, NoTarget) =>
        (srifu1Coords, srifu2Coords).mapN(
          StandardResolutionMode
            .TargetPlusSky(obsType,
                           obsClass,
                           blueConfig,
                           redConfig,
                           baseCoords,
                           fiberAgitator1,
                           fiberAgitator2,
                           t,
                           _,
                           _,
                           userTargets,
                           resolutionMode,
                           conditions,
                           scienceMagnitude
            )
        )
      case (SkyPosition, Target(t), NoTarget, NoTarget) =>
        (srifu1Coords, srifu2Coords).mapN(
          StandardResolutionMode
            .SkyPlusTarget(obsType,
                           obsClass,
                           blueConfig,
                           redConfig,
                           baseCoords,
                           fiberAgitator1,
                           fiberAgitator2,
                           _,
                           t,
                           _,
                           userTargets,
                           resolutionMode,
                           conditions,
                           scienceMagnitude
            )
        )
      case (NoTarget, NoTarget, Target(t), SkyPosition) =>
        (hrifu1Coords, hrifu2Coords).mapN(
          HighResolutionMode
            .TargetPlusSky(obsType,
                           obsClass,
                           blueConfig,
                           redConfig,
                           baseCoords,
                           fiberAgitator1,
                           fiberAgitator2,
                           t,
                           _,
                           _,
                           userTargets,
                           resolutionMode,
                           conditions,
                           scienceMagnitude
            )
        )
      case _                                            =>
        None
    }

    extracted.toRight(ContentError("Response does not constitute a valid GHOST configuration"))
  }

  implicit val eq: Eq[GhostConfig] = Eq.instance {
    case (a: StandardResolutionMode.SingleTarget, b: StandardResolutionMode.SingleTarget)   => a === b
    case (a: StandardResolutionMode.DualTarget, b: StandardResolutionMode.DualTarget)       => a === b
    case (a: StandardResolutionMode.TargetPlusSky, b: StandardResolutionMode.TargetPlusSky) =>
      a === b
    case (a: StandardResolutionMode.SkyPlusTarget, b: StandardResolutionMode.SkyPlusTarget) =>
      a === b
    case (a: HighResolutionMode.TargetPlusSky, b: HighResolutionMode.TargetPlusSky)         => a === b
    case _                                                                                  => false
  }

}

case class GhostCalibration(
  override val obsType:        String,
  override val obsClass:       String,
  override val blueConfig:     ChannelConfig @@ BlueChannel,
  override val redConfig:      ChannelConfig @@ RedChannel,
  override val baseCoords:     Option[Coordinates],
  override val fiberAgitator1: FiberAgitator,
  override val fiberAgitator2: FiberAgitator,
  override val resolutionMode: Option[ResolutionMode],
  override val conditions:     Conditions,
  override val coAdds:         Option[Int],
  val isHR:                    Boolean
) extends GhostConfig {

  override val slitMaskConfiguration: Configuration =
    if (isHR) giapiConfig(GhostSlitMaskPositioner, "SMP_HI_ONLY")
    else giapiConfig(GhostSlitMaskPositioner, "SMP_STD_ONLY")

  override val baseConfiguration: Configuration =
    Configuration.Zero

  override def ifu1TargetType: IFUTargetType =
    IFUTargetType.NoTarget

  override def ifu2TargetType: IFUTargetType =
    IFUTargetType.NoTarget

  override def ifu1BundleType: BundleConfig =
    BundleConfig.Standard

  override def ifu2BundleType: Option[BundleConfig] =
    None

  override def ifu2Configuration: Configuration = Configuration.Zero

  override val ifu1Coordinates: Coordinates = Coordinates.Zero

  override val userTargets: List[GemTarget] = Nil

  override val scienceMagnitude: Option[Double] = None

  def adcConfiguration: Configuration = Configuration.Zero

}

sealed trait StandardResolutionMode extends GhostConfig {
  import StandardResolutionMode._

  def ifu1Coordinates: Coordinates

  override val slitMaskConfiguration: Configuration =
    giapiConfig(GhostSlitMaskPositioner, "SMP_STD_ONLY")

  override def ifu1TargetType: IFUTargetType =
    this match {
      case s: SingleTarget   => IFUTargetType.Target(s.ifu1TargetName)
      case d: DualTarget     => IFUTargetType.Target(d.ifu1TargetName)
      case ts: TargetPlusSky => IFUTargetType.Target(ts.ifu1TargetName)
      case _: SkyPlusTarget  => IFUTargetType.SkyPosition
    }

  override def ifu2TargetType: IFUTargetType =
    this match {
      case _: SingleTarget   => IFUTargetType.NoTarget
      case d: DualTarget     => IFUTargetType.Target(d.ifu2TargetName)
      case st: SkyPlusTarget => IFUTargetType.Target(st.ifu2TargetName)
      case _: TargetPlusSky  => IFUTargetType.SkyPosition
    }

  override def ifu1BundleType: BundleConfig =
    this match {
      case _: SingleTarget | _: DualTarget | _: TargetPlusSky => BundleConfig.Standard
      case _: SkyPlusTarget                                   => BundleConfig.Sky
    }

  override def ifu2BundleType: Option[BundleConfig] =
    this match {
      case _: SingleTarget                  => None
      case _: DualTarget | _: SkyPlusTarget => Some(BundleConfig.Standard)
      case _: TargetPlusSky                 => Some(BundleConfig.Sky)
    }

  def adcConfiguration: Configuration =
    giapiConfig(GhostAdc1, "ADC_DEMAND_TRACK") |+|
      giapiConfig(GhostAdc2, "ADC_DEMAND_TRACK")
}

object StandardResolutionMode {
  final case class SingleTarget(
    override val obsType:          String,
    override val obsClass:         String,
    override val blueConfig:       ChannelConfig @@ BlueChannel,
    override val redConfig:        ChannelConfig @@ RedChannel,
    override val baseCoords:       Option[Coordinates],
    override val fiberAgitator1:   FiberAgitator,
    override val fiberAgitator2:   FiberAgitator,
    ifu1TargetName:                String,
    override val ifu1Coordinates:  Coordinates,
    override val userTargets:      List[GemTarget],
    override val resolutionMode:   Option[ResolutionMode],
    override val conditions:       Conditions,
    override val scienceMagnitude: Option[Double]
  ) extends StandardResolutionMode {
    override def ifu2Configuration: Configuration =
      GhostConfig.ifuPark(IFUNum.IFU2)
  }

  implicit val srmSingleTargetEq: Eq[SingleTarget] = Eq.by(x =>
    (x.obsType,
     x.blueConfig: ChannelConfig,
     x.redConfig: ChannelConfig,
     x.baseCoords,
     x.fiberAgitator1,
     x.fiberAgitator2,
     x.ifu1TargetName,
     x.ifu1Coordinates,
     x.userTargets,
     x.resolutionMode,
     x.conditions,
     x.scienceMagnitude
    )
  )

  final case class DualTarget(
    override val obsType:          String,
    override val obsClass:         String,
    override val blueConfig:       ChannelConfig @@ BlueChannel,
    override val redConfig:        ChannelConfig @@ RedChannel,
    override val baseCoords:       Option[Coordinates],
    override val fiberAgitator1:   FiberAgitator,
    override val fiberAgitator2:   FiberAgitator,
    ifu1TargetName:                String,
    override val ifu1Coordinates:  Coordinates,
    ifu2TargetName:                String,
    ifu2Coordinates:               Coordinates,
    override val userTargets:      List[GemTarget],
    override val resolutionMode:   Option[ResolutionMode],
    override val conditions:       Conditions,
    override val scienceMagnitude: Option[Double]
  ) extends StandardResolutionMode {
    override def ifu2Configuration: Configuration =
      GhostConfig.ifuConfig(IFUNum.IFU2,
                            IFUTargetType.Target(ifu2TargetName),
                            ifu2Coordinates,
                            BundleConfig.Standard
      )
  }

  implicit val srmDualTargetEq: Eq[DualTarget] = Eq.by(x =>
    (x.obsType,
     x.blueConfig: ChannelConfig,
     x.redConfig: ChannelConfig,
     x.baseCoords,
     x.fiberAgitator1,
     x.fiberAgitator2,
     x.ifu1TargetName,
     x.ifu1Coordinates,
     x.ifu2TargetName,
     x.ifu2Coordinates,
     x.userTargets,
     x.resolutionMode,
     x.conditions,
     x.scienceMagnitude
    )
  )

  final case class TargetPlusSky(
    override val obsType:          String,
    override val obsClass:         String,
    override val blueConfig:       ChannelConfig @@ BlueChannel,
    override val redConfig:        ChannelConfig @@ RedChannel,
    override val baseCoords:       Option[Coordinates],
    override val fiberAgitator1:   FiberAgitator,
    override val fiberAgitator2:   FiberAgitator,
    ifu1TargetName:                String,
    override val ifu1Coordinates:  Coordinates,
    ifu2Coordinates:               Coordinates,
    override val userTargets:      List[GemTarget],
    override val resolutionMode:   Option[ResolutionMode],
    override val conditions:       Conditions,
    override val scienceMagnitude: Option[Double]
  ) extends StandardResolutionMode {
    override def ifu2Configuration: Configuration =
      GhostConfig.ifuConfig(IFUNum.IFU2,
                            IFUTargetType.SkyPosition,
                            ifu2Coordinates,
                            BundleConfig.Sky
      )
  }

  implicit val srmTargetPlusSkyEq: Eq[TargetPlusSky] = Eq.by(x =>
    (x.obsType,
     x.blueConfig: ChannelConfig,
     x.redConfig: ChannelConfig,
     x.baseCoords,
     x.fiberAgitator1,
     x.fiberAgitator2,
     x.ifu1TargetName,
     x.ifu1Coordinates,
     x.ifu2Coordinates,
     x.userTargets,
     x.resolutionMode,
     x.conditions,
     x.scienceMagnitude
    )
  )

  final case class SkyPlusTarget(
    override val obsType:          String,
    override val obsClass:         String,
    override val blueConfig:       ChannelConfig @@ BlueChannel,
    override val redConfig:        ChannelConfig @@ RedChannel,
    override val baseCoords:       Option[Coordinates],
    override val fiberAgitator1:   FiberAgitator,
    override val fiberAgitator2:   FiberAgitator,
    override val ifu1Coordinates:  Coordinates,
    ifu2TargetName:                String,
    ifu2Coordinates:               Coordinates,
    override val userTargets:      List[GemTarget],
    override val resolutionMode:   Option[ResolutionMode],
    override val conditions:       Conditions,
    override val scienceMagnitude: Option[Double]
  ) extends StandardResolutionMode {
    override def ifu2Configuration: Configuration =
      GhostConfig.ifuConfig(IFUNum.IFU2,
                            IFUTargetType.Target(ifu2TargetName),
                            ifu2Coordinates,
                            BundleConfig.Standard
      )

  }

  implicit val srmSkyPlusTargetEq: Eq[SkyPlusTarget] = Eq.by(x =>
    (x.obsType,
     x.blueConfig: ChannelConfig,
     x.redConfig: ChannelConfig,
     x.baseCoords,
     x.fiberAgitator1,
     x.fiberAgitator2,
     x.ifu1Coordinates,
     x.ifu2TargetName,
     x.ifu2Coordinates,
     x.userTargets,
     x.resolutionMode,
     x.conditions,
     x.scienceMagnitude
    )
  )
}

sealed trait HighResolutionMode extends GhostConfig {

  override val slitMaskConfiguration: Configuration =
    giapiConfig(GhostSlitMaskPositioner, "SMP_HI_ONLY")

  override def ifu1BundleType: BundleConfig = BundleConfig.HighRes

  def ifu1TargetName: String

  override def ifu1TargetType: IFUTargetType = IFUTargetType.Target(ifu1TargetName)

  override def ifu2TargetType: IFUTargetType =
    IFUTargetType.SkyPosition

  override def ifu2BundleType: Option[BundleConfig] =
    Some(BundleConfig.Sky)

  def adcConfiguration: Configuration =
    giapiConfig(GhostAdc1, "ADC_DEMAND_TRACK") |+|
      giapiConfig(GhostAdc2, "ADC_DEMAND_TRACK")
}

object HighResolutionMode {
  final case class TargetPlusSky(
    override val obsType:          String,
    override val obsClass:         String,
    override val blueConfig:       ChannelConfig @@ BlueChannel,
    override val redConfig:        ChannelConfig @@ RedChannel,
    override val baseCoords:       Option[Coordinates],
    override val fiberAgitator1:   FiberAgitator,
    override val fiberAgitator2:   FiberAgitator,
    override val ifu1TargetName:   String,
    override val ifu1Coordinates:  Coordinates,
    ifu2Coordinates:               Coordinates,
    override val userTargets:      List[GemTarget],
    override val resolutionMode:   Option[ResolutionMode],
    override val conditions:       Conditions,
    override val scienceMagnitude: Option[Double]
  ) extends HighResolutionMode {
    override def ifu2Configuration: Configuration =
      GhostConfig.ifuConfig(IFUNum.IFU2,
                            IFUTargetType.SkyPosition,
                            ifu2Coordinates,
                            BundleConfig.Sky
      )
  }

  implicit val hrTargetPlusSkyEq: Eq[TargetPlusSky] = Eq.by(x =>
    (x.obsType,
     x.obsClass,
     x.blueConfig: ChannelConfig,
     x.redConfig: ChannelConfig,
     x.baseCoords,
     x.fiberAgitator1,
     x.fiberAgitator2,
     x.ifu1TargetName,
     x.ifu1Coordinates,
     x.ifu2Coordinates,
     x.userTargets,
     x.resolutionMode,
     x.conditions,
     x.scienceMagnitude
    )
  )
}
