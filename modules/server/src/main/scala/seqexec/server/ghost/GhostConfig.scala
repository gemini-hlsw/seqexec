// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.ghost

import cats.Eq
import cats.syntax.all._
import giapi.client.commands.Configuration
import giapi.client.GiapiConfig
import giapi.client.syntax.all._
import seqexec.server.ConfigUtilOps.ContentError
import seqexec.server.ConfigUtilOps.ExtractFailure
import lucuma.core.model.{ Target => GemTarget }
import lucuma.core.math.Coordinates
import lucuma.core.enum.GiapiStatusApply
import lucuma.core.enum.GiapiStatusApply._
import GhostConfig._

// GHOST has a number of different possible configuration modes: we add types for them here.
sealed trait GhostConfig {
  def obsType: String
  def blueConfig: ChannelConfig
  def redConfig: ChannelConfig

  def baseCoords: Option[Coordinates]
  def userTargets: List[GemTarget]

  def fiberAgitator1: FiberAgitator
  def fiberAgitator2: FiberAgitator
  def ifu1TargetType: IFUTargetType
  def ifu2TargetType: IFUTargetType
  def ifu1BundleType: BundleConfig
  def ifu1Coordinates: Coordinates
  def ifu2BundleType: Option[BundleConfig]

  def targetConfig(t: GemTarget, i: Int): Configuration =
    // Note the base coordinates are already PM corrected in the OT
    t.track
      .map(_.baseCoordinates)
      .map { c =>
        GhostConfig.UserTargetsApply
          .get(i + 1)
          .map { case (name, ra, dec) =>
            GhostConfig.giapiConfig(name, t.name.value) |+|
              GhostConfig.giapiConfig(ra, c.ra.toAngle.toDoubleDegrees) |+|
              GhostConfig.giapiConfig(dec, c.dec.toAngle.toSignedDoubleDegrees)
          }
          .combineAll
      }
      .getOrElse(Configuration.Zero)

  def userTargetsConfig: Configuration =
    userTargets.zipWithIndex.map(Function.tupled(targetConfig)).combineAll |+|
      giapiConfig(GiapiStatusApply.GhostUserTargetCount, userTargets.length)

  def ifuNone(ifuNum: IFUNum): Configuration =
    giapiConfig(ifuNum.targetItem, IFUTargetType.NoTarget: IFUTargetType) |+|
      giapiConfig(ifuNum.demandItem, DemandType.DemandNone: DemandType)

  def ifu1Config: Configuration =
    if (isScience)
      GhostConfig.ifuConfig(IFUNum.IFU1, ifu1TargetType, ifu1Coordinates, ifu1BundleType)
    else
      ifuNone(IFUNum.IFU1)

  def imageTypeConf = obsType.toLowerCase match {
    case "bias" => "BIAS"
    case "flat" => "FLAT"
    case "dark" => "DARK"
    case _      => "OBJECT"
  }

  def isScience: Boolean = obsType.equalsIgnoreCase("object")

  def ifu2Configuration: Configuration

  final def ifu2Config: Configuration =
    if (isScience)
      ifu2Configuration
    else
      ifuNone(IFUNum.IFU2)

  def channelConfig: Configuration =
    giapiConfig(GhostBlueBinningRcf, blueConfig.binning.getSpectralBinning()) |+|
      giapiConfig(GhostBlueBinningCcf, blueConfig.binning.getSpatialBinning()) |+|
      giapiConfig(GhostBlueDuration, blueConfig.exposure.toMillis.toInt) |+|
      giapiConfig(GhostBlueUnit, 0.001) |+|
      giapiConfig(GhostBlueRequestType, "HARDWARE") |+|
      giapiConfig(GhostBlueExposureCount, blueConfig.count) |+|
      giapiConfig(GhostBlueImageType, imageTypeConf) |+|
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
      giapiConfig(GhostRedExposureCount, redConfig.count) |+|
      giapiConfig(GhostRedImageType, imageTypeConf) |+|
      giapiConfig(GhostRedDoDisplay, 1) |+|
      giapiConfig(GhostRedDoFlush, 1) |+|
      giapiConfig(GhostRedDoContinuous, 0) |+|
      giapiConfig(GhostRedDoReadout, 1) |+|
      giapiConfig(GhostRedDoSave, 1) |+|
      giapiConfig(GhostRedReadMode, redConfig.readMode.value) |+|
      giapiConfig(GhostRedCcdRequestType, "CCD_CAMERA_SET")

  def configuration: Configuration =
    if (isScience) {
      GhostConfig.fiberConfig1(fiberAgitator1) |+|
        GhostConfig.fiberConfig2(fiberAgitator2)
    } else {
      GhostConfig.fiberConfig1(FiberAgitator.None) |+|
        GhostConfig.fiberConfig2(FiberAgitator.None)
    } |+|
      /*ifu1Config |+| ifu2Config |+| */ userTargetsConfig |+| channelConfig

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
    // guideConfig:   Option[]
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
    obsType:        String,
    blueConfig:     ChannelConfig,
    redConfig:      ChannelConfig,
    baseCoords:     Option[Coordinates],
    fiberAgitator1: FiberAgitator,
    fiberAgitator2: FiberAgitator,
    srifu1Name:     Option[String],
    srifu1Coords:   Option[Coordinates],
    srifu2Name:     Option[String],
    srifu2Coords:   Option[Coordinates],
    hrifu1Name:     Option[String],
    hrifu1Coords:   Option[Coordinates],
    hrifu2Name:     Option[String],
    hrifu2Coords:   Option[Coordinates],
    userTargets:    List[GemTarget]
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
                          blueConfig,
                          redConfig,
                          baseCoords,
                          fiberAgitator1,
                          fiberAgitator2,
                          t,
                          _,
                          userTargets
            )
        )
      case (Target(t1), Target(t2), NoTarget, NoTarget) =>
        (srifu1Coords, srifu2Coords).mapN(
          StandardResolutionMode
            .DualTarget(obsType,
                        blueConfig,
                        redConfig,
                        baseCoords,
                        fiberAgitator1,
                        fiberAgitator2,
                        t1,
                        _,
                        t2,
                        _,
                        userTargets
            )
        )
      case (Target(t), SkyPosition, NoTarget, NoTarget) =>
        (srifu1Coords, srifu2Coords).mapN(
          StandardResolutionMode
            .TargetPlusSky(obsType,
                           blueConfig,
                           redConfig,
                           baseCoords,
                           fiberAgitator1,
                           fiberAgitator2,
                           t,
                           _,
                           _,
                           userTargets
            )
        )
      case (SkyPosition, Target(t), NoTarget, NoTarget) =>
        (srifu1Coords, srifu2Coords).mapN(
          StandardResolutionMode
            .SkyPlusTarget(obsType,
                           blueConfig,
                           redConfig,
                           baseCoords,
                           fiberAgitator1,
                           fiberAgitator2,
                           _,
                           t,
                           _,
                           userTargets
            )
        )
      case (NoTarget, NoTarget, Target(t), NoTarget)    =>
        hrifu1Coords.map(
          HighResolutionMode.SingleTarget(obsType,
                                          blueConfig,
                                          redConfig,
                                          baseCoords,
                                          fiberAgitator1,
                                          fiberAgitator2,
                                          t,
                                          _,
                                          userTargets
          )
        )
      case (NoTarget, NoTarget, Target(t), SkyPosition) =>
        (hrifu1Coords, hrifu2Coords).mapN(
          HighResolutionMode
            .TargetPlusSky(obsType,
                           blueConfig,
                           redConfig,
                           baseCoords,
                           fiberAgitator1,
                           fiberAgitator2,
                           t,
                           _,
                           _,
                           userTargets
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
    case (a: HighResolutionMode.SingleTarget, b: HighResolutionMode.SingleTarget)           => a === b
    case (a: HighResolutionMode.TargetPlusSky, b: HighResolutionMode.TargetPlusSky)         => a === b
    case _                                                                                  => false
  }

}

case class GhostCalibration(
  override val obsType:        String,
  override val blueConfig:     ChannelConfig,
  override val redConfig:      ChannelConfig,
  override val baseCoords:     Option[Coordinates],
  override val fiberAgitator1: FiberAgitator,
  override val fiberAgitator2: FiberAgitator
) extends GhostConfig {

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

}

sealed trait StandardResolutionMode extends GhostConfig {
  import StandardResolutionMode._

  def ifu1Coordinates: Coordinates

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
}

object StandardResolutionMode {
  final case class SingleTarget(
    override val obsType:         String,
    override val blueConfig:      ChannelConfig,
    override val redConfig:       ChannelConfig,
    override val baseCoords:      Option[Coordinates],
    override val fiberAgitator1:  FiberAgitator,
    override val fiberAgitator2:  FiberAgitator,
    ifu1TargetName:               String,
    override val ifu1Coordinates: Coordinates,
    override val userTargets:     List[GemTarget]
  ) extends StandardResolutionMode {
    override def ifu2Configuration: Configuration =
      GhostConfig.ifuPark(IFUNum.IFU2)
  }

  implicit val srmSingleTargetEq: Eq[SingleTarget] = Eq.by(x =>
    (x.obsType,
     x.blueConfig,
     x.redConfig,
     x.baseCoords,
     x.fiberAgitator1,
     x.fiberAgitator2,
     x.ifu1TargetName,
     x.ifu1Coordinates
    )
  )

  final case class DualTarget(
    override val obsType:         String,
    override val blueConfig:      ChannelConfig,
    override val redConfig:       ChannelConfig,
    override val baseCoords:      Option[Coordinates],
    override val fiberAgitator1:  FiberAgitator,
    override val fiberAgitator2:  FiberAgitator,
    ifu1TargetName:               String,
    override val ifu1Coordinates: Coordinates,
    ifu2TargetName:               String,
    ifu2Coordinates:              Coordinates,
    override val userTargets:     List[GemTarget]
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
     x.blueConfig,
     x.redConfig,
     x.baseCoords,
     x.fiberAgitator1,
     x.fiberAgitator2,
     x.ifu1TargetName,
     x.ifu1Coordinates,
     x.ifu2TargetName,
     x.ifu2Coordinates
    )
  )

  final case class TargetPlusSky(
    override val obsType:         String,
    override val blueConfig:      ChannelConfig,
    override val redConfig:       ChannelConfig,
    override val baseCoords:      Option[Coordinates],
    override val fiberAgitator1:  FiberAgitator,
    override val fiberAgitator2:  FiberAgitator,
    ifu1TargetName:               String,
    override val ifu1Coordinates: Coordinates,
    ifu2Coordinates:              Coordinates,
    override val userTargets:     List[GemTarget]
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
     x.blueConfig,
     x.redConfig,
     x.baseCoords,
     x.fiberAgitator1,
     x.fiberAgitator2,
     x.ifu1TargetName,
     x.ifu1Coordinates,
     x.ifu2Coordinates
    )
  )

  final case class SkyPlusTarget(
    override val obsType:         String,
    override val blueConfig:      ChannelConfig,
    override val redConfig:       ChannelConfig,
    override val baseCoords:      Option[Coordinates],
    override val fiberAgitator1:  FiberAgitator,
    override val fiberAgitator2:  FiberAgitator,
    override val ifu1Coordinates: Coordinates,
    ifu2TargetName:               String,
    ifu2Coordinates:              Coordinates,
    override val userTargets:     List[GemTarget]
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
     x.blueConfig,
     x.redConfig,
     x.baseCoords,
     x.fiberAgitator1,
     x.fiberAgitator2,
     x.ifu1Coordinates,
     x.ifu2TargetName,
     x.ifu2Coordinates
    )
  )
}

sealed trait HighResolutionMode extends GhostConfig {
  import HighResolutionMode._

  override def ifu1BundleType: BundleConfig = BundleConfig.HighRes

  def ifu1TargetName: String

  override def ifu1TargetType: IFUTargetType = IFUTargetType.Target(ifu1TargetName)

  override def ifu2TargetType: IFUTargetType =
    this match {
      case _: SingleTarget  => IFUTargetType.NoTarget
      case _: TargetPlusSky => IFUTargetType.SkyPosition
    }

  override def ifu2BundleType: Option[BundleConfig] =
    this match {
      case _: SingleTarget  => None
      case _: TargetPlusSky => Some(BundleConfig.Sky)
    }
}

object HighResolutionMode {
  final case class SingleTarget(
    override val obsType:         String,
    override val blueConfig:      ChannelConfig,
    override val redConfig:       ChannelConfig,
    override val baseCoords:      Option[Coordinates],
    override val fiberAgitator1:  FiberAgitator,
    override val fiberAgitator2:  FiberAgitator,
    override val ifu1TargetName:  String,
    override val ifu1Coordinates: Coordinates,
    override val userTargets:     List[GemTarget]
  ) extends HighResolutionMode {
    override def ifu2Configuration: Configuration =
      GhostConfig.ifuPark(IFUNum.IFU2)
  }

  implicit val hrSingleTargetEq: Eq[SingleTarget] = Eq.by(x =>
    (x.obsType,
     x.blueConfig,
     x.redConfig,
     x.baseCoords,
     x.fiberAgitator1,
     x.fiberAgitator2,
     x.ifu1TargetName,
     x.ifu1Coordinates
    )
  )

  final case class TargetPlusSky(
    override val obsType:         String,
    override val blueConfig:      ChannelConfig,
    override val redConfig:       ChannelConfig,
    override val baseCoords:      Option[Coordinates],
    override val fiberAgitator1:  FiberAgitator,
    override val fiberAgitator2:  FiberAgitator,
    override val ifu1TargetName:  String,
    override val ifu1Coordinates: Coordinates,
    ifu2Coordinates:              Coordinates,
    override val userTargets:     List[GemTarget]
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
     x.blueConfig,
     x.redConfig,
     x.baseCoords,
     x.fiberAgitator1,
     x.fiberAgitator2,
     x.ifu1TargetName,
     x.ifu1Coordinates,
     x.ifu2Coordinates
    )
  )
}
