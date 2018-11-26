// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.ghost

import cats.data.EitherT
import cats.implicits._
import cats.{Eq, Show}
import cats.effect.Sync
import gem.math.{Angle, HourAngle}
import giapi.client.commands.{CommandResult, CommandResultException, Configuration}
import giapi.client.ghost.GHOSTClient
import seqexec.model.dhs.ImageFileId
import seqexec.server.SeqActionF
import seqexec.server.keywords.GDSClient

import scala.concurrent.duration._
import org.log4s._
import seqexec.server.SeqexecFailure.{Execution, SeqexecException}
import squants.time.Time

final case class GHOSTController[F[_]: Sync](ghostClient: GHOSTClient[F],
                                             gdsClient: GDSClient) {
  import GHOSTController._
  val Log: Logger = getLogger

  // TODO: Are GIAPI parameters maintained between applies? Here is a use case that requires more information:
  // 1. We observe in standard resolution mode using both IFUs on targets.
  // 2. We observe in high resolution mode using only IFU1 on a target.
  // Question: Do we need to, for (2), touch IFU2 at all (i.e. to standard mode, no target)?
  // Do we need to always set target type for IFUs? How about bundle configuration?
  //
  // Right now, this only generate a config if the name, RA, and dec are defined: otherwise, we get the empty config.
  private def ifuConfig(ifuNum: IFUNum,
                        nameOpt: Option[String],
                        raOpt: Option[HourAngle],
                        decOpt: Option[Angle],
                        bundleConfig: BundleConfig): Configuration = {
    def cfg[P: Show](paramName: String, paramVal: P) =
      Configuration.single(s"${ifuNum.ifuStr}.$paramName", paramVal)

    (nameOpt, raOpt, decOpt) match {
      // Case 1: IFU is in use here for an actual position.
      case (Some(name@_), Some(ra), Some(dec)) =>
        val ifuTargetType = IFUTargetType.determineType(nameOpt)
        cfg("target", ifuTargetType.targetType) |+|
          cfg("type", DemandType.DemandRADec.demandType) |+|
          cfg("ra", ra.toDoubleDegrees) |+|
          cfg("dec", dec.toDoubleDegrees) |+|
          cfg("bundle", bundleConfig.determineType(ifuTargetType).configName)

      // Case 2: IFU is explicitly excluded from use.
      case (Some(name@_), None, None) =>
        cfg("target", IFUTargetType.NoTarget.targetType) |+|
          cfg("type", DemandType.DemandPark.demandType)

      // Case 3: IFU is not in this mode. Look to other mode (e.g. high res).
      case _ =>
        Configuration.Zero
    }
  }

  private def ifu1Config(config: GHOSTConfig): Configuration = {
    val srConfig = ifuConfig(IFUNum.IFU1, config.srifu1Name, config.srifu1CoordsRAHMS,
      config.srifu1CoordsDecDMS, BundleConfig.Standard)
    val hrConfig = ifuConfig(IFUNum.IFU2, config.hrifu1Name, config.hrifu1CoordsRAHMS,
      config.hrifu1CoordsDecDMS, BundleConfig.HighRes)
    srConfig |+| hrConfig
  }

  private def ifu2Config(config: GHOSTConfig): Configuration = {
    val srConfig = ifuConfig(IFUNum.IFU2, config.srifu2Name, config.srifu2CoordsRAHMS,
      config.srifu2CoordsDecDMS, BundleConfig.Standard)
    val hrConfig = ifuConfig(IFUNum.IFU2, config.hrifu2Name, config.srifu2CoordsRAHMS, config.srifu2CoordsDecDMS,
      BundleConfig.HighRes)
    srConfig |+| hrConfig
  }


  // If the srifu parameters are defined, use them; otherwise, use the hrifu parameters.
  // Which set of parameters to use is determined completely by which of srifuName and hrifuName is set.
  // If neither is set, we do not use the IFU and request to be parked.
  private def ghostConfig(config: GHOSTConfig): SeqActionF[F, CommandResult] = {
    val giapiApplyUF1Config = ifu1Config(config)
    val giapiApplyUF1Modified = if (giapiApplyUF1Config === Configuration.Zero) {
      Configuration.single(s"${IFUNum.IFU1.ifuStr}.target", IFUTargetType.NoTarget.targetType) |+|
        Configuration.single(s"${IFUNum.IFU1.ifuStr}.type", DemandType.DemandPark.demandType)
    } else giapiApplyUF1Config

    val giapiApplyUF2Config = ifu2Config(config)
    val giapiApplyUF2Modified = if (giapiApplyUF2Config === Configuration.Zero) {
      Configuration.single(s"${IFUNum.IFU2.ifuStr}.target", IFUTargetType.NoTarget.targetType) |+|
        Configuration.single(s"${IFUNum.IFU2.ifuStr}.type", DemandType.DemandPark.demandType)
    } else giapiApplyUF2Config

    val giapiApply = giapiApplyUF1Modified |+| giapiApplyUF2Modified

    EitherT(ghostClient.genericApply(giapiApply).attempt)
      .leftMap {
        // The GMP sends these cryptic messages but we can do better
        case CommandResultException(_, "Message cannot be null") => Execution("Unhandled Apply command")
        case CommandResultException(_, m)                        => Execution(m)
        case f                                                   => SeqexecException(f)
      }
  }

  def applyConfig(config: GHOSTConfig): SeqActionF[F, Unit] =
    for {
      _ <- SeqActionF.apply(Log.debug("Start GHOST configuration"))
      _ <- SeqActionF.apply(Log.debug(s"GHOST configuration $config"))
      _ <- ghostConfig(config)
      _ <- SeqActionF.apply(Log.debug("Completed GHOST configuration"))
    } yield ()

  // We use a dummy observation for now, since at this point, we cannot actually observe using the instrument.
  def observe(fileId: ImageFileId, expTime: Time): SeqActionF[F, ImageFileId] =
    SeqActionF.apply((fileId, expTime)._1) // suppress unused error
  //    EitherT(ghostClient.observe(fileId, expTime.toMilliseconds.milliseconds).map(_ => fileId).attempt)
  //      .leftMap {
  //        case CommandResultException(_, "Message cannot be null") => Execution("Unhandled observe command")
  //        case CommandResultException(_, m)                        => Execution(m)
  //        case f                                                   => SeqexecException(f)
  //      }

  def endObserve: SeqActionF[F, Unit] = SeqActionF.void
}

object GHOSTController {
  sealed abstract class BundleConfig(val configName: String) {
    def determineType(t: IFUTargetType): BundleConfig = t match {
      case IFUTargetType.SkyPosition => BundleConfig.Sky
      case _                         => this
    }
  }
  object BundleConfig {
    case object Standard extends BundleConfig(configName = "IFU_LORES")
    case object HighRes  extends BundleConfig(configName = "IFU_HRES")
    case object Sky      extends BundleConfig(configName = "IFU_SKY")
  }

  sealed abstract class IFUNum(val ifuNum: Int) {
    val ifuStr: String = s"ghost:cc:cu:ifu$ifuNum"
  }
  object IFUNum {
    case object IFU1 extends IFUNum(ifuNum = 1)
    case object IFU2 extends IFUNum(ifuNum = 2)
  }

  sealed abstract class IFUTargetType(val targetType: String)
  object IFUTargetType {
    case object NoTarget extends IFUTargetType(targetType = "IFU_TARGET_NONE")
    case object SkyPosition extends IFUTargetType(targetType = "IFU_TARGET_SKY")
    case object Target extends IFUTargetType(targetType = "IFU_TARGET_OBJECT")

    def determineType(name: Option[String]): IFUTargetType = name match {
      case None        => NoTarget
      case Some("Sky") => SkyPosition
      case _           => Target
    }
  }

  sealed abstract class DemandType(val demandType: String)
  object DemandType {
    // Future DemandTypes: HALT, HOME, XY
    case object DemandRADec extends DemandType("IFU_DEMAND_RADEC")
    case object DemandPark extends DemandType("IFU_DEMAND_PARK")
  }

  /**
    * TODO: This is a hack for the 2018 testing. We simply accept all parameters from the ODB as Options, and
    * then, from there, put together an apply. As we proceed, this will be scrapped and we will focus on type
    * safety, since amongst these parameters, some will only be defined when others are, and some defined parameters
    * will force others to be None. Eventually, we will want to represent all the target information in a type-safe
    * way that GHOST uses in the ODB.
    */
  final case class GHOSTConfig(baseRAHMS: Option[HourAngle],
                               baseDecDMS: Option[Angle],
                               expTime: Duration,
                               srifu1Name: Option[String],
                               srifu1CoordsRAHMS: Option[HourAngle],
                               srifu1CoordsDecDMS: Option[Angle],
                               srifu2Name: Option[String],
                               srifu2CoordsRAHMS: Option[HourAngle],
                               srifu2CoordsDecDMS: Option[Angle],
                               hrifu1Name: Option[String],
                               hrifu1CoordsRAHMS: Option[HourAngle],
                               hrifu1CoordsDecDMS: Option[Angle],
                               hrifu2Name: Option[String],
                               hrifu2CoordsRAHMS: Option[HourAngle],
                               hrifu2CoordsDecDMS: Option[Angle])

  object GHOSTConfig {
    private implicit val durationEq: Eq[Duration] = Eq.by(_.toMillis)
    implicit val eq: Eq[GHOSTConfig] = Eq.by(
      x =>
        (x.baseRAHMS,
          x.baseDecDMS,
          x.expTime,
          x.srifu1Name,
          x.srifu1CoordsRAHMS,
          x.srifu1CoordsDecDMS,
          x.srifu2Name,
          x.srifu2CoordsRAHMS,
          x.srifu2CoordsDecDMS,
          x.hrifu1Name,
          x.hrifu1CoordsRAHMS,
          x.hrifu1CoordsDecDMS,
          x.hrifu2Name,
          x.hrifu2CoordsRAHMS,
          x.hrifu2CoordsDecDMS))

    implicit val show: Show[GHOSTConfig] = Show.fromToString
  }
}
