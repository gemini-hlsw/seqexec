// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.ghost

import cats.Eq
import cats.implicits._
import lucuma.core.util.Enumerated
import giapi.client.GiapiConfig
import edu.gemini.spModel.gemini.ghost.GhostBinning
import lucuma.core.enum.GiapiStatusApply
import lucuma.core.enum.GiapiStatusApply._
import scala.concurrent.duration.FiniteDuration

sealed trait FiberAgitator extends Product with Serializable

object FiberAgitator {
  case object On  extends FiberAgitator
  case object Off extends FiberAgitator

  implicit val FiberAgitatorEnumerated: Enumerated[FiberAgitator] =
    Enumerated.of(On, Off)

  implicit val FiberAgitatorGiapi: GiapiConfig[FiberAgitator] =
    GiapiConfig.instance {
      case On  => "FA_DEMAND_ON"
      case Off => "FA_DEMAND_OFF"
    }

  def fromBoolean(b: Boolean): FiberAgitator =
    if (b) On else Off
}

sealed trait DemandType extends Product with Serializable {
  def demandType: String
}

object DemandType {
  case object DemandRADec extends DemandType {
    val demandType = "IFU_DEMAND_RADEC"
  }
  case object DemandXY    extends DemandType {
    val demandType = "IFU_DEMAND_XY"
  }
  case object DemandPark  extends DemandType {
    val demandType = "IFU_DEMAND_PARK"
  }
  implicit val FiberAgitatorEnumerated: Enumerated[DemandType] =
    Enumerated.of(DemandRADec, DemandXY, DemandPark)

  implicit val DemandTypeCconfiguration: GiapiConfig[DemandType] = _.demandType
}

sealed abstract class IFUNum(val ifuNum: Int) extends Product with Serializable {
  val ifuStr: String = s"ghost:cc:cu:ifu$ifuNum"

  def demandItem: GiapiStatusApply = this match {
    case IFUNum.IFU1 => GhostIFU1Type
    case IFUNum.IFU2 => GhostIFU2Type
  }

  def bundleItem: GiapiStatusApply = this match {
    case IFUNum.IFU1 => GhostIFU1Bundle
    case IFUNum.IFU2 => GhostIFU2Bundle
  }

  def targetItem: GiapiStatusApply = this match {
    case IFUNum.IFU1 => GhostIFU1Target
    case IFUNum.IFU2 => GhostIFU2Target
  }

}

object IFUNum {
  case object IFU1 extends IFUNum(ifuNum = 1)
  case object IFU2 extends IFUNum(ifuNum = 2)
  implicit val ifuNumConfiguration: GiapiConfig[IFUNum] = _.ifuStr
}

sealed abstract class BundleConfig(val configName: String) extends Product with Serializable {
  def determineType(t: IFUTargetType): BundleConfig =
    t match {
      case IFUTargetType.SkyPosition => BundleConfig.Sky
      case _                         => this
    }
}

object BundleConfig {
  case object Standard extends BundleConfig(configName = "IFU_STDRES")
  case object HighRes  extends BundleConfig(configName = "IFU_HIRES")
  case object Sky      extends BundleConfig(configName = "IFU_SKY")
  implicit val bundleConfiguration: GiapiConfig[BundleConfig] = _.configName
}

sealed abstract class IFUTargetType(val targetType: String) extends Product with Serializable
object IFUTargetType {

  case object NoTarget                  extends IFUTargetType(targetType = "IFU_TARGET_NONE")
  case object SkyPosition               extends IFUTargetType(targetType = "IFU_TARGET_SKY")
  final case class Target(name: String) extends IFUTargetType(targetType = "IFU_TARGET_OBJECT")

  def determineType(name: Option[String]): IFUTargetType =
    name match {
      case None        => NoTarget
      case Some("Sky") => SkyPosition
      case Some(x)     => Target(x)
    }

  implicit val ifuTargetTypeConfiguration: GiapiConfig[IFUTargetType] = _.targetType
}

sealed abstract class ReadNoiseGain(val value: String) extends Product with Serializable
object ReadNoiseGain {

  case object SlowLow  extends ReadNoiseGain(value = "CHIP_READOUT_SLOW")
  case object FastLow  extends ReadNoiseGain(value = "CHIP_READOUT_MEDIUM")
  case object FastHigh extends ReadNoiseGain(value = "CHIP_READOUT_FAST")

  // implicit val ifuTargetTypeConfiguration: GiapiConfig[ReadNoiseGain] = _.targetType
}

final case class ChannelConfig(
  binning:  GhostBinning,
  exposure: FiniteDuration,
  count:    Int,
  readMode: ReadNoiseGain
)

object ChannelConfig {

  implicit val eqGhostBinning: Eq[GhostBinning]   = Eq.fromUniversalEquals
  implicit val eqReadNoiseGain: Eq[ReadNoiseGain] = Eq.fromUniversalEquals

  implicit val eqChannelConfig: Eq[ChannelConfig] =
    Eq.by(x => (x.binning, x.exposure, x.count, x.readMode))
}

// final case class TargetConfig(name: String, coords: Coordinates, guideFiber: Option[GuideFiberState])
