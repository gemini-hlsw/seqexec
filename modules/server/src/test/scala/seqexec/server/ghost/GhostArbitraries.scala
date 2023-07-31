// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.ghost

import lucuma.core.math.Coordinates
import lucuma.core.math.arb.ArbCoordinates
import lucuma.core.model.arb.ArbTarget
import lucuma.core.arb.ArbTime
import lucuma.core.util.arb.ArbEnumerated._
import lucuma.core.model.{ Target => GemTarget }
import org.scalacheck.Arbitrary._
import edu.gemini.spModel.target.env.ResolutionMode
import org.scalacheck.Arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen
import edu.gemini.spModel.gemini.ghost.GhostBinning
import scala.concurrent.duration.FiniteDuration
import seqexec.model.Conditions
import seqexec.model.SeqexecModelArbitraries._
import shapeless.tag

trait GhostArbitraries extends ArbTime {

  import ArbCoordinates._
  import ArbTarget._

  implicit val readNoiseArb: Arbitrary[ReadNoiseGain] =
    Arbitrary(Gen.oneOf(ReadNoiseGain.Slow, ReadNoiseGain.Medium, ReadNoiseGain.Fast))

  implicit val resoultionModeArb: Arbitrary[ResolutionMode] =
    Arbitrary(
      Gen.oneOf(ResolutionMode.Standard,
                ResolutionMode.GhostStandard,
                ResolutionMode.GhostHigh,
                ResolutionMode.GhostPRV
      )
    )

  implicit val readNoiseCogen: Cogen[ReadNoiseGain] = Cogen[String].contramap(_.value)

  implicit val rmCogen: Cogen[ResolutionMode] = Cogen[String].contramap(_.tag)

  implicit val channelConfigArb: Arbitrary[ChannelConfig] =
    Arbitrary {
      for {
        bin      <- arbitrary[GhostBinning]
        exp      <- arbitrary[FiniteDuration]
        cnt      <- arbitrary[Int]
        readMode <- arbitrary[ReadNoiseGain]
      } yield ChannelConfig(bin, exp, cnt, readMode)
    }

  val ghostSRSingleTargetConfigGen: Gen[StandardResolutionMode.SingleTarget] =
    for {
      obsType         <- arbitrary[String]
      obsClass        <- arbitrary[String]
      blueConfig      <- arbitrary[ChannelConfig]
      redConfig       <- arbitrary[ChannelConfig]
      basePos         <- arbitrary[Option[Coordinates]]
      fa1             <- arbitrary[FiberAgitator]
      fa2             <- arbitrary[FiberAgitator]
      ifu1TargetName  <- arbitrary[String]
      ifu1Coordinates <- arbitrary[Coordinates]
      targets         <- arbitrary[List[GemTarget]]
      rm              <- arbitrary[Option[ResolutionMode]]
      conditions      <- arbitrary[Conditions]
      mag             <- arbitrary[Option[Double]]
    } yield StandardResolutionMode.SingleTarget(
      obsType,
      obsClass,
      tag[BlueChannel][ChannelConfig](blueConfig),
      tag[RedChannel][ChannelConfig](redConfig),
      basePos,
      fa1,
      fa2,
      ifu1TargetName,
      ifu1Coordinates,
      targets,
      rm,
      conditions,
      mag
    )

  implicit val ghostSRSingleTargetConfigCogen: Cogen[StandardResolutionMode.SingleTarget] =
    Cogen[(Option[Coordinates], String, Coordinates)]
      .contramap(x => (x.baseCoords, x.ifu1TargetName, x.ifu1Coordinates))

  val ghostSRDualTargetConfigGen: Gen[StandardResolutionMode.DualTarget] =
    for {
      obsType         <- arbitrary[String]
      obsClass        <- arbitrary[String]
      blueConfig      <- arbitrary[ChannelConfig]
      redConfig       <- arbitrary[ChannelConfig]
      basePos         <- arbitrary[Option[Coordinates]]
      fa1             <- arbitrary[FiberAgitator]
      fa2             <- arbitrary[FiberAgitator]
      ifu1TargetName  <- arbitrary[String]
      ifu1Coordinates <- arbitrary[Coordinates]
      ifu2TargetName  <- arbitrary[String]
      ifu2Coordinates <- arbitrary[Coordinates]
      targets         <- arbitrary[List[GemTarget]]
      rm              <- arbitrary[Option[ResolutionMode]]
      conditions      <- arbitrary[Conditions]
      mag             <- arbitrary[Option[Double]]
    } yield StandardResolutionMode.DualTarget(
      obsType,
      obsClass,
      tag[BlueChannel][ChannelConfig](blueConfig),
      tag[RedChannel][ChannelConfig](redConfig),
      basePos,
      fa1,
      fa2,
      ifu1TargetName,
      ifu1Coordinates,
      ifu2TargetName,
      ifu2Coordinates,
      targets,
      rm,
      conditions,
      mag
    )

  implicit val ghostSRDualTargetConfigCogen: Cogen[StandardResolutionMode.DualTarget] =
    Cogen[(Option[Coordinates], String, Coordinates, String, Option[Coordinates])]
      .contramap(x =>
        (x.baseCoords, x.ifu1TargetName, x.ifu1Coordinates, x.ifu2TargetName, x.ifu2Coordinates)
      )

  val ghostSRTargetSkyConfigGen: Gen[StandardResolutionMode.TargetPlusSky] =
    for {
      obsType         <- arbitrary[String]
      obsClass        <- arbitrary[String]
      blueConfig      <- arbitrary[ChannelConfig]
      redConfig       <- arbitrary[ChannelConfig]
      basePos         <- arbitrary[Option[Coordinates]]
      fa1             <- arbitrary[FiberAgitator]
      fa2             <- arbitrary[FiberAgitator]
      ifu1TargetName  <- arbitrary[String]
      ifu1Coordinates <- arbitrary[Coordinates]
      ifu2Coordinates <- arbitrary[Coordinates]
      targets         <- arbitrary[List[GemTarget]]
      rm              <- arbitrary[Option[ResolutionMode]]
      conditions      <- arbitrary[Conditions]
      mag             <- arbitrary[Option[Double]]
    } yield StandardResolutionMode.TargetPlusSky(
      obsType,
      obsClass,
      tag[BlueChannel][ChannelConfig](blueConfig),
      tag[RedChannel][ChannelConfig](redConfig),
      basePos,
      fa1,
      fa2,
      ifu1TargetName,
      ifu1Coordinates,
      ifu2Coordinates,
      targets,
      rm,
      conditions,
      mag
    )

  implicit val ghostSRTargetSkyConfigCogen: Cogen[StandardResolutionMode.TargetPlusSky] =
    Cogen[(Option[Coordinates], String, Coordinates, Option[Coordinates])]
      .contramap(x => (x.baseCoords, x.ifu1TargetName, x.ifu1Coordinates, x.ifu2Coordinates))

  implicit val ghostSRSkyTargetConfigGen: Gen[StandardResolutionMode.SkyPlusTarget] =
    for {
      obsType         <- arbitrary[String]
      obsClass        <- arbitrary[String]
      blueConfig      <- arbitrary[ChannelConfig]
      redConfig       <- arbitrary[ChannelConfig]
      basePos         <- arbitrary[Option[Coordinates]]
      fa1             <- arbitrary[FiberAgitator]
      fa2             <- arbitrary[FiberAgitator]
      ifu1Coordinates <- arbitrary[Coordinates]
      ifu2TargetName  <- arbitrary[String]
      ifu2Coordinates <- arbitrary[Coordinates]
      targets         <- arbitrary[List[GemTarget]]
      rm              <- arbitrary[Option[ResolutionMode]]
      conditions      <- arbitrary[Conditions]
      mag             <- arbitrary[Option[Double]]
    } yield StandardResolutionMode.SkyPlusTarget(
      obsType,
      obsClass,
      tag[BlueChannel][ChannelConfig](blueConfig),
      tag[RedChannel][ChannelConfig](redConfig),
      basePos,
      fa1,
      fa2,
      ifu1Coordinates,
      ifu2TargetName,
      ifu2Coordinates,
      targets,
      rm,
      conditions,
      mag
    )

  implicit val ghostSRSkyTargetConfigCogen: Cogen[StandardResolutionMode.SkyPlusTarget] =
    Cogen[(Option[Coordinates], Coordinates, String, Option[Coordinates])]
      .contramap(x => (x.baseCoords, x.ifu1Coordinates, x.ifu2TargetName, x.ifu2Coordinates))

  implicit val ghostHRTargetPlusSkyConfigGen: Gen[HighResolutionMode.TargetPlusSky] =
    for {
      obsType    <- arbitrary[String]
      obsClass   <- arbitrary[String]
      blueConfig <- arbitrary[ChannelConfig]
      redConfig  <- arbitrary[ChannelConfig]
      basePos    <- arbitrary[Option[Coordinates]]
      hrifu1Name <- arbitrary[String]
      fa1        <- arbitrary[FiberAgitator]
      fa2        <- arbitrary[FiberAgitator]
      hrifu1Pos  <- arbitrary[Coordinates]
      hrifu2Pos  <- arbitrary[Coordinates]
      targets    <- arbitrary[List[GemTarget]]
      rm         <- arbitrary[Option[ResolutionMode]]
      conditions <- arbitrary[Conditions]
      mag        <- arbitrary[Option[Double]]
    } yield HighResolutionMode.TargetPlusSky(
      obsType,
      obsClass,
      tag[BlueChannel][ChannelConfig](blueConfig),
      tag[RedChannel][ChannelConfig](redConfig),
      basePos,
      fa1,
      fa2,
      hrifu1Name,
      hrifu1Pos,
      hrifu2Pos,
      targets,
      rm,
      conditions,
      mag
    )

  implicit val ghostHRTargetSkyConfigCogen: Cogen[HighResolutionMode.TargetPlusSky] =
    Cogen[(Option[Coordinates], String, Coordinates, Option[Coordinates])]
      .contramap(x => (x.baseCoords, x.ifu1TargetName, x.ifu1Coordinates, x.ifu2Coordinates))

  implicit val ghostConfigArb: Arbitrary[GhostConfig] = Arbitrary {
    Gen.oneOf(
      ghostSRSingleTargetConfigGen,
      ghostSRDualTargetConfigGen,
      ghostSRTargetSkyConfigGen,
      ghostSRSkyTargetConfigGen,
      ghostHRTargetPlusSkyConfigGen
    )
  }

  object GhostHelpers {
    def extractSRIFU1Name(x: GhostConfig): Option[String] = x match {
      case StandardResolutionMode.SingleTarget(_, _, _, _, _, _, _, name, _, _, _, _, _)     =>
        Some(name)
      case StandardResolutionMode.DualTarget(_, _, _, _, _, _, _, name, _, _, _, _, _, _, _) =>
        Some(name)
      case StandardResolutionMode.TargetPlusSky(_, _, _, _, _, _, _, name, _, _, _, _, _, _) =>
        Some(name)
      case _: StandardResolutionMode.SkyPlusTarget                                           => Some("Sky")
      case _                                                                                 => None
    }

    def extractSRIFU1Coordinates(x: GhostConfig): Option[Coordinates] = x match {
      case c: StandardResolutionMode => Some(c.ifu1Coordinates)
      case _                         => None
    }

    def extractSRIFU2Name(x: GhostConfig): Option[String] = x match {
      case StandardResolutionMode.DualTarget(_, _, _, _, _, _, _, _, _, name, _, _, _, _, _) =>
        Some(name)
      case _: StandardResolutionMode.TargetPlusSky                                           => Some("Sky")
      case StandardResolutionMode.SkyPlusTarget(_, _, _, _, _, _, _, _, name, _, _, _, _, _) =>
        Some(name)
      case _                                                                                 => None
    }

    def extractSRIFU2Coordinates(x: GhostConfig): Option[Coordinates] = x match {
      case StandardResolutionMode.DualTarget(_, _, _, _, _, _, _, _, _, _, coords, _, _, _, _) =>
        Some(coords)
      case StandardResolutionMode.TargetPlusSky(_, _, _, _, _, _, _, _, _, coords, _, _, _, _) =>
        Some(coords)
      case StandardResolutionMode.SkyPlusTarget(_, _, _, _, _, _, _, _, _, coords, _, _, _, _) =>
        Some(coords)
      case _                                                                                   => None
    }

    def extractHRIFU1Name(x: GhostConfig): Option[String] = x match {
      case c: HighResolutionMode => Some(c.ifu1TargetName)
      case _                     => None
    }

    def extractHRIFU1Coordinates(x: GhostConfig): Option[Coordinates] = x match {
      case c: HighResolutionMode => Some(c.ifu1Coordinates)
      case _                     => None
    }

    def extractHRIFU2Name(x: GhostConfig): Option[String] = x match {
      case _: HighResolutionMode.TargetPlusSky => Some("Sky")
      case _                                   => None
    }

    def extractHRIFU2Coordinates(x: GhostConfig): Option[Coordinates] = x match {
      case c: HighResolutionMode.TargetPlusSky => c.ifu2Coordinates
      case _                                   => None
    }
  }

  implicit val ghostConfigCoGen: Cogen[GhostConfig] = {
    import GhostHelpers._
    Cogen[
      (
        String,
        String,
        // ChannelConfig,
        // ChannelConfig,
        Option[Coordinates],
        // List[GemTarget],
        Option[String],
        Option[Coordinates],
        Option[String],
        Option[Coordinates],
        Option[String],
        Option[Coordinates],
        Option[String],
        Option[Coordinates]
      )
    ].contramap(x =>
      (x.obsType,
       x.obsClass,
       // x.blueConfig,
       // x.redConfig,
       x.baseCoords,
       // x.userTargets,
       extractSRIFU1Name(x),
       extractSRIFU1Coordinates(x),
       extractSRIFU2Name(x),
       extractSRIFU2Coordinates(x),
       extractHRIFU1Name(x),
       extractHRIFU1Coordinates(x),
       extractHRIFU2Name(x),
       extractHRIFU2Coordinates(x)
      )
    )
  }

}
