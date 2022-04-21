// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.ghost

import lucuma.core.math.Coordinates
import lucuma.core.math.arb.ArbCoordinates
import lucuma.core.model.arb.ArbTarget
import lucuma.core.arb.ArbTime
import lucuma.core.util.arb.ArbEnumerated._
import lucuma.core.model.{ Target => GemTarget }
import org.scalacheck.Arbitrary._
import org.scalacheck.Arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen
import scala.concurrent.duration.Duration
import edu.gemini.spModel.gemini.ghost.GhostBinning
import scala.concurrent.duration.FiniteDuration

trait GhostArbitraries extends ArbTime {

  import ArbCoordinates._
  import ArbTarget._

  implicit val channelConfigArb: Arbitrary[ChannelConfig] =
    Arbitrary {
      for {
        bin <- arbitrary[GhostBinning]
        exp <- arbitrary[FiniteDuration]
        cnt <- arbitrary[Int]
      } yield ChannelConfig(bin, exp, cnt)
    }

  val ghostSRSingleTargetConfigGen: Gen[StandardResolutionMode.SingleTarget] =
    for {
      blueConfig      <- arbitrary[ChannelConfig]
      redConfig       <- arbitrary[ChannelConfig]
      basePos         <- arbitrary[Option[Coordinates]]
      expTime         <- arbitrary[Duration]
      fa1             <- arbitrary[FiberAgitator]
      fa2             <- arbitrary[FiberAgitator]
      ifu1TargetName  <- arbitrary[String]
      ifu1Coordinates <- arbitrary[Coordinates]
      targets         <- arbitrary[List[GemTarget]]
    } yield StandardResolutionMode.SingleTarget(blueConfig,
                                                redConfig,
                                                basePos,
                                                expTime,
                                                fa1,
                                                fa2,
                                                ifu1TargetName,
                                                ifu1Coordinates,
                                                targets
    )

  implicit val ghostSRSingleTargetConfigCogen: Cogen[StandardResolutionMode.SingleTarget] =
    Cogen[(Option[Coordinates], Duration, String, Coordinates)]
      .contramap(x => (x.baseCoords, x.expTime, x.ifu1TargetName, x.ifu1Coordinates))

  val ghostSRDualTargetConfigGen: Gen[StandardResolutionMode.DualTarget] =
    for {
      blueConfig      <- arbitrary[ChannelConfig]
      redConfig       <- arbitrary[ChannelConfig]
      basePos         <- arbitrary[Option[Coordinates]]
      expTime         <- arbitrary[Duration]
      fa1             <- arbitrary[FiberAgitator]
      fa2             <- arbitrary[FiberAgitator]
      ifu1TargetName  <- arbitrary[String]
      ifu1Coordinates <- arbitrary[Coordinates]
      ifu2TargetName  <- arbitrary[String]
      ifu2Coordinates <- arbitrary[Coordinates]
      targets         <- arbitrary[List[GemTarget]]
    } yield StandardResolutionMode.DualTarget(blueConfig,
                                              redConfig,
                                              basePos,
                                              expTime,
                                              fa1,
                                              fa2,
                                              ifu1TargetName,
                                              ifu1Coordinates,
                                              ifu2TargetName,
                                              ifu2Coordinates,
                                              targets
    )

  implicit val ghostSRDualTargetConfigCogen: Cogen[StandardResolutionMode.DualTarget] =
    Cogen[(Option[Coordinates], Duration, String, Coordinates, String, Coordinates)]
      .contramap(x =>
        (x.baseCoords,
         x.expTime,
         x.ifu1TargetName,
         x.ifu1Coordinates,
         x.ifu2TargetName,
         x.ifu2Coordinates
        )
      )

  val ghostSRTargetSkyConfigGen: Gen[StandardResolutionMode.TargetPlusSky] =
    for {
      blueConfig      <- arbitrary[ChannelConfig]
      redConfig       <- arbitrary[ChannelConfig]
      basePos         <- arbitrary[Option[Coordinates]]
      exp             <- arbitrary[Duration]
      fa1             <- arbitrary[FiberAgitator]
      fa2             <- arbitrary[FiberAgitator]
      ifu1TargetName  <- arbitrary[String]
      ifu1Coordinates <- arbitrary[Coordinates]
      ifu2Coordinates <- arbitrary[Coordinates]
      targets         <- arbitrary[List[GemTarget]]
    } yield StandardResolutionMode.TargetPlusSky(blueConfig,
                                                 redConfig,
                                                 basePos,
                                                 exp,
                                                 fa1,
                                                 fa2,
                                                 ifu1TargetName,
                                                 ifu1Coordinates,
                                                 ifu2Coordinates,
                                                 targets
    )

  implicit val ghostSRTargetSkyConfigCogen: Cogen[StandardResolutionMode.TargetPlusSky] =
    Cogen[(Option[Coordinates], Duration, String, Coordinates, Coordinates)]
      .contramap(x =>
        (x.baseCoords, x.expTime, x.ifu1TargetName, x.ifu1Coordinates, x.ifu2Coordinates)
      )

  implicit val ghostSRSkyTargetConfigGen: Gen[StandardResolutionMode.SkyPlusTarget] =
    for {
      blueConfig      <- arbitrary[ChannelConfig]
      redConfig       <- arbitrary[ChannelConfig]
      basePos         <- arbitrary[Option[Coordinates]]
      exp             <- arbitrary[Duration]
      fa1             <- arbitrary[FiberAgitator]
      fa2             <- arbitrary[FiberAgitator]
      ifu1Coordinates <- arbitrary[Coordinates]
      ifu2TargetName  <- arbitrary[String]
      ifu2Coordinates <- arbitrary[Coordinates]
      targets         <- arbitrary[List[GemTarget]]
    } yield StandardResolutionMode.SkyPlusTarget(blueConfig,
                                                 redConfig,
                                                 basePos,
                                                 exp,
                                                 fa1,
                                                 fa2,
                                                 ifu1Coordinates,
                                                 ifu2TargetName,
                                                 ifu2Coordinates,
                                                 targets
    )

  implicit val ghostSRSkyTargetConfigCogen: Cogen[StandardResolutionMode.SkyPlusTarget] =
    Cogen[(Option[Coordinates], Duration, Coordinates, String, Coordinates)]
      .contramap(x =>
        (x.baseCoords, x.expTime, x.ifu1Coordinates, x.ifu2TargetName, x.ifu2Coordinates)
      )

  implicit val ghostHRSingleTargetConfigGen: Gen[HighResolutionMode.SingleTarget] =
    for {
      blueConfig <- arbitrary[ChannelConfig]
      redConfig  <- arbitrary[ChannelConfig]
      basePos    <- arbitrary[Option[Coordinates]]
      exp        <- arbitrary[Duration]
      fa1        <- arbitrary[FiberAgitator]
      fa2        <- arbitrary[FiberAgitator]
      hrifu1Name <- arbitrary[String]
      hrifu1Pos  <- arbitrary[Coordinates]
      targets    <- arbitrary[List[GemTarget]]
    } yield HighResolutionMode.SingleTarget(blueConfig,
                                            redConfig,
                                            basePos,
                                            exp,
                                            fa1,
                                            fa2,
                                            hrifu1Name,
                                            hrifu1Pos,
                                            targets
    )

  implicit val ghostHRSingleTargetConfigCogen: Cogen[HighResolutionMode.SingleTarget] =
    Cogen[(Option[Coordinates], Duration, String, Coordinates)]
      .contramap(x => (x.baseCoords, x.expTime, x.ifu1TargetName, x.ifu1Coordinates))

  implicit val ghostHRTargetPlusSkyConfigGen: Gen[HighResolutionMode.TargetPlusSky] =
    for {
      blueConfig <- arbitrary[ChannelConfig]
      redConfig  <- arbitrary[ChannelConfig]
      basePos    <- arbitrary[Option[Coordinates]]
      exp        <- arbitrary[Duration]
      hrifu1Name <- arbitrary[String]
      fa1        <- arbitrary[FiberAgitator]
      fa2        <- arbitrary[FiberAgitator]
      hrifu1Pos  <- arbitrary[Coordinates]
      hrifu2Pos  <- arbitrary[Coordinates]
      targets    <- arbitrary[List[GemTarget]]
    } yield HighResolutionMode.TargetPlusSky(blueConfig,
                                             redConfig,
                                             basePos,
                                             exp,
                                             fa1,
                                             fa2,
                                             hrifu1Name,
                                             hrifu1Pos,
                                             hrifu2Pos,
                                             targets
    )

  implicit val ghostHRTargetSkyConfigCogen: Cogen[HighResolutionMode.TargetPlusSky] =
    Cogen[(Option[Coordinates], Duration, String, Coordinates, Coordinates)]
      .contramap(x =>
        (x.baseCoords, x.expTime, x.ifu1TargetName, x.ifu1Coordinates, x.ifu2Coordinates)
      )

  implicit val ghostConfigArb: Arbitrary[GhostConfig] = Arbitrary {
    Gen.oneOf(
      ghostSRSingleTargetConfigGen,
      ghostSRDualTargetConfigGen,
      ghostSRTargetSkyConfigGen,
      ghostSRSkyTargetConfigGen,
      ghostHRSingleTargetConfigGen,
      ghostHRTargetPlusSkyConfigGen
    )
  }

  object GhostHelpers {
    def extractSRIFU1Name(x: GhostConfig): Option[String] = x match {
      case StandardResolutionMode.SingleTarget(_, _, _, _, _, _, name, _, _)     => Some(name)
      case StandardResolutionMode.DualTarget(_, _, _, _, _, _, name, _, _, _, _) => Some(name)
      case StandardResolutionMode.TargetPlusSky(_, _, _, _, _, _, name, _, _, _) => Some(name)
      case _: StandardResolutionMode.SkyPlusTarget                               => Some("Sky")
      case _                                                                     => None
    }

    def extractSRIFU1Coordinates(x: GhostConfig): Option[Coordinates] = x match {
      case c: StandardResolutionMode => Some(c.ifu1Coordinates)
      case _                         => None
    }

    def extractSRIFU2Name(x: GhostConfig): Option[String] = x match {
      case StandardResolutionMode.DualTarget(_, _, _, _, _, _, _, _, name, _, _) => Some(name)
      case _: StandardResolutionMode.TargetPlusSky                               => Some("Sky")
      case StandardResolutionMode.SkyPlusTarget(_, _, _, _, _, _, _, name, _, _) => Some(name)
      case _                                                                     => None
    }

    def extractSRIFU2Coordinates(x: GhostConfig): Option[Coordinates] = x match {
      case StandardResolutionMode.DualTarget(_, _, _, _, _, _, _, _, _, coords, _) => Some(coords)
      case StandardResolutionMode.TargetPlusSky(_, _, _, _, _, _, _, _, coords, _) => Some(coords)
      case StandardResolutionMode.SkyPlusTarget(_, _, _, _, _, _, _, _, coords, _) => Some(coords)
      case _                                                                       => None
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
      case c: HighResolutionMode.TargetPlusSky => Some(c.ifu2Coordinates)
      case _                                   => None
    }
  }

  implicit val ghostConfigCoGen: Cogen[GhostConfig] = {
    import GhostHelpers._
    Cogen[
      (
        Option[Coordinates],
        Duration,
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
      (x.baseCoords,
       x.expTime,
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
