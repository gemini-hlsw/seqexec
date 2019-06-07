// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.ghost

import gsp.math.Coordinates
import gsp.math.arb.{ ArbCoordinates, ArbTime }
import org.scalacheck.Arbitrary._
import org.scalacheck.Arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen
import scala.concurrent.duration.Duration
import scala.concurrent.duration._

trait GhostArbitraries extends ArbTime {

  import ArbCoordinates._

  val ghostSRSingleTargetConfigGen: Gen[StandardResolutionMode.SingleTarget] =
    for {
      basePos    <- arbitrary[Option[Coordinates]]
      exp        <- arbitrary[Duration]
      srifu1Name <- arbitrary[String]
      srifu1Pos  <- arbitrary[Coordinates]
    } yield
      StandardResolutionMode.SingleTarget(basePos, exp, srifu1Name, srifu1Pos)

  implicit val ghostSRSingleTargetConfigCogen
    : Cogen[StandardResolutionMode.SingleTarget] =
    Cogen[(Option[Coordinates], Duration, String, Coordinates)]
      .contramap(x =>
        (x.baseCoords, x.expTime, x.ifu1TargetName, x.ifu1Coordinates))

  val ghostSRDualTargetConfigGen: Gen[StandardResolutionMode.DualTarget] =
    for {
      basePos    <- arbitrary[Option[Coordinates]]
      exp        <- arbitrary[Duration]
      srifu1Name <- arbitrary[String]
      srifu1Pos  <- arbitrary[Coordinates]
      srifu2Name <- arbitrary[String]
      srifu2Pos  <- arbitrary[Coordinates]
    } yield
      StandardResolutionMode.DualTarget(basePos,
                                        exp,
                                        srifu1Name,
                                        srifu1Pos,
                                        srifu2Name,
                                        srifu2Pos)

  implicit val ghostSRDualTargetConfigCogen
    : Cogen[StandardResolutionMode.DualTarget] =
    Cogen[(Option[Coordinates], Duration, String, Coordinates, String, Coordinates)]
      .contramap(
        x =>
          (x.baseCoords,
           x.expTime,
           x.ifu1TargetName,
           x.ifu1Coordinates,
           x.ifu2TargetName,
           x.ifu2Coordinates))

  val ghostSRTargetSkyConfigGen: Gen[StandardResolutionMode.TargetPlusSky] =
    for {
      basePos    <- arbitrary[Option[Coordinates]]
      exp        <- arbitrary[Duration]
      srifu1Name <- arbitrary[String]
      srifu1Pos  <- arbitrary[Coordinates]
      srifu2Pos  <- arbitrary[Coordinates]
    } yield
      StandardResolutionMode.TargetPlusSky(basePos,
                                           exp,
                                           srifu1Name,
                                           srifu1Pos,
                                           srifu2Pos)

  implicit val ghostSRTargetSkyConfigCogen
    : Cogen[StandardResolutionMode.TargetPlusSky] =
    Cogen[(Option[Coordinates], Duration, String, Coordinates, Coordinates)]
      .contramap(
        x =>
          (x.baseCoords,
           x.expTime,
           x.ifu1TargetName,
           x.ifu1Coordinates,
           x.ifu2Coordinates))

  implicit val ghostSRSkyTargetConfigGen
    : Gen[StandardResolutionMode.SkyPlusTarget] =
    for {
      basePos    <- arbitrary[Option[Coordinates]]
      exp        <- arbitrary[Duration]
      srifu1Pos  <- arbitrary[Coordinates]
      srifu2Name <- arbitrary[String]
      srifu2Pos  <- arbitrary[Coordinates]
    } yield
      StandardResolutionMode.SkyPlusTarget(basePos,
                                           exp,
                                           srifu1Pos,
                                           srifu2Name,
                                           srifu2Pos)

  implicit val ghostSRSkyTargetConfigCogen
    : Cogen[StandardResolutionMode.SkyPlusTarget] =
    Cogen[(Option[Coordinates], Duration, Coordinates, String, Coordinates)]
      .contramap(
        x =>
          (x.baseCoords,
           x.expTime,
           x.ifu1Coordinates,
           x.ifu2TargetName,
           x.ifu2Coordinates))

  implicit val ghostHRSingleTargetConfigGen
    : Gen[HighResolutionMode.SingleTarget] =
    for {
      basePos    <- arbitrary[Option[Coordinates]]
      exp        <- arbitrary[Duration]
      hrifu1Name <- arbitrary[String]
      hrifu1Pos  <- arbitrary[Coordinates]
    } yield HighResolutionMode.SingleTarget(basePos, exp, hrifu1Name, hrifu1Pos)

  implicit val ghostHRSingleTargetConfigCogen
    : Cogen[HighResolutionMode.SingleTarget] =
    Cogen[(Option[Coordinates], Duration, String, Coordinates)]
      .contramap(x =>
        (x.baseCoords, x.expTime, x.ifu1TargetName, x.ifu1Coordinates))

  implicit val ghostHRTargetPlusSkyConfigGen
    : Gen[HighResolutionMode.TargetPlusSky] =
    for {
      basePos    <- arbitrary[Option[Coordinates]]
      exp        <- arbitrary[Duration]
      hrifu1Name <- arbitrary[String]
      hrifu1Pos  <- arbitrary[Coordinates]
      hrifu2Pos  <- arbitrary[Coordinates]
    } yield
      HighResolutionMode.TargetPlusSky(basePos,
                                       exp,
                                       hrifu1Name,
                                       hrifu1Pos,
                                       hrifu2Pos)

  implicit val ghostHRTargetSkyConfigCogen
    : Cogen[HighResolutionMode.TargetPlusSky] =
    Cogen[(Option[Coordinates], Duration, String, Coordinates, Coordinates)]
      .contramap(
        x =>
          (x.baseCoords,
           x.expTime,
           x.ifu1TargetName,
           x.ifu1Coordinates,
           x.ifu2Coordinates))

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
      case StandardResolutionMode.SingleTarget(_, _, name, _)     => Some(name)
      case StandardResolutionMode.DualTarget(_, _, name, _, _, _) => Some(name)
      case StandardResolutionMode.TargetPlusSky(_, _, name, _, _) => Some(name)
      case _: StandardResolutionMode.SkyPlusTarget                => Some("Sky")
      case _                                                                      => None
    }

    def extractSRIFU1Coordinates(x: GhostConfig): Option[Coordinates] = x match {
      case c: StandardResolutionMode => Some(c.ifu1Coordinates)
      case _                                         => None
    }

    def extractSRIFU2Name(x: GhostConfig): Option[String] = x match {
      case StandardResolutionMode.DualTarget(_, _, _, _, name, _) => Some(name)
      case _: StandardResolutionMode.TargetPlusSky                => Some("Sky")
      case StandardResolutionMode.SkyPlusTarget(_, _, _, name, _) => Some(name)
      case _                                                                      => None
    }

    def extractSRIFU2Coordinates(x: GhostConfig): Option[Coordinates] = x match {
      case StandardResolutionMode.DualTarget(_, _, _, _, _, coords) => Some(coords)
      case StandardResolutionMode.TargetPlusSky(_, _, _, _, coords) => Some(coords)
      case StandardResolutionMode.SkyPlusTarget(_, _, _, _, coords) => Some(coords)
      case _                                                                        => None
    }

    def extractHRIFU1Name(x: GhostConfig): Option[String] = x match {
      case c: HighResolutionMode => Some(c.ifu1TargetName)
      case _                                     => None
    }

    def extractHRIFU1Coordinates(x: GhostConfig): Option[Coordinates] = x match {
      case c: HighResolutionMode => Some(c.ifu1Coordinates)
      case _                                     => None
    }

    def extractHRIFU2Name(x: GhostConfig): Option[String] = x match {
      case _: HighResolutionMode.TargetPlusSky => Some("Sky")
      case _                                                   => None
    }

    def extractHRIFU2Coordinates(x: GhostConfig): Option[Coordinates] = x match {
      case c: HighResolutionMode.TargetPlusSky => Some(c.ifu2Coordinates)
      case _                                                   => None
    }
  }

  implicit val ghostConfigCoGen: Cogen[GhostConfig] = {
    import GhostHelpers._
    Cogen[(Option[Coordinates],
      Duration,
      Option[String],
      Option[Coordinates],
      Option[String],
      Option[Coordinates],
      Option[String],
      Option[Coordinates],
      Option[String],
      Option[Coordinates])]
      .contramap(
        x => (x.baseCoords,
          x.expTime,
          extractSRIFU1Name(x),
          extractSRIFU1Coordinates(x),
          extractSRIFU2Name(x),
          extractSRIFU2Coordinates(x),
          extractHRIFU1Name(x),
          extractHRIFU1Coordinates(x),
          extractHRIFU2Name(x),
          extractHRIFU2Coordinates(x)
        ))}

}
