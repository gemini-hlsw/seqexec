// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package arb

import gem.config.StaticConfig
import gem.config.GmosConfig._
import gem.enum._
import gem.math.Offset

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Arbitrary, Cogen, Gen}


trait ArbGmos {

  import ArbOffset._
  import ArbEnumerated._

  // Static Config

  implicit val arbGmosShuffleOffset: Arbitrary[GmosShuffleOffset] =
    Arbitrary(Gen.posNum[Int].map(GmosShuffleOffset.unsafeFromRowCount))

  implicit val cogGmosShuffleOffset: Cogen[GmosShuffleOffset] =
    Cogen[Int].contramap(_.detectorRows)

  implicit val arbGmosShuffleCycles: Arbitrary[GmosShuffleCycles] =
    Arbitrary(Gen.posNum[Int].map(GmosShuffleCycles.unsafeFromCycleCount))

  implicit val cogGmosShuffleCycles: Cogen[GmosShuffleCycles] =
    Cogen[Int].contramap(_.toInt)

  implicit val arbGmosNodAndShuffle: Arbitrary[GmosNodAndShuffle] =
    Arbitrary(
      for {
        a <- arbitrary[Offset]
        b <- arbitrary[Offset]
        e <- arbitrary[GmosEOffsetting]
        o <- arbitrary[GmosShuffleOffset]
        c <- arbitrary[GmosShuffleCycles]
      } yield GmosNodAndShuffle(a, b, e, o, c)
    )

  implicit val cogGmosNodAndShuffle: Cogen[GmosNodAndShuffle] =
    Cogen[(Offset, Offset, GmosEOffsetting, GmosShuffleOffset, GmosShuffleCycles)]
      .contramap(n => (n.posA, n.posB, n.eOffset, n.shuffle, n.cycles))

  implicit val arbGmosCustomRoiEntry: Arbitrary[GmosCustomRoiEntry] =
    Arbitrary(
      for {
        xMin <- Gen.posNum[Short]
        yMin <- Gen.posNum[Short]
        xRng <- Gen.posNum[Short]
        yRng <- Gen.posNum[Short]
      } yield GmosCustomRoiEntry.unsafeFromDescription(xMin, yMin, xRng, yRng)
    )

  implicit val cogGmosCustomRoiEntry: Cogen[GmosCustomRoiEntry] =
    Cogen[(Short, Short, Short, Short)].contramap(c => (c.xMin, c.yMin, c.xRange, c.yRange))

  implicit val arbGmosCommonStaticConfig: Arbitrary[GmosCommonStaticConfig] =
    Arbitrary(
      for {
        d <- arbitrary[GmosDetector]
        p <- arbitrary[MosPreImaging]
        n <- arbitrary[Option[GmosNodAndShuffle]]
        c <- Gen.choose(1, 5)
        r <- Gen.listOfN(c, arbitrary[GmosCustomRoiEntry])
      } yield GmosCommonStaticConfig(d, p, n, r.toSet)
    )

  implicit val cogGmosCommonStaticConfig: Cogen[GmosCommonStaticConfig] =
    Cogen[(GmosDetector, MosPreImaging, Option[GmosNodAndShuffle], List[GmosCustomRoiEntry])]
      .contramap(c => (c.detector, c.mosPreImaging, c.nodAndShuffle, c.customRois.toList))

  implicit val arbGmosNorthStatic: Arbitrary[StaticConfig.GmosN] =
    Arbitrary(
      for {
        c <- arbitrary[GmosCommonStaticConfig]
        s <- arbitrary[GmosNorthStageMode]
      } yield StaticConfig.GmosN(c, s)
    )

  implicit val cogGmosNorthStaticConfig: Cogen[StaticConfig.GmosN] =
    Cogen[(GmosCommonStaticConfig, GmosNorthStageMode)]
      .contramap(g => (g.common, g.stageMode))

  implicit val arbGmosSouthStatic: Arbitrary[StaticConfig.GmosS] =
    Arbitrary(
      for {
        c <- arbitrary[GmosCommonStaticConfig]
        s <- arbitrary[GmosSouthStageMode]
      } yield StaticConfig.GmosS(c, s)
    )

  implicit val cogGmosSouthStaticConfig: Cogen[StaticConfig.GmosS] =
    Cogen[(GmosCommonStaticConfig, GmosSouthStageMode)]
      .contramap(g => (g.common, g.stageMode))


  // Dynamic Config

}

object ArbGmos extends ArbGmos
