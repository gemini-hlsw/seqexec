// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import edu.gemini.seqexec.server.tcs.{BinaryOnOff, BinaryYesNo}
import org.scalacheck.Arbitrary._
import org.scalacheck.Arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen
import shapeless.tag
import shapeless.tag.@@
import squants.Angle
import squants.space.Degrees

trait TcsArbitraries {
  implicit val tcsBeamArb: Arbitrary[TcsController.Beam] = Arbitrary(Gen.oneOf(TcsController.Beam.A, TcsController.Beam.B, TcsController.Beam.C))
  implicit val tcsBeamCogen: Cogen[TcsController.Beam] =
    Cogen[String].contramap(_.productPrefix)
  implicit val tcsNodChopArb: Arbitrary[TcsController.NodChop] = Arbitrary {
    for {
      n <- arbitrary[TcsController.Beam]
      c <- arbitrary[TcsController.Beam]
    } yield TcsController.NodChop(n, c)
  }
  implicit val tcsNodChopCogen: Cogen[TcsController.NodChop] =
    Cogen[(TcsController.Beam, TcsController.Beam)].contramap(x => (x.nod, x.chop))

  private val angleMMGen = Gen.posNum[Double].map(Degrees(_))
  implicit val offsetPArb: Arbitrary[Angle@@TcsController.OffsetP] = Arbitrary(angleMMGen.map(tag[TcsController.OffsetP].apply))
  implicit val offsetPCogen: Cogen[Angle@@TcsController.OffsetP] =
    Cogen[Double].contramap(_.value)
  implicit val offsetYArb: Arbitrary[Angle@@TcsController.OffsetQ] = Arbitrary(angleMMGen.map(tag[TcsController.OffsetQ].apply))
  implicit val offsetYCogen: Cogen[Angle@@TcsController.OffsetQ] =
    Cogen[Double].contramap(_.value)
  implicit val fpoArb: Arbitrary[TcsController.InstrumentOffset] = Arbitrary {
    for {
      p <- arbitrary[Angle@@TcsController.OffsetP]
      q <- arbitrary[Angle@@TcsController.OffsetQ]
    } yield TcsController.InstrumentOffset(p, q)
  }
  implicit val fpoCogen: Cogen[TcsController.InstrumentOffset] =
    Cogen[(Angle@@TcsController.OffsetP, Angle@@TcsController.OffsetQ)].contramap(x => (x.p, x.q))

  implicit val crFollowArb: Arbitrary[CRFollow] = Arbitrary {
    Gen.oneOf(CRFollow.On, CRFollow.Off)
  }
  implicit val crFollowCogen: Cogen[CRFollow] =
    Cogen[String].contramap(_.productPrefix)

  implicit val binaryYNArb: Arbitrary[BinaryYesNo] = Arbitrary(Gen.oneOf(BinaryYesNo.Yes, BinaryYesNo.No))
  implicit val binaryYNCommandCogen: Cogen[BinaryYesNo] =
    Cogen[String].contramap(_.name)
  implicit val binaryOOArb: Arbitrary[BinaryOnOff] = Arbitrary(Gen.oneOf(BinaryOnOff.Off, BinaryOnOff.On))
  implicit val binaryOOCommandCogen: Cogen[BinaryOnOff] =
    Cogen[String].contramap(_.name)
}
