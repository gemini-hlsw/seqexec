// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gcal

import org.scalacheck.Arbitrary._
import org.scalacheck.Arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen
import seqexec.server.gcal.GcalController._

trait GcalArbitraries {
  implicit val gcalLampArb: Arbitrary[GcalController.LampState] =
    Arbitrary(Gen.oneOf(GcalController.LampState.On, GcalController.LampState.Off))

  implicit val gcalLampCogen: Cogen[GcalController.LampState] =
    Cogen[String].contramap(_.productPrefix)

  implicit val gcalArLampArb: Arbitrary[GcalController.ArLampState] =
    Arbitrary(arbitrary[GcalController.LampState].map(ArLampState.apply))

  implicit val gcalArLampCogen: Cogen[GcalController.ArLampState] =
    Cogen[GcalController.LampState].contramap(_.self)

  implicit val gcalCuArLampArb: Arbitrary[GcalController.CuArLampState] =
    Arbitrary(arbitrary[GcalController.LampState].map(CuArLampState.apply))

  implicit val gcalCuArLampCogen: Cogen[GcalController.CuArLampState] =
    Cogen[GcalController.LampState].contramap(_.self)

  implicit val gcalQh5WLampArb: Arbitrary[GcalController.QH5WLampState] =
    Arbitrary(arbitrary[GcalController.LampState].map(QH5WLampState.apply))

  implicit val gcalQh5WLampCogen: Cogen[GcalController.QH5WLampState] =
    Cogen[GcalController.LampState].contramap(_.self)

  implicit val gcalQh1000WLampArb: Arbitrary[GcalController.QH100WLampState] =
    Arbitrary(arbitrary[GcalController.LampState].map(QH100WLampState.apply))

  implicit val gcalQh100WLampCogen: Cogen[GcalController.QH100WLampState] =
    Cogen[GcalController.LampState].contramap(_.self)

  implicit val gcalThArLampArb: Arbitrary[GcalController.ThArLampState] =
    Arbitrary(arbitrary[GcalController.LampState].map(ThArLampState.apply))

  implicit val gcalThArLampCogen: Cogen[GcalController.ThArLampState] =
    Cogen[GcalController.LampState].contramap(_.self)

  implicit val gcalXeLampArb: Arbitrary[GcalController.XeLampState] =
    Arbitrary(arbitrary[GcalController.LampState].map(XeLampState.apply))

  implicit val gcalXeLampCogen: Cogen[GcalController.XeLampState] =
    Cogen[GcalController.LampState].contramap(_.self)

  implicit val gcalIrLampArb: Arbitrary[GcalController.IrLampState] =
    Arbitrary(arbitrary[GcalController.LampState].map(IrLampState.apply))

  implicit val gcalIrLampCogen: Cogen[GcalController.IrLampState] =
    Cogen[GcalController.LampState].contramap(_.self)
}
