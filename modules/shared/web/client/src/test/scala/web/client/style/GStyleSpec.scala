// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package web.client.style

import cats.kernel.laws.discipline.EqTests
import cats.kernel.laws.discipline.MonoidTests
import cats.tests.CatsSuite
import org.scalacheck.{Arbitrary, Cogen, Gen}
import org.scalacheck.Arbitrary._

/**
  * Tests the Monocle Lenses for Seqexec Events
  */
@SuppressWarnings(Array("org.wartremover.warts.ToString"))
final class GStyleSpec extends CatsSuite {
  implicit val arbGStyle: Arbitrary[GStyle] = Arbitrary {
    for {
        cs <- Gen.listOf(Gen.alphaLowerStr)
    } yield GStyle(cs)
  }

  implicit val gStyleCogen: Cogen[GStyle] =
    Cogen[String].contramap(_.htmlClass)

  checkAll("Eq[GStyle]", EqTests[GStyle].eqv)
  checkAll("Monoid[GStyle]", MonoidTests[GStyle].monoid)
}
