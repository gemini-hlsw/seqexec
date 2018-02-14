// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import cats.kernel.laws.discipline._
import cats.tests.CatsSuite
import gem.arb._
import gem.enum.Instrument
import org.scalacheck.Arbitrary

@SuppressWarnings(Array("org.wartremover.warts.ToString", "org.wartremover.warts.Equals"))
final class TargetEnvironmentSpec extends CatsSuite {
  import ArbTargetEnvironment._

  // laws
  Instrument.all.foreach { i =>
    implicit val x: Arbitrary[TargetEnvironment.Aux[i.type]] = Arbitrary(genTargetEnvironment(i: Instrument.Aux[i.type]))
    checkAll(s"TargetEnvironment[$i.type]", EqTests[TargetEnvironment.Aux[i.type]].eqv)
  }

}
