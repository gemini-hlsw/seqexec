// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package arb

import gem.math.{ Angle, Declination }
import org.scalacheck._
import org.scalacheck.Arbitrary._

trait ArbDeclination {
  import ArbAngle._

  implicit val arbDeclination: Arbitrary[Declination] =
    Arbitrary(arbitrary[Angle].map(Declination.fromAngleWithCarry(_)._1))

}
object ArbDeclination extends ArbDeclination
