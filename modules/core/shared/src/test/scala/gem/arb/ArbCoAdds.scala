// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package arb

import gem.syntax.prism._

import org.scalacheck.{ Arbitrary, Cogen, Gen }

trait ArbCoAdds {

  implicit val arbCoAdds: Arbitrary[CoAdds] =
    Arbitrary(Gen.posNum[Short].map(CoAdds.fromShort.unsafeGet))

  implicit val cogCoAdds: Cogen[CoAdds] =
    Cogen[Short].contramap(_.toShort)

}

object ArbCoAdds extends ArbCoAdds
