// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package arb

import gem.math.Index
import gem.syntax.prism._
import org.scalacheck._
import org.scalacheck.Gen._

trait ArbIndex {

  val genIndex: Gen[Index] =
    choose[Short](1, Short.MaxValue).map(Index.fromShort.unsafeGet)

  implicit val arbIndex: Arbitrary[Index] =
    Arbitrary(genIndex)

  implicit val cogIndex: Cogen[Index] =
    Cogen[Short].contramap(_.toShort)

}

object ArbIndex extends ArbIndex