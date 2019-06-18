// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model.arb

import org.scalacheck.Arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen
import seqexec.model.enum.ComaOption

trait ArbComaOption {

  implicit val arbComaOption: Arbitrary[ComaOption] =
    Arbitrary {
      Gen.oneOf[ComaOption](ComaOption.ComaOn, ComaOption.ComaOff)
    }

  implicit val commaOptionCogen: Cogen[ComaOption] =
    Cogen[String].contramap(_.productPrefix)

}

object ArbComaOption extends ArbComaOption
