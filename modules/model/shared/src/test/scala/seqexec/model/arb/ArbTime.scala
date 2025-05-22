// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model.arb

import lucuma.core.arb.ArbTime.arbSDuration
import org.scalacheck.Arbitrary
import org.scalacheck.Cogen
import squants.time._

trait ArbTime {

  implicit val arbTime: Arbitrary[Time] =
    Arbitrary {
      arbSDuration.arbitrary.map(Time.apply)
    }

  implicit val timeCogen: Cogen[Time] =
    Cogen[Long]
      .contramap(_.millis)

}

object ArbTime extends ArbTime
