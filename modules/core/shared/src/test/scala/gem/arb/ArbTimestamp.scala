// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package arb

import gem.util.Timestamp
import gsp.math.arb.ArbTime

import org.scalacheck._

import java.time._

// Arbitrary but reasonable Timestamp
trait ArbTimestamp {

  import ArbTime.cogInstant

  implicit val arbTimestamp: Arbitrary[Timestamp] =
    Arbitrary {
      for {
        m <- Gen.choose(0L, Duration.between(Instant.EPOCH, Timestamp.Max.toInstant).toMillis)
        u <- Gen.choose(0, 999L)
      } yield Timestamp.Epoch.plusMillis(m).flatMap(_.plusMicros(u)).getOrElse(Timestamp.Epoch)
    }

  implicit val cogTimestamp: Cogen[Timestamp] =
    Cogen[Instant].contramap(_.toInstant)

}

object ArbTimestamp extends ArbTimestamp
