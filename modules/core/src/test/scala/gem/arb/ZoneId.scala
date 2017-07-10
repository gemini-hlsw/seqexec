// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package arb

import java.time.ZoneId
import org.scalacheck._
import org.scalacheck.Gen._
import scala.collection.JavaConverters._

trait ArbZoneId {

  implicit val arbZoneId: Arbitrary[ZoneId] =
    Arbitrary {
      oneOf(ZoneId.getAvailableZoneIds.asScala.toSeq).map(ZoneId.of)
    }

}
object ArbZoneId extends ArbZoneId
