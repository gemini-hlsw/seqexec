// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.json.instances

import cats.tests.CatsSuite
import gsp.math.arb.ArbTime
import gem.instances.time._
import io.circe.testing.CodecTests
import io.circe.testing.instances._
import java.time.{ Duration, Instant }

final class TimeJsonSpec extends CatsSuite {
  import ArbTime._
  import time._

  checkAll("Duration", CodecTests[Duration].unserializableCodec)
  checkAll("Instant", CodecTests[Instant].unserializableCodec)

}
