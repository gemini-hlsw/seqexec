// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.json.instances

import gem.arb.ArbGcalConfig

import cats.tests.CatsSuite
import gem.config._
import io.circe.testing.CodecTests
import io.circe.testing.instances._

final class GcalConfigJsonSpec extends CatsSuite {
  import gcalconfig._

  import ArbGcalConfig._

  checkAll("GcalConfig", CodecTests[GcalConfig].unserializableCodec)

}
