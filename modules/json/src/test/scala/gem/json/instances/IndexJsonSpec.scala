// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.json.instances

import cats.tests.CatsSuite
import gsp.math.Index
import gsp.math.arb.ArbIndex
import io.circe.testing.CodecTests
import io.circe.testing.instances._

final class IndexJsonSpec extends CatsSuite {
  import ArbIndex._
  import index._

  checkAll("Index", CodecTests[Index].unserializableCodec)

}
