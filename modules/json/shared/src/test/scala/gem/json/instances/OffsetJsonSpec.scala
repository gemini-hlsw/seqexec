// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.json.instances

import cats.tests.CatsSuite
import gem.math.Offset
import gem.arb._
import io.circe.testing.CodecTests
import io.circe.testing.instances._

final class OffsetJsonSpec extends CatsSuite {
  import ArbOffset._
  import offset._

  checkAll("Offset.P", CodecTests[Offset.P].unserializableCodec)
  checkAll("Offset.Q", CodecTests[Offset.Q].unserializableCodec)
  checkAll("Offset", CodecTests[Offset].unserializableCodec)

}
