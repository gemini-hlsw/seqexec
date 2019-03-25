// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.json.instances

import cats.tests.CatsSuite
import gem.Asterism
import gem.arb._
import io.circe.testing.CodecTests
import io.circe.testing.instances._

final class AsterismJsonSpec extends CatsSuite {
  import ArbAsterism._
  import asterism._

  checkAll("Asterism", CodecTests[Asterism].unserializableCodec)

}
