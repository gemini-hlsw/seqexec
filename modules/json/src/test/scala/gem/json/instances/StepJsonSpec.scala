// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.json.instances

import cats.tests.CatsSuite
import gem.Step
import gem.arb._
import io.circe.testing.CodecTests
import io.circe.testing.instances._

final class StepJsonSpec extends CatsSuite {
  import ArbStep._
  import step._

  checkAll("Step", CodecTests[Step].unserializableCodec)

}
