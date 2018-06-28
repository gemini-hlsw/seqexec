// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.json.instances

import cats.tests.CatsSuite
import gem.CoAdds
import gem.config.Arbitraries
import io.circe.testing.CodecTests
import io.circe.testing.instances._

final class CoAddsJsonSpec extends CatsSuite with Arbitraries {
  import coadds._

  checkAll("CoAdds", CodecTests[CoAdds].unserializableCodec)

}
