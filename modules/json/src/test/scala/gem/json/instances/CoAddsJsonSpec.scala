// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.json.instances

import cats.tests.CatsSuite
import gem.CoAdds
import gem.arb.ArbCoAdds
import io.circe.testing.CodecTests
import io.circe.testing.instances._

final class CoAddsJsonSpec extends CatsSuite {
  import coadds._

  import ArbCoAdds._

  checkAll("CoAdds", CodecTests[CoAdds].unserializableCodec)

}
