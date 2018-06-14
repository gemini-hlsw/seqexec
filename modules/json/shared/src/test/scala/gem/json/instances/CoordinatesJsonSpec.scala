// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.json.instances

import cats.tests.CatsSuite
import gem.math._
import gem.arb._
import io.circe.testing.CodecTests
import io.circe.testing.instances._

final class CoordinatesJsonSpec extends CatsSuite {
  import ArbCoordinates._
  import ArbDeclination._
  import ArbRightAscension._
  import coordinates._

  checkAll("RightAscension", CodecTests[RightAscension].unserializableCodec)
  checkAll("Declination", CodecTests[Declination].unserializableCodec)
  checkAll("Coordinates", CodecTests[Coordinates].unserializableCodec)

}
