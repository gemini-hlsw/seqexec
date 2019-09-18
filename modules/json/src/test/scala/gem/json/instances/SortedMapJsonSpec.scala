// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.json.instances

import cats.implicits._
import cats.laws.discipline.arbitrary._
import cats.tests.CatsSuite
import io.circe.testing.CodecTests
import io.circe.testing.instances._
import scala.collection.immutable.SortedMap

final class SortedMapJsonSpec extends CatsSuite {
  import sortedmap._

  checkAll("SortedMap", CodecTests[SortedMap[Int, String]].unserializableCodec)

}
