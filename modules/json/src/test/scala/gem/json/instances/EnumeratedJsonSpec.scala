// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.json.instances

import cats.tests.CatsSuite
import gem.enum._
import gem.arb._
import io.circe.testing.CodecTests
import io.circe.testing.instances._

final class EnumeratedJsonSpec extends CatsSuite {
  import ArbEnumerated._
  import enumerated._

  // These are generated so we'll just do a spot check. They should all work the same way.
  checkAll("AsterismType",     CodecTests[AsterismType].unserializableCodec)
  checkAll("DailyProgramType", CodecTests[DailyProgramType].unserializableCodec)
  checkAll("EphemerisKeyType", CodecTests[EphemerisKeyType].unserializableCodec)
  checkAll("EventType",        CodecTests[EventType].unserializableCodec)
  checkAll("F2Disperser",      CodecTests[F2Disperser].unserializableCodec)
  checkAll("F2Filter",         CodecTests[F2Filter].unserializableCodec)
  checkAll("F2Fpu",            CodecTests[F2Fpu].unserializableCodec)
  checkAll("F2LyotWheel",      CodecTests[F2LyotWheel].unserializableCodec)
  checkAll("F2ReadMode",       CodecTests[F2ReadMode].unserializableCodec)
  checkAll("F2WindowCover",    CodecTests[F2WindowCover].unserializableCodec)

}
