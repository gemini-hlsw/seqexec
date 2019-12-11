// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model.boopickle

import cats.Eq
import cats.tests.CatsSuite
import gem.{ProgramId, Observation}
import gem.arb.{ ArbProgramId, ArbObservation }
import gsp.math.Index
import gsp.math.arb.{ ArbIndex, ArbTime }
import java.time.{LocalDate, Year}
import org.scalacheck.Arbitrary._

/**
  * Tests Serialization/Deserialization using BooPickle
  */
final class GemModelBooPicklersSpec extends CatsSuite with GemModelBooPicklers with ArbTime with ArbIndex with ArbProgramId {
  import ArbObservation.arbObservationId

  implicit val yearEq:      Eq[Year]      = Eq.by(_.getValue)
  implicit val localDateEq: Eq[LocalDate] = Eq.by(_.toEpochDay)

  checkAll("Pickler[Year]",                  PicklerTests[Year].pickler)
  checkAll("Pickler[LocalDate]",             PicklerTests[LocalDate].pickler)
  checkAll("Pickler[Index]",                 PicklerTests[Index].pickler)
  checkAll("Pickler[ProgramId]",             PicklerTests[ProgramId].pickler)
  checkAll("Pickler[Observation.Id]",        PicklerTests[Observation.Id].pickler)
}
