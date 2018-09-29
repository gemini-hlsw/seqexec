// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.model.boopickle

import cats.Eq
import cats.tests.CatsSuite
import gem.{ProgramId, Observation}
import gem.arb.{ArbIndex, ArbTime, ArbProgramId, ArbObservation}
import gem.math.Index
import java.time.{LocalDate, Year}
import org.scalacheck.Arbitrary._

/**
  * Tests Serialization/Deserialization using BooPickle
  */
@SuppressWarnings(
  Array(
    "org.wartremover.warts.ImplicitParameter",
    "org.wartremover.warts.Throw",
    "org.wartremover.warts.OptionPartial",
    "org.wartremover.warts.NonUnitStatements",
    "org.wartremover.warts.Equals"
  ))
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
