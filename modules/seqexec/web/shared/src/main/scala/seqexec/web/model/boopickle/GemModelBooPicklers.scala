// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model.boopickle

import boopickle.Default._
import gem.{ProgramId, Observation}
import gem.math.Index
import java.time.{LocalDate, Year}

/**
  * Contains boopickle implicit picklers of gem model objects
  * Eventually these will be shared across gem clients
  */
@SuppressWarnings(Array("org.wartremover.warts.Equals", "org.wartremover.warts.PublicInference", "org.wartremover.warts.ImplicitParameter", "org.wartremover.warts.NonUnitStatements", "org.wartremover.warts.Throw", "org.wartremover.warts.OptionPartial"))
trait GemModelBooPicklers extends BooPicklerSyntax {
  implicit val yearPickler:          Pickler[Year]           = transformPickler(Year.of)(_.getValue)
  implicit val localDatePickler:     Pickler[LocalDate]      = transformPickler(LocalDate.ofEpochDay)(_.toEpochDay)
  implicit val indexPickler:         Pickler[Index]          = Index.fromShort.toPickler
  implicit val programIdPickler:     Pickler[ProgramId]      = ProgramId.fromString.toPickler
  implicit val observationIdPickler: Pickler[Observation.Id] = generatePickler[Observation.Id]
}
