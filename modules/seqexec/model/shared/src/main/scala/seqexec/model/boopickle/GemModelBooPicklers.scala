// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model.boopickle

import boopickle.Default.Pickler
import boopickle.Default.transformPickler
import boopickle.Default.generatePickler
import boopickle.Default.stringPickler
import boopickle.Default.shortPickler
import boopickle.Default.intPickler
import boopickle.Default.longPickler
import boopickle.DefaultBasic.iterablePickler
import gem.{ProgramId, Observation}
import gsp.math.Index
import java.time.{LocalDate, Year}

/**
  * Contains boopickle implicit picklers of gem model objects
  * Eventually these will be shared across gem clients
  */
trait GemModelBooPicklers extends BooPicklerSyntax {
  implicit val yearPickler:          Pickler[Year]           = transformPickler(Year.of)(_.getValue)
  implicit val localDatePickler:     Pickler[LocalDate]      = transformPickler(LocalDate.ofEpochDay)(_.toEpochDay)
  implicit val indexPickler:         Pickler[Index]          = Index.fromShort.toPickler
  implicit val programIdPickler:     Pickler[ProgramId]      = ProgramId.fromString.toPickler
  implicit val observationIdPickler: Pickler[Observation.Id] = generatePickler[Observation.Id]
  implicit val lObservationIdPickler: Pickler[List[Observation.Id]] = iterablePickler[Observation.Id, List]
}
