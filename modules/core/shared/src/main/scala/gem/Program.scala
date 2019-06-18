// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import cats.Eq
import cats.implicits._
import gsp.math.Index
import scala.collection.immutable.SortedMap

/**
 * A science program, the root data type in the science model.
 * @group Program Model
 */
final case class Program(id: Program.Id, title: String, observations: SortedMap[Index, Observation])

object Program {
  type Id                 = ProgramId
  val  Id: ProgramId.type = ProgramId

  implicit val EqProgram: Eq[Program] =
    Eq.by(p => (p.id, p.title, p.observations))

}