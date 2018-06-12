// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import gem.math.Index
import scala.collection.immutable.TreeMap

/**
 * A science program, the root data type in the science model.
 * @group Program Model
 */
final case class Program(id: Program.Id, title: String, observations: TreeMap[Index, Observation])

object Program {
  type Id                 = ProgramId
  val  Id: ProgramId.type = ProgramId
}