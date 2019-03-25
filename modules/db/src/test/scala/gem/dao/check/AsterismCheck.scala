// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao
package check

import gem.Target
import gem.enum.Instrument

class AsterismCheck extends Check {

  import AsterismDao.Statements._

  "AsterismDao.Statements" should
            "SingleTarget.insert"    in check(insertSingleTarget(Dummy.observationId, Target.Id(0), Instrument.GmosS))
  it should "GhostDualTarget.insert" in check(insertGhostDualTarget(Dummy.observationId, Target.Id(0), Target.Id(0)))
  it should "select"                 in check(select(Dummy.observationId))
  it should "selectAll"              in check(selectAll(Dummy.programId))
}
