// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao
package check

import gem.Target
import gem.enum.Instrument

class AsterismCheck extends Check {

  import AsterismDao.Statements._

  "AsterismDao.Statements" should
            "SingleTarget.insert"    in check(SingleTarget.insert(Dummy.observationId, Target.Id(0), Instrument.GmosS))
  it should "SingleTarget.select"    in check(SingleTarget.select(Dummy.observationId))
  it should "SingleTarget.selectAll" in check(SingleTarget.selectAll(Dummy.programId))

  it should "GhostDualTarget.insert"    in check(GhostDualTarget.insert(Dummy.observationId, Target.Id(0), Target.Id(0)))
  it should "GhostDualTarget.select"    in check(GhostDualTarget.select(Dummy.observationId))
  it should "GhostDualTarget.selectAll" in check(GhostDualTarget.selectAll(Dummy.programId))
}
