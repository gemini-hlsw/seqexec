// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package dao
package check

import gem.enum.Guider
import gem.enum.Instrument.GmosN

class GuideStarCheck extends Check {

  import GuideStarDao.Statements._

  "GuideStarDao" should
            "insert"      in check(insert(GuideGroup.Id(0), Target.Id(0), Guider.GmosSOI, Dummy.observationId, GmosN))
  it should "select"      in check(select(GuideStar.Id(0)))
  it should "selectGroup" in check(selectGroup(GuideGroup.Id(0)))
  it should "selectObs"   in check(selectObs(Dummy.observationId))
  it should "selectProg"  in check(selectProg(Dummy.programId))

}
