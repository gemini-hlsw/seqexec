// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao
package check

class ObservationCheck extends Check {
  import ObservationDao.Statements._
  "ObservationDao.Statements" should
            "insert"         in check(insert(Dummy.observationId, Dummy.observation))
  it should "selectIds"      in check(selectIds(Dummy.programId))
  it should "selectFlat"     in check(selectFlat(Dummy.observationId))
  it should "selectAllFlat"  in check(selectAllFlat(Dummy.programId))
}
