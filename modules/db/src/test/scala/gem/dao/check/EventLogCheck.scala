// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao
package check

class EventLogCheck extends Check {
  import EventLogDao.Statements._
  "EventLogDao.Statements" should
            "insertEvent" in check(insertEvent(Dummy.eventType, Dummy.observationId, None))
  it should "selectAll"   in check(selectAll(Dummy.instant, Dummy.instant))
}
