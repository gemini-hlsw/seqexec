package gem.dao
package check

class EventLogCheck extends Check {
  import EventLogDao.Statements._
  "EventLogDao.Statements" should
            "insertEvent" in check(insertEvent(Dummy.eventType, Dummy.observationId, None))
  it should "selectAll"   in check(selectAll(Dummy.instant, Dummy.instant))
}
