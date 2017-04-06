package gem.dao
package check

class GcalCheck extends Check {
  import GcalDao.Statements._
  "GcalDao.Statements" should
            "insert" in check(insert(Dummy.gcalConfig, None))
  it should "select" in check(select(0))
}
