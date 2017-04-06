package gem.dao
package check

class DatasetCheck extends Check {
  import DatasetDao.Statements._
  "DatasetDao.Statements" should
            "selectAll" in check(selectAll(Dummy.observationId))
  it should "insert"    in check(insert(0, Dummy.dataset))
}
