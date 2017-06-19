package gem.dao
package check

class ObservationCheck extends Check {
  import ObservationDao.Statements._
  "ObservationDao.Statements" should
            "insert"         in check(insert(Dummy.observation, 0))
  it should "selectIds"      in check(selectIds(Dummy.programId))
  it should "selectStaticId" in check(selectStaticId(Dummy.observationId))
  it should "selectFlat"     in check(selectFlat(Dummy.observationId))
  it should "selectAllFlat"  in check(selectAllFlat(Dummy.programId))
}
