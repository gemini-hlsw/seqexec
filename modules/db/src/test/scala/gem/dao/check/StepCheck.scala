package gem.dao
package check

class StepCheck extends Check {
  import StepDao.Statements._
  "StepDao.Statements" should
            "deleteAtLocation"     in check(deleteAtLocation(Dummy.observationId, Dummy.locationMiddle))
  it should "delete"               in check(delete(Dummy.observationId))
  it should "allF2Only"            in check(allF2Only(Dummy.observationId))
  it should "selectAllEmpty"       in check(selectAllEmpty(Dummy.observationId))
  it should "selectOneEmpty"       in check(selectOneEmpty(Dummy.observationId, Dummy.locationMiddle))
  it should "insertF2Config"       in check(insertF2Config(0, Dummy.f2Config))
  it should "insertScienceSlice"   in check(insertScienceSlice(0, Dummy.telescopeConfig))
  it should "insertSmartGcalSlice" in check(insertSmartGcalSlice(0, Dummy.smartGcalType))
  it should "insertGcalStep"       in check(insertGcalStep(0, 0))
  it should "insertDarkSlice"      in check(insertDarkSlice(0))
  it should "insertBiasSlice"      in check(insertBiasSlice(0))
  it should "insertBaseSlice"      in check(insertBaseSlice(Dummy.observationId, Dummy.locationMiddle, Dummy.instrumentConfig, Dummy.stepType))
}
