package gem.dao
package check

class SmartGcalCheck extends Check {
  import SmartGcalDao.Statements._
  "SmartGcalDao.Statements" should
            "selectF2byLamp"     in check(selectF2byLamp(Dummy.f2SmartGcalKey)(Dummy.gcalLampType))
  it should "selectF2byBaseline" in check(selectF2byBaseline(Dummy.f2SmartGcalKey)(Dummy.gcalBaselineType))
  it should "insertSmartF2"      in check(insertSmartF2(Dummy.gcalLampType, Dummy.gcalBaselineType, 0, Dummy.f2SmartGcalKey))
}
