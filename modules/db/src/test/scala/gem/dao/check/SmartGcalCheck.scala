// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao
package check

class SmartGcalCheck extends Check {
  import SmartGcalDao.Statements._
  "SmartGcalDao.Statements" should
            "selectF2byLamp"            in check(selectF2ByLamp(Dummy.f2SmartGcalKey)(Dummy.gcalLampType))
  it should "selectF2byBaseline"        in check(selectF2ByBaseline(Dummy.f2SmartGcalKey)(Dummy.gcalBaselineType))
  it should "createIndexF2"             in check(createIndexF2)
  it should "dropIndexF2"               in check(dropIndexF2)
  it should "selectGmosNorthByLamp"     in check(selectGmosNorthByLamp(Dummy.gmosNorthSmartGcalSearchKey)(Dummy.gcalLampType))
  it should "selectGmosNorthByBaseline" in check(selectGmosNorthByBaseline(Dummy.gmosNorthSmartGcalSearchKey)(Dummy.gcalBaselineType))
  it should "selectGmosSouthByLamp"     in check(selectGmosSouthByLamp(Dummy.gmosSouthSmartGcalSearchKey)(Dummy.gcalLampType))
  it should "selectGmosSouthByBaseline" in check(selectGmosSouthByBaseline(Dummy.gmosSouthSmartGcalSearchKey)(Dummy.gcalBaselineType))
  it should "bulkInsertGmosNorth"       in check(bulkInsertGmosNorth)
  it should "bulkInsertGmosSouth"       in check(bulkInsertGmosSouth)
  it should "createIndexGmosNorth"      in check(createIndexGmosNorth)
  it should "dropIndexGmosNorth"        in check(dropIndexGmosNorth)
  it should "createIndexGmosSouth"      in check(createIndexGmosSouth)
  it should "dropIndexGmosSouth"        in check(dropIndexGmosSouth)
}
