// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao
package check

class SmartGcalCheck extends Check {
  import SmartGcalDao.Statements._
  "SmartGcalDao.Statements" should
            "selectF2byLamp"            in check(Flamingos2.selectByLamp(Dummy.f2SmartGcalKey)(Dummy.gcalLampType))
  it should "selectF2byBaseline"        in check(Flamingos2.selectByBaseline(Dummy.f2SmartGcalKey)(Dummy.gcalBaselineType))
  it should "createIndexF2"             in check(Flamingos2.createIndex)
  it should "dropIndexF2"               in check(Flamingos2.dropIndex)
  it should "selectGmosNorthByLamp"     in check(GmosNorth.selectByLamp(Dummy.gmosNorthSmartGcalSearchKey)(Dummy.gcalLampType))
  it should "selectGmosNorthByBaseline" in check(GmosNorth.selectByBaseline(Dummy.gmosNorthSmartGcalSearchKey)(Dummy.gcalBaselineType))
  it should "selectGmosSouthByLamp"     in check(GmosSouth.selectByLamp(Dummy.gmosSouthSmartGcalSearchKey)(Dummy.gcalLampType))
  it should "selectGmosSouthByBaseline" in check(GmosSouth.selectByBaseline(Dummy.gmosSouthSmartGcalSearchKey)(Dummy.gcalBaselineType))
  it should "bulkInsertGmosNorth"       in check(GmosNorth.bulkInsert)
  it should "bulkInsertGmosSouth"       in check(GmosSouth.bulkInsert)
  it should "createIndexGmosNorth"      in check(GmosNorth.createIndex)
  it should "dropIndexGmosNorth"        in check(GmosNorth.dropIndex)
  it should "createIndexGmosSouth"      in check(GmosSouth.createIndex)
  it should "dropIndexGmosSouth"        in check(GmosSouth.dropIndex)
  it should "selectGnirsByLamp"         in check(Gnirs.selectByLamp(Dummy.gnirsSmartGcalKey)(Dummy.gcalLampType))
  it should "selectGnirsbyBaseline"     in check(Gnirs.selectByBaseline(Dummy.gnirsSmartGcalKey)(Dummy.gcalBaselineType))
  it should "createIndexGnirs"          in check(Gnirs.createIndex)
  it should "dropIndexGnirs"            in check(Gnirs.dropIndex)
}
