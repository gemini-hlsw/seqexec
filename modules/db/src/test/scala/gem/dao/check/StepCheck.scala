// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao
package check

class StepCheck extends Check {
  import StepDao.Statements._
  "StepDao.Statements" should
            "deleteAtLocation"      in check(deleteAtLocation(Dummy.observationId, Dummy.locationMiddle))
  it should "delete"                in check(delete(Dummy.observationId))
  it should "selectAllEmpty"        in check(selectAllEmpty(Dummy.observationId))
  it should "selectOneEmpty"        in check(selectOneEmpty(Dummy.observationId, Dummy.locationMiddle))
  it should "insertScienceSlice"    in check(insertScienceSlice(0, Dummy.telescopeConfig))
  it should "insertSmartGcalSlice"  in check(insertSmartGcalSlice(0, Dummy.smartGcalType))
  it should "insertDarkSlice"       in check(insertDarkSlice(0))
  it should "insertBiasSlice"       in check(insertBiasSlice(0))
  it should "insertBaseSlice"       in check(insertBaseSlice(Dummy.observationId, Dummy.locationMiddle, Dummy.instrumentConfig, Dummy.stepType))

  it should "F2.insert"             in check(F2.insert(0, Dummy.f2Config))
  it should "F2.selectAll"          in check(F2.selectAll(Dummy.observationId))
  it should "F2.selectOne"          in check(F2.selectOne(Dummy.observationId, Dummy.locationMiddle))

  it should "Gmos.insertNorth"      in check(Gmos.insertNorth(0, gem.config.DynamicConfig.GmosNorth.Default))
  it should "Gmos.insertSouth"      in check(Gmos.insertSouth(0, gem.config.DynamicConfig.GmosSouth.Default))
  it should "Gmos.selectAllNorth"   in check(Gmos.selectAllNorth(Dummy.observationId))
  it should "Gmos.selectOneNorth"   in check(Gmos.selectOneNorth(Dummy.observationId, Dummy.locationMiddle))
  it should "Gmos.selectAllSouth"   in check(Gmos.selectAllSouth(Dummy.observationId))
  it should "Gmos.selectOneSouth"   in check(Gmos.selectOneSouth(Dummy.observationId, Dummy.locationMiddle))
}
