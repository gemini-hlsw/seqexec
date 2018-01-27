// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao
package check

import gem.enum.Instrument.GmosN
import gem.config.StaticConfig
import gem.config.GmosConfig.GmosNodAndShuffle

class StaticCheck extends Check {
  import StaticConfigDao.Statements._

  "StaticDao.Statements" should
            "F2.insert"                 in check(F2.insert(Dummy.observationId, StaticConfig.F2.Default))
  it should "F2.select"                 in check(F2.select(Dummy.observationId))

  it should "Gmos.insertNorth"          in check(Gmos.insertNorth(Dummy.observationId, StaticConfig.GmosNorth.Default))
  it should "Gmos.insertSouth"          in check(Gmos.insertSouth(Dummy.observationId, StaticConfig.GmosSouth.Default))
  it should "Gmos.insertCustomRoiEntry" in check(Gmos.insertCustomRoiEntry(Dummy.observationId, GmosN, Dummy.gmosCustomRoiEntry))
  it should "Gmos.insertNodAndShuffle"  in check(Gmos.insertNodAndShuffle(Dummy.observationId, GmosN, GmosNodAndShuffle.Default))
  it should "Gmos.selectNorth"          in check(Gmos.selectNorth(Dummy.observationId))
  it should "Gmos.selectSouth"          in check(Gmos.selectSouth(Dummy.observationId))
  it should "Gmos.selectCustomRoiEntry" in check(Gmos.selectCustomRoiEntry(Dummy.observationId, GmosN))
  it should "Gmos.selectNodAndShuffle"  in check(Gmos.selectNodAndShuffle(Dummy.observationId, GmosN))
}
